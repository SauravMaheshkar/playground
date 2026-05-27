use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use async_openai::types::chat::{ChatCompletionMessageToolCall, FunctionCall};
use serde_json::json;

static DIR_COUNTER: AtomicU64 = AtomicU64::new(0);

/// A temporary directory that cleans itself up on drop.
struct TempDir {
    path: PathBuf,
}

impl TempDir {
    fn new() -> Self {
        let id = DIR_COUNTER.fetch_add(1, Ordering::SeqCst);
        let path = std::env::temp_dir().join(format!("coding-agent-test-{id}"));
        let _ = fs::remove_dir_all(&path);
        fs::create_dir_all(&path).unwrap();
        TempDir { path }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

// -----------------------------------------------------------------------
// Unit tests — tool handler functions
// -----------------------------------------------------------------------

#[tokio::test]
async fn test_read_existing_file() {
    let tmp = TempDir::new();
    let path = tmp.path().join("read_test.txt");
    fs::write(&path, "hello from read test").unwrap();

    let result = coding_agent::handle_read(path.to_str().unwrap())
        .await
        .unwrap();
    assert_eq!(result, "hello from read test");
}

#[tokio::test]
async fn test_read_nonexistent_file() {
    let result = coding_agent::handle_read("/tmp/__coding_agent_nope__").await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_write_creates_file() {
    let tmp = TempDir::new();
    let path = tmp.path().join("write_test.txt");

    coding_agent::handle_write(path.to_str().unwrap(), "written content")
        .await
        .unwrap();

    let content = fs::read_to_string(&path).unwrap();
    assert_eq!(content, "written content");
}

#[tokio::test]
async fn test_write_overwrites_file() {
    let tmp = TempDir::new();
    let path = tmp.path().join("overwrite_test.txt");
    fs::write(&path, "old content").unwrap();

    coding_agent::handle_write(path.to_str().unwrap(), "new content")
        .await
        .unwrap();

    let content = fs::read_to_string(&path).unwrap();
    assert_eq!(content, "new content");
}

#[tokio::test]
async fn test_bash_echo() {
    let result = coding_agent::handle_bash("echo 'hello world'")
        .await
        .unwrap();
    assert_eq!(result, "hello world");
}

#[tokio::test]
async fn test_bash_multiple_commands() {
    let result = coding_agent::handle_bash("echo 'line1' && echo 'line2'")
        .await
        .unwrap();
    assert_eq!(result, "line1\nline2");
}

#[tokio::test]
async fn test_bash_stderr_fallback() {
    let result = coding_agent::handle_bash("echo 'error msg' >&2")
        .await
        .unwrap();
    assert_eq!(result, "error msg");
}

#[tokio::test]
async fn test_bash_exit_code_nonzero() {
    let result = coding_agent::handle_bash("echo 'partial output' && exit 1")
        .await
        .unwrap();
    assert_eq!(result, "partial output");
}

#[tokio::test]
async fn test_bash_no_output() {
    let result = coding_agent::handle_bash("true").await.unwrap();
    assert_eq!(result, "");
}

#[tokio::test]
async fn test_process_read_tool_call() {
    let tmp = TempDir::new();
    let path = tmp.path().join("dispatch_read.txt");
    fs::write(&path, "dispatch read content").unwrap();

    let args = json!({ "file_path": path.to_str().unwrap() });
    let tool_call = ChatCompletionMessageToolCall {
        id: "call_1".into(),
        function: FunctionCall {
            name: "read".into(),
            arguments: args.to_string(),
        },
    };

    let mut messages = vec![];
    coding_agent::process_tool_call(&tool_call, &mut messages)
        .await
        .unwrap();

    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0]["role"], "tool");
    assert_eq!(messages[0]["content"], "dispatch read content");
    assert_eq!(messages[0]["tool_call_id"], "call_1");
}

#[tokio::test]
async fn test_process_write_tool_call() {
    let tmp = TempDir::new();
    let path = tmp.path().join("dispatch_write.txt");

    let args = json!({ "file_path": path.to_str().unwrap(), "content": "dispatch write content" });
    let tool_call = ChatCompletionMessageToolCall {
        id: "call_2".into(),
        function: FunctionCall {
            name: "write".into(),
            arguments: args.to_string(),
        },
    };

    let mut messages = vec![];
    coding_agent::process_tool_call(&tool_call, &mut messages)
        .await
        .unwrap();

    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0]["role"], "tool");
    assert_eq!(messages[0]["content"], "File written successfully");
    assert_eq!(messages[0]["tool_call_id"], "call_2");

    let content = fs::read_to_string(&path).unwrap();
    assert_eq!(content, "dispatch write content");
}

#[tokio::test]
async fn test_process_bash_tool_call() {
    let args = json!({ "command": "echo 'dispatch bash'" });
    let tool_call = ChatCompletionMessageToolCall {
        id: "call_3".into(),
        function: FunctionCall {
            name: "bash".into(),
            arguments: args.to_string(),
        },
    };

    let mut messages = vec![];
    coding_agent::process_tool_call(&tool_call, &mut messages)
        .await
        .unwrap();

    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0]["role"], "tool");
    assert_eq!(messages[0]["content"], "dispatch bash");
    assert_eq!(messages[0]["tool_call_id"], "call_3");
}

#[tokio::test]
async fn test_process_unknown_tool_call() {
    let args = json!({});
    let tool_call = ChatCompletionMessageToolCall {
        id: "call_unknown".into(),
        function: FunctionCall {
            name: "nonexistent-tool".into(),
            arguments: args.to_string(),
        },
    };

    let mut messages = vec![];
    coding_agent::process_tool_call(&tool_call, &mut messages)
        .await
        .unwrap();

    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0]["role"], "tool");
    assert!(
        messages[0]["content"]
            .as_str()
            .unwrap()
            .contains("Unknown tool")
    );
}

#[tokio::test]
async fn test_multiple_tool_calls_in_sequence() {
    let tmp = TempDir::new();
    let path_a = tmp.path().join("multi_a.txt");
    let path_b = tmp.path().join("multi_b.txt");
    fs::write(&path_a, "content a").unwrap();

    let calls = vec![
        ChatCompletionMessageToolCall {
            id: "multi_1".into(),
            function: FunctionCall {
                name: "read".into(),
                arguments: json!({ "file_path": path_a.to_str().unwrap() }).to_string(),
            },
        },
        ChatCompletionMessageToolCall {
            id: "multi_2".into(),
            function: FunctionCall {
                name: "bash".into(),
                arguments: json!({ "command": "echo 'bash result'" }).to_string(),
            },
        },
        ChatCompletionMessageToolCall {
            id: "multi_3".into(),
            function: FunctionCall {
                name: "write".into(),
                arguments: json!({
                    "file_path": path_b.to_str().unwrap(),
                    "content": "written by multi"
                })
                .to_string(),
            },
        },
    ];

    let mut messages = vec![];
    for tc in &calls {
        coding_agent::process_tool_call(tc, &mut messages)
            .await
            .unwrap();
    }

    assert_eq!(messages.len(), 3);
    assert_eq!(messages[0]["tool_call_id"], "multi_1");
    assert_eq!(messages[0]["content"], "content a");
    assert_eq!(messages[1]["tool_call_id"], "multi_2");
    assert_eq!(messages[1]["content"], "bash result");
    assert_eq!(messages[2]["tool_call_id"], "multi_3");
    assert_eq!(messages[2]["content"], "File written successfully");

    let written = fs::read_to_string(&path_b).unwrap();
    assert_eq!(written, "written by multi");
}

// -----------------------------------------------------------------------
// End-to-end tests — real model calls via OpenRouter
// -----------------------------------------------------------------------
//
// These tests call the LLM API using the configured model. They require a
// `.env` file with `OPENROUTER_API_KEY` set.
//
// If the key is missing the tests panic with a helpful message.

fn require_api_key() {
    dotenv::dotenv().ok();
    let key = std::env::var("OPENROUTER_API_KEY");
    match &key {
        Ok(k) => eprintln!(
            "[debug] OPENROUTER_API_KEY loaded ({}..{})",
            &k[..10],
            &k[k.len() - 4..]
        ),
        Err(_) => panic!(
            "OPENROUTER_API_KEY is not set. \
             Create a .env file (see .env.example) with your OpenRouter API key."
        ),
    }
    eprintln!(
        "[debug] OPENROUTER_MODEL={:?}",
        std::env::var("OPENROUTER_MODEL").ok()
    );
}

#[tokio::test]
async fn e2e_read_tool() {
    require_api_key();
    let tmp = TempDir::new();
    let file_path = tmp.path().join("e2e_read.txt");
    let content = "Hello from the e2e read test!";
    fs::write(&file_path, content).unwrap();

    let client = coding_agent::create_client();
    let prompt = format!(
        "Read the file at {} and tell me exactly what it says. \
         Use the read tool — do not guess the contents.",
        file_path.display()
    );
    let response = coding_agent::run_agent_loop(&client, &prompt)
        .await
        .expect("agent loop failed");

    assert!(
        response.contains("Hello from the e2e read test!"),
        "Expected response to contain file content.\nResponse: {response}"
    );
}

#[tokio::test]
async fn e2e_write_tool() {
    require_api_key();
    let tmp = TempDir::new();
    let file_path = tmp.path().join("e2e_write.txt");

    let client = coding_agent::create_client();
    let prompt = format!(
        "Write the text 'written by coding agent' to the file at {}. \
         Use the write tool.",
        file_path.display()
    );
    coding_agent::run_agent_loop(&client, &prompt)
        .await
        .expect("agent loop failed");

    let written =
        fs::read_to_string(&file_path).expect("file should have been created by the write tool");
    assert!(
        written.contains("written by coding agent"),
        "Expected file to contain 'written by coding agent'.\nActual content: {written}"
    );
}

#[tokio::test]
async fn e2e_bash_tool() {
    require_api_key();
    let client = coding_agent::create_client();
    let prompt = "Run the bash command 'echo HELLO_FROM_THE_AGENT' and tell me what it outputs. \
         Use the bash tool.";
    let response = coding_agent::run_agent_loop(&client, prompt)
        .await
        .expect("agent loop failed");

    assert!(
        response.contains("HELLO_FROM_THE_AGENT"),
        "Expected response to contain the echo output.\nResponse: {response}"
    );
}

#[tokio::test]
async fn e2e_multi_step_tool_calls() {
    require_api_key();
    let tmp = TempDir::new();

    // Step 1: create a source file
    let source = tmp.path().join("source.txt");
    fs::write(&source, "multi-step data").unwrap();

    // Step 2: ask the agent to read it, write a new file, and run bash to verify
    let dest = tmp.path().join("dest.txt");
    let client = coding_agent::create_client();
    let prompt = format!(
        "Do the following steps in order using the available tools:

         1. Read the file at {}
         2. Write the contents you read to the file at {}
         3. Run 'echo DONE' in bash

         After all steps, tell me what you did.",
        source.display(),
        dest.display()
    );
    let response = coding_agent::run_agent_loop(&client, &prompt)
        .await
        .expect("agent loop failed");

    // Verify the dest file was written
    let written = fs::read_to_string(&dest)
        .expect("dest.txt should have been created by the write tool in step 2");
    assert_eq!(
        written.trim(),
        "multi-step data",
        "dest.txt should contain the content from source.txt"
    );

    // Verify the model acknowledged the multi-step workflow
    // (it may say "read"/"wrote"/"contents" and "write"/"wrote" and "bash"/"echo")
    let mentions_read =
        response.contains("read") || response.contains("contents") || response.contains("file");
    let mentions_write =
        response.contains("write") || response.contains("wrote") || response.contains("dest");
    let mentions_bash = response.contains("bash") || response.contains("echo DONE");
    assert!(
        mentions_read && mentions_write && mentions_bash,
        "Expected response to reference the multi-step workflow.\nResponse: {response}"
    );
}
