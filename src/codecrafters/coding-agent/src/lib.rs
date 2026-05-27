use async_openai::{
    Client, config::OpenAIConfig, types::chat::ChatCompletionMessageToolCalls,
    types::chat::CreateChatCompletionResponse,
};
use serde_json::{Value, json};
use std::env;

/// Returns the tool definitions loaded from `tools.json`.
pub fn tools() -> Value {
    serde_json::from_str(include_str!("../tools.json")).unwrap()
}

/// Creates an OpenAI client configured for OpenRouter from environment variables.
///
/// Required env vars: `OPENROUTER_API_KEY`
/// Optional:         `OPENROUTER_BASE_URL` (default: `https://openrouter.ai/api/v1`)
pub fn create_client() -> Client<OpenAIConfig> {
    let base_url = env::var("OPENROUTER_BASE_URL")
        .unwrap_or_else(|_| "https://openrouter.ai/api/v1".to_string());
    let api_key = env::var("OPENROUTER_API_KEY").expect("OPENROUTER_API_KEY is not set");
    let config = OpenAIConfig::new()
        .with_api_base(base_url)
        .with_api_key(api_key);
    Client::with_config(config)
}

/// Runs the full agent loop: sends a prompt to the model, processes any tool
/// calls (read / write / bash), and returns the final text response.
pub async fn run_agent_loop(
    client: &Client<OpenAIConfig>,
    prompt: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut messages = vec![json!({"role": "user", "content": prompt})];
    let tools = tools();
    let model = env::var("OPENROUTER_MODEL")
        .unwrap_or_else(|_| "cohere/command-r-plus-08-2024".to_string());

    loop {
        let response: CreateChatCompletionResponse = client
            .chat()
            .create_byot(json!({
                "messages": messages,
                "model": model,
                "max_tokens": 2048,
                "tools": tools
            }))
            .await?;

        let message = &response.choices[0].message;
        messages.push(serde_json::to_value(&message)?);

        if let Some(tool_calls) = &message.tool_calls {
            for call in tool_calls {
                if let ChatCompletionMessageToolCalls::Function(tool_call) = call {
                    process_tool_call(tool_call, &mut messages).await?;
                }
            }
        } else if let Some(content) = &message.content {
            return Ok(content.clone());
        }
    }
}

// ---------------------------------------------------------------------------
// Tool handlers
// ---------------------------------------------------------------------------

/// Reads a file and returns its contents.
pub async fn handle_read(file_path: &str) -> Result<String, Box<dyn std::error::Error>> {
    let contents = tokio::fs::read_to_string(file_path).await?;
    Ok(contents)
}

/// Writes content to a file.
pub async fn handle_write(
    file_path: &str,
    content: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    tokio::fs::write(file_path, content).await?;
    Ok(())
}

/// Executes a shell command via bash and returns the output.
pub async fn handle_bash(command: &str) -> Result<String, Box<dyn std::error::Error>> {
    let output = tokio::process::Command::new("bash")
        .arg("-c")
        .arg(command)
        .output()
        .await?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let result = stdout.trim();
    let error = stderr.trim();
    if !error.is_empty() {
        Ok(error.to_string())
    } else {
        Ok(result.to_string())
    }
}

/// Processes a single tool call and pushes the result into `messages`.
pub async fn process_tool_call(
    tool_call: &async_openai::types::chat::ChatCompletionMessageToolCall,
    messages: &mut Vec<Value>,
) -> Result<(), Box<dyn std::error::Error>> {
    let tool = &tool_call.function;
    match tool.name.as_str() {
        "read" => {
            let args: Value = serde_json::from_str(&tool.arguments)?;
            let file_path = args["file_path"].as_str().expect("file_path expected");
            match handle_read(file_path).await {
                Ok(contents) => messages.push(
                    json!({"role": "tool", "tool_call_id": tool_call.id, "content": contents}),
                ),
                Err(e) => messages.push(
                    json!({"role": "tool", "tool_call_id": tool_call.id, "content": format!("Error: {e}")}),
                ),
            }
        }
        "write" => {
            let args: Value = serde_json::from_str(&tool.arguments)?;
            let file_path = args["file_path"].as_str().expect("file_path expected");
            let content = args["content"].as_str().expect("content expected");
            match handle_write(file_path, content).await {
                Ok(()) => messages.push(
                    json!({"role": "tool", "tool_call_id": tool_call.id, "content": "File written successfully"}),
                ),
                Err(e) => messages.push(
                    json!({"role": "tool", "tool_call_id": tool_call.id, "content": format!("Error: {e}")}),
                ),
            }
        }
        "bash" => {
            let args: Value = serde_json::from_str(&tool.arguments)?;
            let command = args["command"].as_str().expect("command expected");
            match handle_bash(command).await {
                Ok(content) => messages.push(
                    json!({"role": "tool", "tool_call_id": tool_call.id, "content": content}),
                ),
                Err(e) => messages.push(
                    json!({"role": "tool", "tool_call_id": tool_call.id, "content": format!("Error: {e}")}),
                ),
            }
        }
        _ => messages.push(
            json!({"role": "tool", "tool_call_id": tool_call.id, "content": format!("Unknown tool: {}", tool.name)}),
        ),
    }
    Ok(())
}
