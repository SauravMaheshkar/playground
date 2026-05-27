use clap::Parser;
use coding_agent::{create_client, run_agent_loop};

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[arg(short = 'p', long)]
    prompt: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    dotenv::dotenv().ok();

    let client = create_client();
    let response = run_agent_loop(&client, &args.prompt).await?;

    println!("{response}");
    Ok(())
}
