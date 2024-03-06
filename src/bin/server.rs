use dotenvy::dotenv;
use echo::core::html::HTML;
use echo::core::parser::Parser;
use std::env::var;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;

fn handler(buf: &[u8], size: usize, remote: &str) -> String {
    let parser = Parser::new(buf, size);

    let html = HTML::new(remote, &parser);

    let document = html.document();

    format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n{}\r\n",
        document
    )
}
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    dotenv().ok();

    let host = var("HOST").unwrap_or_else(|_| "127.0.0.1".to_string());
    let port = var("PORT").unwrap_or_else(|_| "8080".to_string());

    let addr = format!("{}:{}", host, port);

    let listener = TcpListener::bind(addr).await?;

    loop {
        let (mut socket, _) = listener.accept().await?;

        // Get the remote address
        let remote_addr = match socket.peer_addr() {
            Ok(addr) => addr,
            Err(e) => {
                eprintln!("failed to get peer address; err = {:?}", e);
                return Err(e.into());
            }
        };

        tokio::spawn(async move {
            let mut buf = [0; 4096];

            let remote = format!("{} {}", remote_addr.ip(), remote_addr.port());

            let n = match socket.read(&mut buf).await {
                // socket closed
                Ok(0) => return,
                Ok(n) => n,
                Err(e) => {
                    eprintln!("failed to read from socket; err = {:?}", e);
                    return;
                }
            };

            let response = handler(&buf, n, remote.as_str());

            if let Err(e) = socket.write_all(response.as_bytes()).await {
                eprintln!("failed to write to socket; err = {:?}", e);
                return;
            }

            // Shutdown the socket after sending the response
            if let Err(e) = socket.shutdown().await {
                eprintln!("failed to shutdown socket; err = {:?}", e);
            }
        });
    }
}
