#[macro_use] extern crate log;
extern crate env_logger;

use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};


fn handle_client(mut stream: TcpStream) {
    stream.set_nodelay(true).unwrap();
    stream.write_all(&[1, 1, 4, 0, 0, 0, 0, 0]).unwrap();
    let mut rb = [0u8; 8];
    loop {
        match stream.read(&mut rb) {
            Ok(len) => {
                if len != 8 {
                    trace!("Got packet {:?} with length {}", &rb[0..len], len)
                }
                trace!("Reading {:?}", rb);
                match rb[0] {
                    1 => stream.write_all(&[108, 1, 0, 0, 0, 0, 0, 0]),
                    104 => {
                        print!("{}", (rb[1] as char).to_string());
                        stream.write_all(&[106, 1, 0, 0, 0, 0, 0, 0])
                    },
                    106 if rb[1] == 0 => stream.write_all(&rb),
                    c => { trace!("Ignoring {}", c); Ok(()) }
                };
            }
            Err(err) => {
                warn!("Disconnecting due to err: {}", err);
                return
            }
        }
    }
}

fn main() {
    let listener = TcpListener::bind("127.0.0.1:5555").unwrap();
    for stream in listener.incoming() {
        println!("Got connection");
        match stream {
            Ok(stream) => handle_client(stream),
            Err(e) => { panic!("Connection err {}", e) }
        }
    }
}

