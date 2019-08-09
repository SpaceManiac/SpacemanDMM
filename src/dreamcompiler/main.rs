extern crate dreamcompiler;
extern crate dreammaker as dm;

use dm::Context;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use windows_named_pipe as Pipe;

fn main() {
	std::env::set_var("RUST_BACKTRACE", "1");
	let proc_path = &std::env::args().collect::<Vec<String>>()[1];
	let mut context = Context::default();
	let dme = dm::detect_environment_default()
		.expect("oh shit")
		.expect("oh fuck");
	let pp = dm::preprocessor::Preprocessor::new(&context, dme).expect("Ayy lmao");
	let indents = dm::indents::IndentProcessor::new(&context, pp);
	let mut parser = dm::parser::Parser::new(&context, indents);
	parser.enable_procs();
	let tree = parser.parse_object_tree();

	let result = dreamcompiler::compile(&context, &tree, proc_path.to_owned());
	println!("{:#X?}", result.bytecode);
	//println!("{:?}", result.strings);
	let serialized = serde_json::to_string(&result).expect("Somehow unserializable compile result");
	//println!("{}", serialized);
	println!("Compiled, ready to send patch.");
	let mut pipe = Pipe::PipeStream::connect(Path::new("\\\\.\\pipe\\DMDBGPatch")).unwrap();
	//let mut pipe = Pipe::PipeListener::bind(Path::new("\\\\.\\pipe\\dmcompiler"))
	//    .unwrap()
	//    .accept()
	//    .unwrap();
	pipe.write_all(&serialized.as_bytes()).unwrap();
	pipe.write_all(b"\n").unwrap();
	pipe.flush().unwrap();
}
