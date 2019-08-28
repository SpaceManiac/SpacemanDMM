extern crate dreammaker as dm;

use std::collections::HashMap;

use dm::ast::*;
use dm::objtree::{ObjectTree, ProcRef, TypeRef};
use dm::Context;

mod bytecode;
use bytecode::*;

use dm::ast::PathOp;
use dm::objtree::Code;
use dm::objtree::NavigatePathResult::ProcPath;

type CompileResult = std::result::Result<CompileData, String>;

#[derive(Debug)]
pub struct Compiler<'a> {
	context: &'a Context,
	objtree: &'a ObjectTree,
	raw_path: String,
	proc: &'a ProcRef<'a>,
	local_vars: HashMap<String, i32>,
	bytecode: Vec<i32>,
	strings_to_link: Vec<(String, i32)>,
	src_procs: Vec<(String, i32)>,
	global_procs: Vec<(String, i32)>,
}

#[derive(serde_derive::Serialize, Debug)]
pub struct CompileData {
	pub path: String,
	pub bytecode: Vec<i32>,
	pub strings: Vec<(String, i32)>,
	pub local_count: i32,
	pub src_procs: Vec<(String, i32)>,
	pub global_procs: Vec<(String, i32)>,
}

pub fn compile(context: &Context, objtree: &ObjectTree, procpath: String) -> CompileResult {
	let mut has_proc = false;
	let mut ass = procpath
		.split("/")
		.skip(1)
		.map(|part| {
			if part == "proc" || part == "verb" {
				has_proc = true
			}
			(PathOp::Slash, part)
		})
		.collect::<Vec<(PathOp, &str)>>();
	if !has_proc {
		ass.insert(ass.len() - 1, (PathOp::Slash, "proc"));
	}
	//println!("{:?}", ass);
	let proc = match objtree.root().navigate_path(&ass) {
		Some(ProcPath(xd, kind)) => xd,
		Some(TypePath) => panic!("Typepath"),
		None => panic!("Proc not found"),
	};

	let mut compiler = Compiler::new(&context, &objtree, &proc, &procpath);
	compiler.compile()
}

impl<'a> Compiler<'a> {
	pub fn new(
		context: &'a Context,
		objtree: &'a ObjectTree,
		proc: &'a ProcRef,
		raw_path: &String,
	) -> Self {
		let raw_path = raw_path.clone();
		Compiler {
			context,
			objtree,
			proc,
			raw_path,
			local_vars: HashMap::new(),
			bytecode: Vec::with_capacity(512),
			strings_to_link: Vec::with_capacity(32),
			src_procs: Vec::with_capacity(32),
			global_procs: Vec::with_capacity(32),
		}
	}

	pub fn compile(&mut self) -> CompileResult {
		if let dm::objtree::Code::Present(ref code) = self.proc.code {
			self.visit_block(code)?;
		}
		self.bytecode.push(0);
		Ok(CompileData {
			path: self.raw_path.clone(),
			bytecode: self.bytecode.clone(),
			strings: self.strings_to_link.clone(),
			local_count: self.local_vars.len() as i32,
			src_procs: self.src_procs.clone(),
			global_procs: self.global_procs.clone(),
		})
	}

	pub fn emit(&mut self, bytes: &[i32]) {
		for byte in bytes {
			self.bytecode.push(*byte)
		}
	}

	pub fn visit_block(&mut self, block: &'a [Spanned<Statement>]) -> Result<(), String> {
		for stmt in block.iter() {
			self.visit_statement(&stmt.elem)?;
		}
		Ok(())
	}

	pub fn visit_statement(&mut self, statement: &'a Statement) -> Result<(), String> {
		match statement {
			Statement::Expr(expr) => {
				self.visit_expression_statement(expr)?;
			}
			Statement::Var(var) => {
				self.visit_var(var)?;
			}
			Statement::If { arms, else_arm } => {
				self.visit_if(arms, else_arm)?;
			}
			Statement::Setting { .. } => {}
			_ => panic!("Unimplemented statement"),
		}
		Ok(())
	}

	pub fn visit_ident(&mut self, name: &String, follows: &'a Vec<Spanned<Follow>>) {
		let mut end = Vec::<i32>::with_capacity(8);
		self.bytecode.push(OP_GETVAR);
		for follow in follows.iter() {
			match &follow.elem {
				Follow::Field(kind, name) => {
					self.bytecode.push(VAR_SUBVAR);
					end.push(0x00);
					self.strings_to_link.push((
						name.clone(),
						self.bytecode.len() as i32 + follows.len() as i32 + 2,
					));
				}
				_ => panic!("Unimplemented follow"),
			}
		}
		if let Some(idx) = self.local_vars.get(name) {
			self.bytecode.push(VAR_LOCAL);
			self.bytecode.push(*idx);
		} else if self.proc.ty().vars.get(name).is_some() { //TODO: check if alternative (GETVAR <name>) is faster than this
			self.emit(&[VAR_SUBVAR, VAR_SRC, 0x00]);
			self.strings_to_link
				.push((name.clone(), self.bytecode.len() as i32 - 1));
		} else if name == "world" {
			self.bytecode.push(VAR_WORLD);
		} else if name == "src" {
			self.bytecode.push(VAR_SRC);
		} else {
			println!("{}", name);
			panic!("Unimplemented ident access");
		}
		end.reverse();
		self.bytecode.append(&mut end);
	}

	fn visit_expression(&mut self, expr: &'a Expression) -> Result<(), String> {
		self.visit_expression_impl(expr, false)
	}

	fn visit_expression_statement(&mut self, expr: &'a Expression) -> Result<(), String> {
		self.visit_expression_impl(expr, true)
	}

	pub fn visit_expression_impl(&mut self, expr: &'a Expression, is_statement: bool) -> Result<(), String> {
		match expr {
			Expression::Base {
				unary,
				term,
				follow,
			} => {
				self.visit_term(&term.elem, &follow, is_statement)?;

				for u in unary.iter().rev() {
					self.emit(&[match u {
						UnaryOp::Not => OP_NOT,
						UnaryOp::Neg => OP_ANEG,
						_ => panic!("Unimplemented unary"),
					}]);
				}
			}
			Expression::BinaryOp { op, lhs, rhs } => {
				if let BinaryOp::LShift = op {
					if is_statement {
						self.visit_output(lhs, rhs)?;
						return Ok(());
					}
				}
				self.visit_expression(lhs)?;
				self.visit_expression(rhs)?;
				self.emit(&[match op {
					BinaryOp::Add => OP_ADD,
					BinaryOp::Sub => OP_SUB,
					BinaryOp::Mul => OP_ADD, //TODO: update opcodes from dmdism
					BinaryOp::Div => OP_ADD,
					BinaryOp::Eq => OP_TEQ,
					BinaryOp::NotEq => OP_TNE,
					_ => panic!("Binop not implemented"),
				}]);
			}
			_ => panic!("Expression not implemented"),
		}
		Ok(())
	}

	pub fn visit_term(
		&mut self,
		term: &'a Term,
		follow: &'a Vec<Spanned<Follow>>,
		is_statement: bool,
	) -> Result<(), String> {
		match term {
			Term::Int(number) => self.emit(&[OP_PUSHI, *number]),
			Term::Float(number) => {
				self.emit(&[OP_PUSHVAL, TYPE_NUMBER, number.to_bits() as i32, 0x00])
			}
			Term::String(string) => {
				self.emit(&[OP_PUSHVAL, TYPE_STRING, 0x00]);
				self.strings_to_link
					.push((string.to_string(), self.bytecode.len() as i32 - 1));
			}
			Term::InterpString(init_string, interps) => {
				let mut final_string = init_string.clone();
				for (i, (expr, string_part)) in interps.iter().enumerate() {
					let mut buf: Vec<u8> = Vec::new();
					if let Some(e) = expr {
						self.visit_expression(&e)?;
					}
					buf.push(0x05);
					buf.push((interps.len() - i) as u8);
					unsafe { final_string.push_str(String::from_utf8_unchecked(buf).as_str()); }
					final_string.push_str(&string_part);
				}
				for ch in final_string.bytes() {
					println!("{}", ch as u32);
				}
				self.emit(&[OP_FORMAT, 0x00, interps.len() as i32]);
				self.strings_to_link
					.push((final_string, self.bytecode.len() as i32 - 2));
			},
			Term::Ident(name) => self.visit_ident(&name, follow),
			Term::Call(name, args) => self.visit_call(name, args, is_statement)?,
			_ => panic!("Unimplemented term"),
		}
		Ok(())
	}

	pub fn visit_var(&mut self, var: &'a VarStatement) -> Result<(), String> {
		if let Some(ref expr) = var.value.as_ref() {
			self.visit_expression(expr)?;
		} else {
			self.emit(&[OP_GETVAR, VAR_NULL]);
		}
		self.emit(&[OP_SETVAR, VAR_LOCAL, self.local_vars.len() as i32]);
		self.local_vars
			.insert(var.name.clone(), self.local_vars.len() as i32);
		Ok(())
	}

	pub fn visit_output(&mut self, lhs: &'a Expression, rhs: &'a Expression) -> Result<(), String> {
		self.visit_expression(lhs)?;
		self.visit_expression(rhs)?;
		self.bytecode.push(OP_OUTPUT);
		Ok(())
	}

	fn visit_if(
		&mut self,
		arms: &'a Vec<(Expression, Vec<Spanned<Statement>>)>,
		else_arm: &'a Option<Vec<Spanned<Statement>>>,
	) -> Result<(), String> {
		let mut patch_after_else: Vec<usize> = Vec::with_capacity(arms.len());
		for &(ref condition, ref block) in arms.iter() {
			self.visit_expression(condition)?;
			self.emit(&[OP_TEST, OP_JZ, 0x00]); //TODO: Add OP_POP based on whether the expression had a TEQ/TNE or other comp ops //Maybe not?
			let jump_location = self.bytecode.len() - 1;
			self.visit_block(block)?;
			if else_arm.is_some() {
				self.emit(&[OP_JMP, 0x00]);
				patch_after_else.push(self.bytecode.len() - 1);
			}
			self.bytecode[jump_location] = self.bytecode.len() as i32;
		}
		if let Some(else_arm) = else_arm {
			self.visit_block(else_arm)?;
			for patch in patch_after_else {
				self.bytecode[patch] = self.bytecode.len() as i32;
			}
		}
		Ok(())
	}

	fn visit_call(&mut self, name: &'a String, args: &'a Vec<Expression>, is_statement: bool) -> Result<(), String> {
		for expr in args {
			self.visit_expression(&expr)?;
		}
		match self.try_builtin(name.as_str(), args.len()) {
			Err(Some(e)) => return Err(e),
			_ => (),
		}
		if let Some(_) = self.proc.ty().procs.get(name) {
			self.emit(&[OP_CALLOBJ, VAR_SRC, 0x1337]);
			self.src_procs
				.push((name.clone(), self.bytecode.len() as i32 - 1));
		} else if self.objtree.root().get_proc(name).is_some() {
			self.emit(&[OP_CALLGLOB, args.len() as i32, 0x1337]);
			self.global_procs
				.push((name.clone(), self.bytecode.len() as i32 - 1));
		} else {
			return Err(format!("Proc not found: {}", name));
		}
		Ok(())
	}

	fn try_builtin(&mut self, name: &str, num_args: usize) -> Result<(), Option<String>> {
		//Simple arg counts first
		if let Some(opcode) = match name {
			"round" => match num_args {
				1 => Some(OP_ROUND),
				2 => Some(OP_ROUNDN),
				_ => None,
			},
			_ => Some(-1), //sentinel value
		} {
			if opcode != -1 {
				self.emit(&[opcode]);
				return Ok(());
			}
		} else {
			return Err(Some(format!("Incorrent number of arguments to {}()", name)));
		}
		
		return Err(None);
	}
}
