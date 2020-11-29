use regex::Regex;
use lazy_static;

use super::dap_types::*;
use super::*;

const EXTOOLS_HELP: &str = "
#dis, #disassemble: show disassembly for current stack frame";

const AUXTOOLS_HELP: &str = "
#dis, #disassemble: show disassembly for current stack frame
#dis, #disassemble <proc path> <override id (optional)>: show disassembly for specified proc";

impl Debugger {
    pub fn evaluate(
        &mut self,
        params: EvaluateArguments,
    ) -> Result<EvaluateResponse, Box<dyn std::error::Error>> {
        let input = params.expression.trim_start();

        match &mut self.client {
            DebugClient::Extools(extools) => {
                let extools = extools.get()?;

                if input.starts_with("#help") {
                    return Ok(EvaluateResponse::from(EXTOOLS_HELP.trim()));
                }

                guard!(let Some(frame_id) = params.frameId else {
                    return Err(Box::new(GenericError("Must select a stack frame to evaluate in")));
                });

                let (thread, frame_no) = extools.get_thread_by_frame_id(frame_id)?;

                if input.starts_with('#') {
                    if input == "#dis" || input == "#disassemble" {
                        guard!(let Some(frame) = thread.call_stack.get(frame_no) else {
                            return Err(Box::new(GenericError("Stack frame out of range")));
                        });

                        let bytecode = extools.bytecode(&frame.proc, frame.override_id);
                        return Ok(EvaluateResponse::from(Self::format_disassembly(bytecode)));
                    }
                }
            }

            DebugClient::Auxtools(auxtools) => {
                lazy_static! {
                    static ref DISASSEMBLE_REGEX: Regex = Regex::new(r"^#dis(?:assemble)? (?P<path>[\w/]+) ?(?P<override>[0-9]*)$").unwrap();
                }

                if input.starts_with("#help") {
                    return Ok(EvaluateResponse::from(AUXTOOLS_HELP.trim()));
                }

                if input == "#dis" || input == "#disassemble" {
                    guard!(let Some(frame_id) = params.frameId else {
                        return Err(Box::new(GenericError("Must select a stack frame to evaluate in")));
                    });

                    let (path, override_id) = auxtools.get_current_proc(frame_id as u32)?.ok_or_else(|| {
                        Box::new(GenericError("Couldn't find current proc"))
                    })?;

                    return Ok(EvaluateResponse::from(auxtools.disassemble(&path, override_id)?));
                }

                if let Some(captures) = DISASSEMBLE_REGEX.captures(input) {
                    let path = &captures["path"];
                    let override_id = match captures.name("override").map(|x| x.as_str()) {
                        Some(str) => str.parse::<u32>().unwrap_or(0),
                        _ => 0,
                    };

                    return Ok(EvaluateResponse::from(auxtools.disassemble(path, override_id)?));
                }
            }
        }

        Err(Box::new(GenericError("Not yet implemented")))
    }

    pub fn format_disassembly(
        bytecode: &[super::extools_types::DisassembledInstruction],
    ) -> String {
        let mut buf = String::new();

        let bytes_max_len = bytecode
            .iter()
            .map(|elem| elem.bytes.len())
            .max()
            .unwrap_or(0);
        for instr in bytecode {
            use std::fmt::Write;
            let _ = writeln!(
                buf,
                "{:#8X}  {:width$}  {} {}",
                instr.offset,
                instr.bytes,
                instr.mnemonic,
                instr.comment,
                width = bytes_max_len
            );
        }

        buf
    }
}
