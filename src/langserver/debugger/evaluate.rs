use super::*;
use super::dap_types::*;

const EVALUATE_HELP: &str = "
#dis, #disassemble: show disassembly for current stack frame";

impl Debugger {
    pub fn evaluate(&mut self, params: EvaluateArguments) -> Result<EvaluateResponse, Box<dyn std::error::Error>> {
        let input = params.expression.trim_start();
        if input.starts_with("#help") {
            return Ok(EvaluateResponse::from(EVALUATE_HELP.trim()));
        }

        let extools = self.extools.get()?;

        if input.starts_with('#') {
            if input == "#dis" || input == "#disassemble" {
                let thread = extools.get_default_thread()?;
                guard!(let Some(frame) = thread.call_stack.get(params.frameId.unwrap_or(0) as usize) else {
                    return Err(Box::new(GenericError("Stack frame out of range")));
                });

                let bytecode = extools.bytecode(&frame.proc, frame.override_id);
                let mut buf = String::new();

                let bytes_max_len = bytecode.iter().map(|elem| elem.bytes.len()).max().unwrap_or(0);
                for instr in bytecode {
                    use std::fmt::Write;
                    let _ = writeln!(buf, "{:6}  {:width$}  {} {}", instr.offset, instr.bytes, instr.mnemonic, instr.comment, width = bytes_max_len);
                }

                return Ok(EvaluateResponse::from(buf));
            }
        }

        Err(Box::new(GenericError("Not yet implemented")))
    }
}
