use super::*;
use dap_types::*;

const EXTOOLS_HELP: &str = "
#dis, #disassemble: show disassembly for current stack frame";

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
                    } else {
                        return Err(Box::new(GenericError("Unknown #command")));
                    }
                }
            }

            DebugClient::Auxtools(auxtools) => {
                let response =
                    auxtools.eval(params.frameId.map(|x| x as u32), input, params.context)?;

                return Ok(EvaluateResponse {
                    result: response.value,
                    variablesReference: response.variables.map(|x| x.0 as i64).unwrap_or(0),
                    ..Default::default()
                });
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
