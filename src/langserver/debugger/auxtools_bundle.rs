#![cfg(auxtools_bundle)]
use std::fs::File;
use std::io::{Result, Write};
use std::path::{Path, PathBuf};

const BYTES: &[u8] = include_bytes!(env!("AUXTOOLS_BUNDLE_DLL"));

fn write(path: &Path) -> Result<()> {
    File::create(path)?.write_all(BYTES)
}

pub fn extract() -> Result<PathBuf> {
    let exe = std::env::current_exe()?;
    let directory = exe.parent().unwrap();
    for i in 0..9 {
        let dll = directory.join(format!("auxtools_debug_server{}.dll", i));
        if let Ok(()) = write(&dll) {
            return Ok(dll);
        }
    }
    let dll = directory.join("auxtools_debug_server9.dll");
    write(&dll)?;
    Ok(dll)
}
