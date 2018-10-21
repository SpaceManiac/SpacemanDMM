use std::process::Command;
use std::env;
use std::path::Path;

fn main() {
    if cfg!(windows) {
        let out_dir = env::var("OUT_DIR").ok().expect("can't find out_dir");

        if cfg!(target_env="msvc") {
            if let Err(e) = Command::new("windres")
                .args(&["res/editor.rc", "-o"])
                .arg(&format!("{}/editor_rc.lib", out_dir))
                .status()
            {
                println!("cargo:warning=`windres` unavailable: {}", e);
                return;
            }
        } else {
            if let Err(e) = Command::new("windres")
                .args(&["res/editor.rc", "-o"])
                .arg(&format!("{}/editor.rc.o", out_dir))
                .status()
            {
                println!("cargo:warning=`windres` unavailable: {}", e);
                return;
            }
            Command::new("ar")
                .args(&["crus", "libeditor_rc.a", "editor.rc.o"])
                .current_dir(Path::new(&out_dir))
                .status()
                .unwrap();
        }

        println!("cargo:rerun-if-changed=editor.rc");
        println!("cargo:rerun-if-changed=gasmask.ico");
        println!("cargo:rustc-link-search=native={}", out_dir);
        println!("cargo:rustc-link-lib=static=editor_rc");
    }
}
