//! Utility macros.
macro_rules! if_annotation {
    ($p:pat in $a:expr; $b:block) => {
        for (_, thing) in $a.clone() {
            if let $p = thing {
                $b
                break
            }
        }
    }
}

macro_rules! match_annotation {
    ($a:expr; $($p:pat => $b:block,)*) => {
        for (_, thing) in $a.clone() {
            match thing {
                $($p => $b,)*
                _ => {}
            }
        }
    }
}
