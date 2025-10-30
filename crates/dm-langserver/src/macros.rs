//! Utility macros.

macro_rules! handle_request {
    ($(on $what:ident(&mut $self:ident, $p:pat) $b:block)*) => {
        impl Debugger {
            fn handle_request_table(command: &str) -> Option<fn(&mut Self, serde_json::Value) -> Result<serde_json::Value, Box<dyn Error>>> {
                use dap_types::*;
                $(if command == <$what>::COMMAND {
                    Some(|this, arguments| {
                        let params: <$what as Request>::Params = serde_json::from_value(arguments)?;
                        let result: <$what as Request>::Result = this.$what(params)?;
                        Ok(serde_json::to_value(result).expect("encode problem"))
                    })
                } else)* {
                    None
                }
            }

            $(
                #[allow(non_snake_case)]
                fn $what(&mut $self, $p: <$what as dap_types::Request>::Params)
                -> Result<<$what as dap_types::Request>::Result, Box<dyn Error>>
                {
                    let _v = $b;
                    #[allow(unreachable_code)] { Ok(_v) }
                }
            )*
        }
    }
}

macro_rules! handle_extools {
    ($(on $what:ident(&mut $self:ident, $p:pat) $b:block)*) => {
        impl ExtoolsThread {
            fn handle_response_table(type_: &str) -> Option<fn(&mut Self, serde_json::Value) -> Result<(), Box<dyn Error>>> {
                $(if type_ == <$what as Response>::TYPE {
                    Some(|this, content| {
                        let deserialized: $what = serde_json::from_value(content)?;
                        this.$what(deserialized)
                    })
                } else)* {
                    None
                }
            }

            $(
                #[allow(non_snake_case)]
                fn $what(&mut $self, $p: $what) -> Result<(), Box<dyn Error>> {
                    let _v = $b;
                    #[allow(unreachable_code)] { Ok(_v) }
                }
            )*
        }
    }
}

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
