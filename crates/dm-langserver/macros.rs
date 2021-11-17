//! Utility macros.

pub mod all_methods {
    pub use lsp_types::request::*;
    pub use crate::extras::*;
}

pub mod all_notifications {
    pub use lsp_types::notification::*;
    pub use crate::extras::*;
}

macro_rules! handle_method_call {
    ($($(#[$attr:meta])* on $what:ident(&mut $self:ident, $p:pat) $b:block)*) => {
        impl<'a> Engine<'a> {
            fn handle_method_call_table(method: &str) -> Option<fn(&mut Self, serde_json::Value) -> Result<serde_json::Value, jsonrpc::Error>> {
                use macros::all_methods::*;
                $(if method == <$what>::METHOD {
                    Some(|this, params_value| {
                        let params: <$what as Request>::Params = serde_json::from_value(params_value).map_err(invalid_request)?;
                        let result: <$what as Request>::Result = this.$what(params)?;
                        Ok(serde_json::to_value(result).expect("encode problem"))
                    })
                } else)* {
                    None
                }
            }

            $(
                #[allow(non_snake_case)]
                $(#[$attr])*
                fn $what(&mut $self, $p: <macros::all_methods::$what as lsp_types::request::Request>::Params)
                -> Result<<macros::all_methods::$what as lsp_types::request::Request>::Result, jsonrpc::Error>
                {
                    #[allow(unused_imports)]
                    use lsp_types::*;
                    #[allow(unused_imports)]
                    use lsp_types::request::*;
                    let _v = $b;
                    #[allow(unreachable_code)] { Ok(_v) }
                }
            )*
        }
    }
}

macro_rules! handle_notification {
    ($(on $what:ident(&mut $self:ident, $p:pat) $b:block)*) => {
        impl<'a> Engine<'a> {
            fn handle_notification_table(method: &str) -> Option<fn(&mut Self, serde_json::Value) -> Result<(), jsonrpc::Error>> {
                use macros::all_notifications::*;
                $(if method == <$what>::METHOD {
                    Some(|this, params_value| {
                        let params: <$what as Notification>::Params = serde_json::from_value(params_value).map_err(invalid_request)?;
                        this.$what(params)
                    })
                } else)* {
                    None
                }
            }

            $(
                #[allow(non_snake_case)]
                fn $what(&mut $self, $p: <macros::all_notifications::$what as lsp_types::notification::Notification>::Params)
                -> Result<(), jsonrpc::Error>
                {
                    #[allow(unused_imports)]
                    use lsp_types::*;
                    #[allow(unused_imports)]
                    use lsp_types::notification::*;
                    let _v = $b;
                    #[allow(unreachable_code)] { Ok(_v) }
                }
            )*
        }
    }
}

macro_rules! handle_request {
    ($(on $what:ident(&mut $self:ident, $p:pat) $b:block)*) => {
        impl Debugger {
            fn handle_request_table(command: &str) -> Option<fn(&mut Self, serde_json::Value) -> Result<serde_json::Value, Box<dyn Error>>> {
                use crate::debugger::dap_types::*;
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
                fn $what(&mut $self, $p: <$what as crate::debugger::dap_types::Request>::Params)
                -> Result<<$what as crate::debugger::dap_types::Request>::Result, Box<dyn Error>>
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
    ($a:expr; $($($p:pat)|* => $b:block,)*) => {
        for (_, thing) in $a.clone() {
            match thing {
                $($($p)|* => $b,)*
                _ => {}
            }
        }
    }
}
