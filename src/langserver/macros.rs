//! Utility macros.

macro_rules! handle_method_call {
    ($(on $what:ident(&mut $self:ident, $p:pat) $b:block)*) => {
        impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
            fn handle_method_call(&mut self, call: jsonrpc::MethodCall) -> Result<serde_json::Value, jsonrpc::Error> {
                use langserver::request::*;

                // "If the server receives a request... before the initialize request...
                // the response should be an error with code: -32002"
                if call.method != <Initialize>::METHOD && self.status != InitStatus::Running {
                    return Err(jsonrpc::Error {
                        code: jsonrpc::ErrorCode::from(-32002),
                        message: "method call before initialize or after shutdown".to_owned(),
                        data: None,
                    })
                }

                let params_value = params_to_value(call.params);

                $(if call.method == <$what>::METHOD {
                    let params: <$what as Request>::Params = serde_json::from_value(params_value)
                        .map_err(invalid_request)?;
                    let result: <$what as Request>::Result = self.$what(params)?;
                    Ok(serde_json::to_value(result).expect("encode problem"))
                } else)* {
                    eprintln!("Call NYI: {} -> {:?}", call.method, params_value);
                    Err(jsonrpc::Error {
                        code: jsonrpc::ErrorCode::InternalError,
                        message: "not yet implemented".to_owned(),
                        data: None,
                    })
                }
            }

            $(
                #[allow(non_snake_case)]
                fn $what(&mut $self, $p: <langserver::request::$what as langserver::request::Request>::Params)
                -> Result<<langserver::request::$what as langserver::request::Request>::Result, jsonrpc::Error>
                {
                    #[allow(unused_imports)]
                    use langserver::*;
                    #[allow(unused_imports)]
                    use langserver::request::*;
                    let _v = $b;
                    #[allow(unreachable_code)] { Ok(_v) }
                }
            )*
        }
    }
}

macro_rules! handle_notification {
    ($(on $what:ident(&mut $self:ident, $p:pat) $b:block)*) => {
        impl<'a, R: io::RequestRead, W: io::ResponseWrite> Engine<'a, R, W> {
            fn handle_notification(&mut self, notification: jsonrpc::Notification) -> Result<(), jsonrpc::Error> {
                use langserver::notification::*;

                // "Notifications should be dropped, except for the exit notification"
                if notification.method != <Exit>::METHOD && self.status != InitStatus::Running {
                    return Ok(())
                }

                let params_value = params_to_value(notification.params);

                $(if notification.method == <$what>::METHOD {
                    let params: <$what as Notification>::Params = serde_json::from_value(params_value)
                        .map_err(invalid_request)?;
                    self.$what(params)?;
                } else)* {
                    eprintln!("Notify NYI: {} => {:?}", notification.method, params_value);
                }
                Ok(())
            }

            $(
                #[allow(non_snake_case)]
                fn $what(&mut $self, $p: <langserver::notification::$what as langserver::notification::Notification>::Params)
                -> Result<(), jsonrpc::Error>
                {
                    #[allow(unused_imports)]
                    use langserver::*;
                    #[allow(unused_imports)]
                    use langserver::notification::*;
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
