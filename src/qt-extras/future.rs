use std::sync::mpsc;
use libc::c_void;

pub fn spawn<F1, F2, R>(task: F1, callback: F2) where
    F1: FnOnce() -> R + Send + 'static,
    F2: FnOnce(R) + 'static,
    R: Send + 'static,
{
    unsafe {
        let (tx, rx) = mpsc::channel();
        let (task, task_data) = make_callback(move || {
            let _ = tx.send(task());
        });
        let (cb, cb_data) = make_callback(move || {
            if let Ok(data) = rx.try_recv() {
                callback(data);
            }
        });
        qt_spawn_future(task, task_data, cb, cb_data);
    }
}

unsafe fn make_callback<F: FnOnce()>(f: F) -> (callback_fn, *mut c_void) {
    unsafe extern "C" fn callback<F: FnOnce()>(ptr: *mut c_void) {
        Box::from_raw(ptr as *mut F)();
    }
    (callback::<F>, Box::into_raw(Box::new(f)) as *mut c_void)
}

#[allow(non_camel_case_types)]
type callback_fn = unsafe extern "C" fn(*mut c_void);
extern {
    fn qt_spawn_future(task: callback_fn, task_data: *mut c_void, cb: callback_fn, cb_data: *mut c_void);
}

cpp! {{
    #include <QtCore/QFuture.h>
    #include <QtCore/QFutureWatcher.h>
    #include <QtConcurrent/QtConcurrent>

    extern "C" typedef void (*callback_fn)(void*);

    class RustSignaller: public QFutureWatcher<void> {
        callback_fn callback;
        void* data;
        void handleFinished() {
            callback(data);
            delete this;
        }

    public:
        RustSignaller(callback_fn callback, void* data)
            : callback(callback)
            , data(data)
        {
            QObject::connect(this, &QFutureWatcher::finished, this, &RustSignaller::handleFinished);
        }
    };

    extern "C" void qt_spawn_future(callback_fn task, void* taskData, callback_fn cb, void* cbData) {
        RustSignaller* signaller = new RustSignaller(cb, cbData);
        QFuture<void> future = QtConcurrent::run([=]() {
            task(taskData);
        });
        signaller->setFuture(future);
    }
}}
