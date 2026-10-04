//! Proc bodies are only needed until dreamchecker and Find All References are done with them. Keeping them in their own heap means their memory can actually be given back once they're dropped.
#![allow(unsafe_code)]

use libmimalloc_sys::{
    mi_heap_collect, mi_heap_get_backing, mi_heap_new, mi_heap_set_default, mi_heap_t,
};

thread_local! {
    // SAFETY: mi_heap_new is a ffi call, no safety contract
    static PROC_HEAP: *mut mi_heap_t = unsafe { mi_heap_new() };
}

/// Start allocating in the proc heap.
pub fn enter() {
    // SAFETY: the heap belongs to this thread
    PROC_HEAP.with(|&heap| unsafe { mi_heap_set_default(heap) });
}

/// Go back to allocating in this thread's normal heap.
pub fn exit() {
    // SAFETY: the backing heap belongs to this thread
    unsafe { mi_heap_set_default(mi_heap_get_backing()) };
}

/// Give the memory of dropped proc bodies back to the OS.
pub fn collect() {
    // SAFETY: the heap belongs to this thread
    PROC_HEAP.with(|&heap| unsafe { mi_heap_collect(heap, true) });
}
