mergeInto(LibraryManager.library, {
    handle_output: function(ptr, len) {
        Module.handle_output(ptr, len);
    },
    return_into_js: function() {
        throw 'SimulateInfiniteLoop';
    },
});
