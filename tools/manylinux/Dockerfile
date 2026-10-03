# manylinux2014 (CentOS 7 based, glibc 2.17).
# https://github.com/pypa/manylinux#manylinux2014-centos-7-based-glibc-217
# Matches Rust's glibc 2.17 minimum as of 2026-10-03.
# https://doc.rust-lang.org/nightly/rustc/platform-support.html#tier-1-with-host-tools
FROM quay.io/pypa/manylinux2014_x86_64

COPY rustup-init /usr/local/bin/rustup-init
RUN rustup-init -y --default-toolchain 1.99.0 --profile minimal
ENV PATH="/root/.cargo/bin:$PATH"
