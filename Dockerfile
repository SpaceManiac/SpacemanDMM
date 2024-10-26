FROM rust:1.74

WORKDIR /usr/src/myapp
COPY . .

RUN cargo build -p dmm-tools-cli --release
RUN ln -s /usr/src/myapp/target/release/dmm-tools /usr/src/myapp/dmm-tools
ENTRYPOINT ["./dmm-tools"]
CMD ["help"]
