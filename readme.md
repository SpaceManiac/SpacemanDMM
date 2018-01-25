##Installation 

These are very basic instructions

1. Clone the repository to your machine
2. Install Rust, following the directions at https://www.rust-lang.org/en-US/install.html or update your existing installation
3. In your `SpacemanDMM` directory, run `cargo build -p cli` (the -p skips QT)

##Usage 

In the same directory as `tgstation.dme`:  
`target/debug/dmm-tools minimap -o dir/to/save/minimap path/to/map.dmm`


##Recomendations 

The minimap output is a very large PNG (Box: 9.3mb). You are strongly advised to run the resulting file through image optimization software, such as `pngcrush`