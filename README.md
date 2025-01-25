# ironsmith

Tools for working with [Smithy](https://smithy.io/) version 2.0 definitions in Rust.

These set of crates are intended to become fully featured code-generation utilities defined entirely within Rust,
sidestepping the Java requirement imposed by the reference implementation. This, however this is an active work in
progress.

Currently, the only fully working and specification compliant crate is `ironsmith-parser` which provides a AST
for Smithy IDL files.
