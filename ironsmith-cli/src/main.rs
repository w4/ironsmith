use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
pub enum Args {
    /// Reads Smithy models in and writes out a single JSON AST model.
    Ast { model: PathBuf },
}

fn main() {
    let args = Args::parse();

    match args {
        Args::Ast { model } => {
            let model = std::fs::read_to_string(model).unwrap();
            let ast = ironsmith_parser::parse_ast(&model).unwrap();
            let semantic = ironsmith_model::SemanticModel::try_from(ast).unwrap();
            println!("{}", serde_json::to_string_pretty(&semantic).unwrap());
        }
    }
}
