use std::convert::TryFrom;
use std::str::FromStr;


use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_macro_input, DeriveInput};

mod casbin;
use casbin::RawModel;

#[proc_macro]
pub fn parse_model(input: TokenStream) -> TokenStream {
    let model = parse_macro_input!(input as RawModel);
    model.into_token_stream().into()
}

#[proc_macro]
pub fn parse_model_str(input: TokenStream) -> TokenStream {
    let input = TokenStream::from_str(&input.to_string().replace("\"", "")).unwrap();
    let model = parse_macro_input!(input as RawModel);
    model.into_token_stream().into()
}
