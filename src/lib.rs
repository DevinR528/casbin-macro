use std::convert::TryFrom;

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
