use std::convert::{TryFrom, TryInto as _};

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    visit::Visit,
    Field, FieldValue, Ident, Token, Type,
};

macro_rules! header {
    ($name:ident in $input:ident for $kw:ty) => {
        let $name = {
            match $input.parse::<$kw>() {
                // this could be a compiler error
                Err(err) => Err(syn::Error::new($input.span(), format!("bad request definition {}: {}", stringify!($input), err))),
                def => def,
            }
        }?;
    };
}

macro_rules! request {
    ($input:ident) => {
        {
            if let Ok(_) = $input.parse::<kw::sub>() {
                Ok(RequestDef::Subject)
            } else if let Ok(_) = $input.parse::<kw::obj>() {
                Ok(RequestDef::Object)
            } else if let Ok(_) = $input.parse::<kw::act>() {
                Ok(RequestDef::Action)
            } else {
                Err(syn::Error::new($input.span(), format!("bad request definition {:?}", stringify!($input))))
            }
        }
    };
}

macro_rules! policy {
    ($input:ident) => {
        {
            if let Ok(_) = $input.parse::<kw::sub>() {
                Ok(PolicyDef::Subject)
            } else if let Ok(_) = $input.parse::<kw::obj>() {
                Ok(PolicyDef::Object)
            } else if let Ok(_) = $input.parse::<kw::act>() {
                Ok(PolicyDef::Action)
            } else if let Ok(_) = $input.parse::<kw::eft>() {
                Ok(PolicyDef::Effect)
            } else {
                Err(syn::Error::new($input.span(), format!("bad policy definition {:?}", stringify!($input))))
            }
        }
    };
}

macro_rules! policy_type {
    ($input:ident) => {
        {
            if let Ok(_) = $input.parse::<kw::r>() {
                Ok(Heading::Request)
            } else if let Ok(_) = $input.parse::<kw::p>() {
                Ok(Heading::Policy)
            } else if let Ok(_) = $input.parse::<kw::e>() {
                Ok(Heading::PolicyEffect)
            } else if let Ok(_) = $input.parse::<kw::m>() {
                Ok(Heading::Matcher)
            } else if let Ok(_) = $input.parse::<kw::g>() {
                Ok(Heading::Role)
            } else {
                Err(syn::Error::new($input.span(), format!("bad request definition {:?}", stringify!($input))))
            }
        }
    };
}

macro_rules! peek_policy_type {
    ($input:ident) => {
        {
            if $input.peek(kw::r) {
                Ok(Heading::Request)
            } else if $input.peek(kw::p) {
                Ok(Heading::Policy)
            } else if $input.peek(kw::e) {
                Ok(Heading::PolicyEffect)
            } else if $input.peek(kw::m) {
                Ok(Heading::Matcher)
            } else if $input.peek(kw::g) {
                Ok(Heading::Role)
            } else {
                Err(syn::Error::new($input.span(), format!("bad request definition {}", stringify!($input))))
            }
        }
    };
}
/// Eats the keywords and parenthesized contend leaving the ParseStream at the condition
/// we want to parse. 
fn parse_effect<'a>(input: &'a ParseStream<'a>) -> syn::Result<syn::parse::ParseBuffer> {
    input.parse::<kw::some>()?;
    let where_paren;
    syn::parenthesized!(where_paren in input);
    where_paren.parse::<Token![where]>()?;
    let clause;
    syn::parenthesized!(clause in where_paren);
    clause.parse::<kw::p>()?;
    clause.parse::<Token![.]>()?;
    Ok(clause)
}

mod kw {
    use syn::custom_keyword;
    custom_keyword!(request_definition);
    custom_keyword!(policy_definition);
    custom_keyword!(role_definition);
    custom_keyword!(policy_effect);
    custom_keyword!(matchers);

    custom_keyword!(r);
    custom_keyword!(p);
    custom_keyword!(e);
    custom_keyword!(m);
    custom_keyword!(g);

    custom_keyword!(sub);
    custom_keyword!(obj);
    custom_keyword!(act);
    custom_keyword!(eft);

    custom_keyword!(some);
    // `where` is already a keyword

    custom_keyword!(allow);
    custom_keyword!(deny);
}

#[derive(Clone, Copy, Debug)]
pub enum Heading {
    Request,
    Policy,
    Role,
    PolicyEffect,
    Matcher,
}

#[derive(Clone, Copy, Debug)]
pub enum RequestDef {
    Subject,
    Object,
    Action,
}

#[derive(Clone, Copy, Debug)]
pub enum PolicyDef {
    Subject,
    Object,
    Action,
    Effect,
}

#[derive(Clone, Copy, Debug)]
pub enum Effect {
    Allow,
    Deny,
}

#[derive(Clone, Copy, Debug)]
pub enum When {
    Equal,
    NotEq,
}

#[derive(Clone, Debug)]
pub struct Condition {
    cond: PolicyDef,
    eft: Effect,
    when: When,
}


#[derive(Clone, Debug)]
pub enum PolicyEffect {
    Some(Condition),
    NotSome(Condition),
}

#[derive(Clone, Debug)]
pub enum CondPairs {
    RequestPolicy { req: RequestDef, pol: PolicyDef, cond: When, },
    RequestRequest { req: RequestDef, req2: RequestDef, cond: When, },
}

#[derive(Clone, Debug, Default)]
pub struct MatcherDef {
    cond: Vec<CondPairs>
}

pub struct RawModel {
    req_def: Vec<RequestDef>,
    policy_def: Vec<PolicyDef>,
    policy_eft: PolicyEffect,
    matchers: MatcherDef
}


impl Parse for CondPairs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        match policy_type!(input)? {
            Heading::Request => {
                input.parse::<Token![.]>()?;
                make_cond_pair(Heading::Policy, request!(input)?, input)
            },
            Heading::Policy => {
                input.parse::<Token![.]>()?;
                make_cond_pair(Heading::Policy, request!(input)?, input)
            },
            _ => /* parse more variants */ todo!("impl more parsing"),
        }
    }
}

impl Parse for RawModel {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut req_def = Vec::default();
        let mut policy_def = Vec::default();
        let mut matchers = MatcherDef::default();

        // REQUEST DEFINITION
        let heading;
        syn::bracketed!(heading in input);
        let mut _title: Heading;
        header!(_title in heading for kw::request_definition);
        // the `r` in r = ...
        input.parse::<kw::r>()?;
        input.parse::<Token![=]>()?;
        // while not a `[` or the start of heading
        while !input.peek(syn::token::Bracket) {
            req_def.push(request!(input)?);
            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        // POLICY DEF
        let heading;
        syn::bracketed!(heading in input);
        let mut _title: Heading;
        header!(_title in heading for kw::policy_definition);
        input.parse::<kw::p>()?;
        input.parse::<Token![=]>()?;
        // while not a `[` or the start of heading
        while !input.peek(syn::token::Bracket) {
            policy_def.push(policy!(input)?);
            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        // POLICY EFFECT
        let heading;
        syn::bracketed!(heading in input);
        let mut _title: Heading;
        header!(_title in heading for kw::policy_effect);
        input.parse::<kw::e>()?;
        input.parse::<Token![=]>()?;
        
        let policy_eft = if input.peek(Token![!]) {
            input.parse::<Token![!]>()?;
            let clause = parse_effect(&input)?;
            // TODO actually parse this
            PolicyEffect::NotSome(Condition {
                cond: policy!(clause)?,
                eft: Effect::Allow,
                when: When::Equal,
            })
        } else {
            let clause = parse_effect(&input)?;
            let cond = policy!(clause)?;

            let when = if clause.parse::<Token![==]>().is_ok() {
                When::Equal
            } else {
                When::NotEq
            };

            let eft = if clause.parse::<kw::allow>().is_ok() {
                Effect::Allow
            } else {
                Effect::Deny
            };

            // TODO actually parse this
            PolicyEffect::Some(Condition {
                cond,
                eft,
                when,
            })
        };
        let heading;
        syn::bracketed!(heading in input);
        let mut _title: Heading;
        header!(_title in heading for kw::matchers);
        input.parse::<kw::m>()?;
        input.parse::<Token![=]>()?;

        while let Ok(_) = peek_policy_type!(input) {
            let pair = input.parse::<CondPairs>()?;
            matchers.cond.push(pair);
            if input.peek(Token![&&]) {
                input.parse::<Token![&&]>()?;
            }
        }

        Ok(Self {req_def, policy_def, policy_eft, matchers })
    }
}

impl ToTokens for RequestDef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let req = match self {
            RequestDef::Subject => quote! { subject },
            RequestDef::Object => quote! { object },
            RequestDef::Action => quote! { action },
        };

        req.to_tokens(tokens);
    }
}

impl ToTokens for PolicyDef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let policy = match self {
            PolicyDef::Subject => quote! { subject },
            PolicyDef::Object => quote! { object },
            PolicyDef::Action => quote! { action },
            PolicyDef::Effect => quote! { effect },
        };

        policy.to_tokens(tokens);
    }
}

impl ToTokens for Condition {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let condition = match self.cond {
            PolicyDef::Effect => {
                if let When::Equal = self.when {
                    if let Effect::Allow = self.eft {
                        quote! { self.enforce(pol) { "allow" } else { "deny" } }
                    } else {
                        quote! { self.enforce(pol) { "deny" } else { "allow" } }
                    }
                } else {
                    todo!("no examples show this in casbin but do it")
                    // if let Effect::Allow = self.eft {
                    //     quote! {}
                    // } else {
                    //     quote! {}
                    // }
                }
            },
            _ => todo!("Condition is not finished parsing or tokenizing"),
        };
        condition.to_tokens(tokens);
    }
}

impl ToTokens for PolicyEffect {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let block = match self {
            Self::Some(cond) => quote! { if #cond },
            Self::NotSome(cond) => quote! { if !#cond },
        };
        
        let method = quote! {
            pub fn policy_effect(&self, pol: &str) -> &str {
                #block
            }
        };

        method.to_tokens(tokens);
    }
}

impl ToTokens for CondPairs {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let block = match self {
            // TODO if cond = When::NotEq use `!=`
            CondPairs::RequestPolicy {req, pol, .. } => quote! { req.#req },
            CondPairs::RequestRequest {req, req2, .. } => quote! { req.#req },
        };
        block.to_tokens(tokens);
    }
}

impl ToTokens for RawModel {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let req_fields = self.req_def.iter().map(|req| quote! { #req: String }).collect::<Vec<_>>();

        let request = quote! {
            #[derive(Clone, Debug)]
            pub struct Request {
                #(#req_fields,)*
            }
        };

        let policy_fields = self.policy_def.iter().map(|p| quote! { #p: String }).collect::<Vec<_>>();
        let policy = quote! {
            #[derive(Clone, Debug)]
            pub struct Policy {
                #( #policy_fields, )*
            }
        };

        let structs = quote! {
            #request
            #policy
            #[derive(Clone, Debug)]
            pub struct Model {
                pub req: Request,
                pub policy: Policy,
                pub users: std::collections::HashMap<String, Request>,
            }
        };

        let req_vals = self.req_def.iter().map(|req| match req {
            RequestDef::Action => quote! { "act" },
            RequestDef::Subject => quote! { "sub" },
            RequestDef::Object => quote! { "obj" },
        }).collect::<Vec<_>>();
        let req_new = self.req_def.iter().map(|req| quote! { #req: }).collect::<Vec<_>>();
        let pol_vals = self.policy_def.iter().map(|pol| match pol {
            PolicyDef::Action => quote! { "act" },
            PolicyDef::Subject => quote! { "sub" },
            PolicyDef::Object => quote! { "obj" },
            PolicyDef::Effect => quote! { "eft" },
        }).collect::<Vec<_>>();
        let policy_new = self.policy_def.iter().map(|pol| quote! { #pol: }).collect::<Vec<_>>();

        let count = self.req_def.iter().enumerate().map(|(i, _)| i).collect::<Vec<usize>>();

        let new = quote! {
            pub fn new() -> Self {
                Self {
                    req: Request {
                        #( #req_new #req_vals.into(), )*
                    },
                    policy: Policy {
                        #( #policy_new #pol_vals.into(), )*
                    },
                    users: std::collections::HashMap::default(),
                }
            }
        };

        let effect = &self.policy_eft;

        let matchers = &self.matchers.cond;
        let enforce = quote! {
            pub fn enforce(&self, pol: &str) -> bool {
                let pol = pol.split(",").map(|s| s.trim()).collect::<Vec<_>>();
                assert_eq!(REQ_LEN, pol.len());
                if let Some(req) = self.users.get(pol[0]) {
                    let mut res = true;
                    #( if !(#matchers == pol[#count]) { return false; } )*
                    res
                } else {
                    false
                }
            }
        };

        let req_fields_names = self.req_def.iter().map(|req| quote! { #req: }).collect::<Vec<_>>();
        let add_policy = quote! {
            pub fn add_policy(&mut self, policy: &str) {
                let pol = policy.split(",").map(|s| s.trim()).collect::<Vec<_>>();
                assert_eq!(REQ_LEN, pol.len());
                self.users.insert(pol[0].to_string(), Request {
                    #( #req_fields_names pol[#count].to_string(), )*
                });
            }
        };

        let impls = quote! {
            impl Model {
                #new
                #effect
                #enforce
                #add_policy
            }
        };

        let len = self.req_def.len();
        let api = quote! {
            const REQ_LEN: usize = #len;
            #structs
            #impls
        };

        api.to_tokens(tokens);
    }
}

fn make_cond_pair(heading: Heading, req: RequestDef, input: ParseStream) -> syn::Result<CondPairs> {
    match heading {
        Heading::Request => {
            if input.peek(Token![!]) {
                input.parse::<Token![!]>()?;
                input.parse::<Token![=]>()?;
                match policy_type!(input)? {
                    Heading::Request => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestRequest { req, req2: request!(input)?, cond: When::NotEq, })
                    },
                    Heading::Policy => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestRequest { req, req2: request!(input)?, cond: When::NotEq, })
                    },
                    _ => /* parse more variants */ todo!("impl more parsing"),
                }
            } else {
                input.parse::<Token![=]>()?;
                input.parse::<Token![=]>()?;
                match policy_type!(input)? {
                    Heading::Request => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestRequest { req, req2: request!(input)?, cond: When::Equal, })
                    },
                    Heading::Policy => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestRequest { req, req2: request!(input)?, cond: When::Equal, })
                    },
                    _ => /* parse more variants */ todo!("impl more parsing"),
                }
            }
        },
        Heading::Policy => {
            if input.peek(Token![!]) {
                input.parse::<Token![!]>()?;
                input.parse::<Token![=]>()?;
                match policy_type!(input)? {
                    Heading::Request => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestPolicy { req, pol: policy!(input)?, cond: When::NotEq, })
                    },
                    Heading::Policy => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestPolicy { req, pol: policy!(input)?, cond: When::NotEq, })
                    },
                    _ => /* parse more variants */ todo!("impl more parsing"),
                }
            } else {
                input.parse::<Token![=]>()?;
                input.parse::<Token![=]>()?;
                match policy_type!(input)? {
                    Heading::Request => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestPolicy { req, pol: policy!(input)?, cond: When::Equal, })
                    },
                    Heading::Policy => {
                        input.parse::<Token![.]>()?;
                        Ok(CondPairs::RequestPolicy { req, pol: policy!(input)?, cond: When::Equal, })
                    },
                    _ => /* parse more variants */ todo!("impl more parsing"),
                }
            }
        },
        _ => /* parse more variants */ todo!("impl more parsing"),
    }
}
