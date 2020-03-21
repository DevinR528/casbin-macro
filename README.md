# Using
`cargo run --example parse`
`cargo expand --example visit`

```rust
use casbin_macro::parse_model;

parse_model! {
    [request_definition]
    r = sub, obj, act

    [policy_definition]
    p = sub, obj, act

    [policy_effect]
    e = some(where (p.eft == allow))

    [matchers]
    m = r.sub == p.sub && r.obj == p.obj && r.act == p.act
}

fn main() {
    let mut model = Model::new();
    model.add_policy("me, data, read");

    println!("{}", model.enforce("me, data, read"));
}
```
