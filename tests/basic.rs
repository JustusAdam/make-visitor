extern crate make_visitor;

use std::collections::HashSet;

make_visitor::make_visitor! {
    #[derive(Clone)]
    pub struct Name(&'static str);
    pub struct Value(usize);
    pub struct Binding {
        name: Name,
        value: Value,
        hidden: usize,
        arr: [Name; 2],
        slc: &'static [Name],
        tup: (Name, Value),
        bx: Box<Name>,
        vec: Vec<Name>,
    }
}

#[test]
fn test_binding() {
    use visit::*;
    struct V(Vec<String>);

    impl Visitor for V {
        fn visit_name(&mut self, name: &Name) {
            self.0.push(name.0.to_string())
        }
        fn visit_value(&mut self, value: &Value) {
            self.0.push(value.0.to_string())
        }
    }

    let mut v = V(vec![]);
    v.visit_binding(&Binding {
        name: Name("Name"),
        value: Value(20),
        hidden: 5,
        arr: [Name("Arr"), Name("Two")],
        slc: &[Name("Slice")],
        tup: (Name("from tuple"), Value(8)),
        bx: Box::new(Name("Box")),
        vec: vec![Name("vec")],
    });

    assert_eq!(
        v.0,
        [
            "Name",
            "20",
            "Arr",
            "Two",
            "Slice",
            "from tuple",
            "8",
            "Box",
            "vec"
        ]
    );
    Name("").clone();
}
