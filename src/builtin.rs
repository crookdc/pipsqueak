use crate::eval::Object;

type Builtin = (String, fn(Vec<Object>) -> Object);

pub fn all() -> Vec<Builtin> {
    vec![("len".to_string(), len)]
}

pub fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        panic!(
            "incorrect number of arguments when calling len {}",
            args.len()
        );
    }
    match args.get(0).unwrap() {
        Object::String(value) => Object::Integer(value.len() as i32),
        other => panic!("incorrect value passed to len: {other:?}"),
    }
}
