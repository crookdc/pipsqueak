use crate::eval::Object;

type Builtin = (String, fn(Vec<Object>) -> Object);

pub fn all() -> Vec<Builtin> {
    vec![("len".to_string(), len), ("print".to_string(), print)]
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

pub fn print(args: Vec<Object>) -> Object {
    args.into_iter().for_each(|arg| match arg {
        Object::Nil => print!("nil"),
        Object::Integer(value) => print!("{}", value),
        Object::String(value) => print!("{}", value),
        Object::Boolean(value) => print!("{}", value),
        _ => {}
    });
    println!();
    Object::Nil
}
