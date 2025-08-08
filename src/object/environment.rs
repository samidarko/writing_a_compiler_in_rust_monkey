use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Env>,
}

pub type Env = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> Env {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed_environment(outer: Env) -> Env {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: Some(outer),
        }))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store
            .get(name)
            .cloned()
            .or_else(|| self.outer.as_ref()?.borrow().get(name))
    }

    pub fn set(&mut self, name: &str, object: &Object) {
        self.store.insert(name.to_string(), object.clone());
    }
}

#[cfg(test)]
mod tests {
    use crate::object::environment::Environment;
    use crate::object::Object;
    use std::result;

    pub type Result<T> = result::Result<T, String>;

    #[test]
    fn environment_found() -> Result<()> {
        let environment = Environment::new();
        let name = "name".to_string();

        environment.borrow_mut().set(&name, &Object::Null);

        let object = environment.borrow().get(&name).unwrap();
        assert_eq!(object, Object::Null);

        Ok(())
    }

    #[test]
    fn environment_not_found() -> Result<()> {
        let environment = Environment::new();
        let name = "name".to_string();

        assert_eq!(environment.borrow_mut().get(&name), None);

        Ok(())
    }

    #[test]
    fn enclosed_environment_found() -> Result<()> {
        let outer = Environment::new();
        let name = "name".to_string();
        outer.borrow_mut().set(&name, &Object::Null);

        let environment = Environment::new_enclosed_environment(outer);

        // test outer environment
        let object = environment.borrow().get(&name).unwrap();
        assert_eq!(object, Object::Null);

        // test current environment
        environment.borrow_mut().set(&name, &Object::Int(1));
        let object = environment.borrow().get(&name).unwrap();
        assert_eq!(object, Object::Int(1));

        let result = environment.borrow().get("unknown");
        assert!(result.is_none());

        Ok(())
    }

    #[test]
    fn enclosed_environment_not_found() -> Result<()> {
        let outer = Environment::new();
        let environment = Environment::new_enclosed_environment(outer);
        let name = "name".to_string();

        assert_eq!(environment.borrow().get(&name), None);

        Ok(())
    }
}
