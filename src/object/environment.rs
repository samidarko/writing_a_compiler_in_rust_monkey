use crate::object::Object;
use std::collections::HashMap;
use std::result;

pub type Result<T> = result::Result<T, String>;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Self) -> Self {
        let mut env = Self::new();
        env.outer = Some(Box::new(outer));
        env
    }

    pub fn get(&self, name: &str) -> Result<Object> {
        match (self.store.get(name), &self.outer) {
            (Some(object), _) => Ok(object.clone()),
            (None, Some(outer)) => outer.get(name),
            (None, _) => Err(format!("identifier not found: {}", &name)),
        }
    }

    pub fn set(&mut self, name: &str, object: Object) -> Object {
        self.store.insert(name.to_string(), object);
        self.get(name).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::object::environment::Environment;
    use crate::object::environment::Result;
    use crate::object::Object;

    #[test]
    fn environment_found() -> Result<()> {
        let mut environment = Environment::new();
        let name = "name".to_string();

        environment.set(&name, Object::Null);

        let object = environment.get(&name)?;
        assert_eq!(object, Object::Null);

        Ok(())
    }

    #[test]
    fn environment_not_found() -> Result<()> {
        let environment = Environment::new();
        let name = "name".to_string();

        assert_eq!(
            environment.get(&name),
            Err("identifier not found: name".to_string())
        );

        Ok(())
    }

    #[test]
    fn enclosed_environment_found() -> Result<()> {
        let mut outer = Environment::new();
        let name = "name".to_string();
        outer.set(&name, Object::Null);

        let mut environment = Environment::new_enclosed_environment(outer);

        // test outer environment
        let object = environment.get(&name)?;
        assert_eq!(object, Object::Null);

        // test current environment
        environment.set(&name, Object::Int(1));
        let object = environment.get(&name)?;
        assert_eq!(object, Object::Int(1));

        let result = environment.get("unknown");
        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn enclosed_environment_not_found() -> Result<()> {
        let outer = Environment::new();
        let environment = Environment::new_enclosed_environment(outer);
        let name = "name".to_string();

        assert_eq!(
            environment.get(&name),
            Err("identifier not found: name".to_string())
        );

        Ok(())
    }
}
