//! String interning for frequently used identifiers and keywords.
//!
//! This module provides string interning to reduce memory usage for frequently
//! used identifiers like keywords, built-in function names, and common variable names.

use std::collections::HashMap;
use std::sync::{Arc, LazyLock, Mutex};

/// Global string interner for frequently used strings.
static STRING_INTERNER: LazyLock<Mutex<StringInterner>> = LazyLock::new(|| {
    let mut interner = StringInterner::new();

    // Pre-intern common keywords and identifiers
    interner.intern_static("let");
    interner.intern_static("fn");
    interner.intern_static("if");
    interner.intern_static("else");
    interner.intern_static("return");
    interner.intern_static("true");
    interner.intern_static("false");
    interner.intern_static("null");
    interner.intern_static("while");
    interner.intern_static("for");
    interner.intern_static("in");

    // Pre-intern common builtin function names
    interner.intern_static("len");
    interner.intern_static("first");
    interner.intern_static("last");
    interner.intern_static("rest");
    interner.intern_static("push");
    interner.intern_static("puts");
    interner.intern_static("exit");
    interner.intern_static("int");
    interner.intern_static("string");

    // Pre-intern common variable names
    interner.intern_static("x");
    interner.intern_static("y");
    interner.intern_static("z");
    interner.intern_static("i");
    interner.intern_static("j");
    interner.intern_static("k");
    interner.intern_static("n");
    interner.intern_static("result");
    interner.intern_static("value");
    interner.intern_static("name");
    interner.intern_static("age");
    interner.intern_static("count");

    Mutex::new(interner)
});

/// String interner that deduplicates strings to save memory.
struct StringInterner {
    strings: HashMap<String, Arc<str>>,
}

impl StringInterner {
    /// Create a new string interner.
    fn new() -> Self {
        Self {
            strings: HashMap::new(),
        }
    }

    /// Intern a static string (known at compile time).
    fn intern_static(&mut self, s: &'static str) -> Arc<str> {
        if let Some(interned) = self.strings.get(s) {
            interned.clone()
        } else {
            let boxed: Arc<str> = s.into();
            self.strings.insert(s.to_string(), boxed.clone());
            boxed
        }
    }

    /// Intern a dynamic string.
    fn intern(&mut self, s: String) -> Arc<str> {
        if let Some(interned) = self.strings.get(&s) {
            interned.clone()
        } else {
            let boxed: Arc<str> = s.clone().into();
            self.strings.insert(s, boxed.clone());
            boxed
        }
    }
}

/// Intern a string to reduce memory usage for frequently used strings.
pub fn intern_string(s: String) -> Arc<str> {
    let mut interner = STRING_INTERNER.lock().unwrap();
    interner.intern(s)
}

/// Intern a static string to reduce memory usage.
pub fn intern_static(s: &'static str) -> Arc<str> {
    let mut interner = STRING_INTERNER.lock().unwrap();
    interner.intern_static(s)
}

/// Check if a string should be interned based on common patterns.
pub fn should_intern(s: &str) -> bool {
    // Intern short strings (likely to be repeated)
    if s.len() <= 10 {
        return true;
    }

    // Intern strings that match common patterns
    s.starts_with("temp") ||
    s.starts_with("var") ||
    s.starts_with("item") ||
    s.starts_with("elem") ||
    s.starts_with("key") ||
    s.starts_with("val") ||
    // Single letters (very common)
    (s.len() == 1 && s.chars().next().unwrap().is_ascii_lowercase())
}

/// Optimized string creation that uses interning for frequently used strings.
pub fn create_optimized_string(s: String) -> Box<str> {
    if should_intern(&s) {
        intern_string(s).as_ref().into()
    } else {
        s.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_interning() {
        let s1 = intern_string("test".to_string());
        let s2 = intern_string("test".to_string());

        // Should be the same instance (memory location)
        assert_eq!(s1.as_ptr(), s2.as_ptr());
    }

    #[test]
    fn test_static_interning() {
        let s1 = intern_static("let");
        let s2 = intern_static("let");

        // Should be the same instance
        assert_eq!(s1.as_ptr(), s2.as_ptr());
    }

    #[test]
    fn test_should_intern() {
        assert!(should_intern("x"));
        assert!(should_intern("temp1"));
        assert!(should_intern("var"));
        assert!(should_intern("key"));
        assert!(!should_intern(
            "very_long_variable_name_that_should_not_be_interned"
        ));
    }
}
