use std::collections::HashMap;

pub type Id = usize;

#[derive(Clone)]
pub struct IdStore<'a> {
    ids: Vec<&'a str>,
    map: HashMap<&'a str, usize>,
}

impl<'a> IdStore<'a> {
    pub fn new<'r>() -> IdStore<'r> {
        IdStore {
            ids: vec![],
            map: HashMap::new(),
        }
    }

    pub fn get_id(&mut self, s: &'a str) -> Id {
        if self.map.contains_key(s) {
            let id = self.map[s];
            id
        } else {
            let id = self.ids.len();
            self.ids.push(s);
            self.map.insert(s, id);
            id
        }
    }

    pub fn get_string(&self, id: Id) -> Option<String> {
        self.ids.get(id).map(|s| s.to_string())
    }
}