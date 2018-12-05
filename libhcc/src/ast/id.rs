use std::collections::HashMap;

pub type Id = usize;

#[derive(Clone)]
pub struct IdStore {
    ids: Vec<String>,
    map: HashMap<String, usize>,
}

impl IdStore {
    pub fn new() -> IdStore {
        IdStore {
            ids: vec![],
            map: HashMap::new(),
        }
    }

    pub fn get_id<S: Into<String>>(&mut self, s: S) -> Id {
        let s = s.into();
        if self.map.contains_key(&s) {
            self.map[&s]
        } else {
            let id = self.ids.len();
            self.ids.push(s.clone());
            self.map.insert(s, id);
            id
        }
    }

    pub fn get_string(&self, id: Id) -> Option<String> {
        self.ids.get(id).map(|s| s.clone())
    }

    pub fn get_str<'b>(&'b self, id: Id) -> &'b str {
        &self.ids[id][..]
    }
}
