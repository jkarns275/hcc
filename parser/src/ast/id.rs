use std::collections::HashMap;

pub type Id = usize;

pub struct IdStore<'a> {
    ids: Vec<&'a str>,
    map: HashMap<&'a str, usize>,
}

impl<'a> IdStore<'a> {
    pub fn get_id(&mut self, s: &'a str) -> Id {
        if self.map.contains_key(s) {
            self.map[s]
        } else {
            let id = self.ids.len();
            self.ids.push(s);
            self.map.insert(s, self.ids.len());
            id
        }
    }

    pub fn get_string(&self, id: Id) -> Option<String> {
        self.ids.get(id).map(|s| s.to_string())
    }
}