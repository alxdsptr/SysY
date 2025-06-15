use std::collections::HashMap;

pub struct LabelGenerator{
    label_map: HashMap<String, usize>,

}
impl LabelGenerator {
    pub fn new() -> Self {
        LabelGenerator {
            label_map: HashMap::new(),
        }
    }

    pub fn get_label(&mut self, name: &str) -> String {
        let count = self.label_map.entry(name.to_string()).or_insert(0);
        *count += 1;
        format!("%{}_{}", name, count)
    }
}