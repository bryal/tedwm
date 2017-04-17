use sequence_trie::{SequenceTrie, TrieKey};

pub struct SequenceSet<K>(SequenceTrie<K, ()>) where K: TrieKey;

#[derive(Debug)]
pub enum Entry<'s, K: 's> {
    Some(Vec<&'s K>),
    NotUnique(Vec<Vec<&'s K>>),
    None,
}

#[derive(Debug)]
pub enum StringEntry {
    Some(String),
    NotUnique(Vec<String>),
    None,
}

impl<K> SequenceSet<K>
    where K: TrieKey
{
    pub fn new() -> Self {
        SequenceSet(SequenceTrie::new())
    }
    pub fn insert<'k, I>(&'k mut self, key: I)
        where I: IntoIterator<Item = &'k K>
    {
        self.0.insert(key, ());
    }

    pub fn contains<'k>(&'k self, key: &'k [K]) -> Entry<'k, K> {
        self.0
            .get_node(key)
            .map(|n| {
            let mut ks = n.keys().peekable();
            let first = ks.next();
            if ks.peek().is_some() {
                Entry::NotUnique(first.into_iter()
                                      .chain(ks)
                                      .map(|tail| key.iter().chain(tail).collect())
                                      .collect())
            } else {
                first.map(|s| Entry::Some(key.iter().chain(s).collect())).unwrap_or(Entry::None)
            }
        })
            .unwrap_or(Entry::None)
    }
}

impl SequenceSet<char> {
    pub fn insert_str(&mut self, key: &str) {
        self.insert(&key.chars().collect::<Vec<_>>());
    }

    pub fn contains_str<'k>(&self, key: &str) -> StringEntry {
        match self.contains(&key.chars().collect::<Vec<_>>()) {
            Entry::Some(cs) => StringEntry::Some(cs.iter().cloned().collect()),
            Entry::NotUnique(matches) => StringEntry::NotUnique(matches.into_iter()
                                                                       .map(|cs| {
                                                                           cs.into_iter().collect()
                                                                       })
                                                                       .collect()),
            Entry::None => StringEntry::None,
        }
    }
}
