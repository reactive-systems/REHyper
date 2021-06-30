use std::collections::HashMap;
use std::io::{Write as _, BufReader, BufWriter};
use std::path::Path;

use super::*;
use super::arena::{Arena, NodeKind};

pub struct ReactiveParEnforcer {
    dict: LTLDict,
    //n_forall: usize,
    //ltl: LTLAst<usize>,
    inputs: HashSet<usize>,

    arena: Arena,
    state: usize,
    enforcing: bool
}

impl ReactiveParEnforcer {
    /*pub fn new(formula: &HyperLTLAst, inputs: &[String], n: usize) -> Result<ReactiveParEnforcer, String> {
        Self::new_using_cache(formula, inputs, n, None)
    }*/

    pub fn new_using_cache(formula: &HyperLTLAst, inputs: &[String], n: usize, path: Option<&Path>) -> Result<ReactiveParEnforcer, String> {
        let (dict, n_forall, ltl) = extract_formula(formula)?;

        let inputs: HashSet<_> = inputs.iter().flat_map(|ap| dict.ap.lookup(ap)).collect();
        let outputs: HashSet<_> = (0..dict.ap.size())
            .filter(|idx| !inputs.contains(idx))
            .collect();

        let (cache_key, arena) =
            if let Some(path) = path {
                let mut key = format!("{};", formula);
                let mut inputs_sorted: Vec<_> = inputs.iter().collect();
                inputs_sorted.sort();
                for (i, next) in inputs_sorted.iter().enumerate() {
                    if i > 0 {
                        key += ",";
                    }
                    write!(key, "{}", next).unwrap();
                }
                write!(key, ";{}", n).unwrap();

                if !path.is_file() {
                    let mut file = File::create(&path)
                        .map_err(|_| format!("could not create file: {:?}", path))?;
                    let cache = HashMap::new();
                    bincode::serialize_into(BufWriter::new(&mut file), &cache)
                        .map_err(|e| format!("{}", e))?;
                    file.flush()
                        .map_err(|e| format!("{}", e))?;
                    (Some((cache, key)), None)
                } else {
                    let mut file = File::open(&path)
                        .map_err(|_| format!("could not open file: {:?}", path))?;
                    let mut cache: HashMap<String, Arena> =
                        bincode::deserialize_from(BufReader::new(&mut file))
                            .map_err(|e| format!("{}", e))?;
                    let arena = cache.remove(&key);
                    if arena.is_some() {
                        debug!("arena found in cache");
                    }
                    (Some((cache, key)), arena)
                }
            } else {
                (None, None)
            };

        let arena =
            match arena {
                Some(arena) => arena,
                None => {
                    debug!("computing parity game");
                    let mut arena = Arena::from_strix(n_forall, &ltl, n, &inputs, &outputs)?;
                    debug!("solving parity game");
                    arena.solve();
                    debug!("done");

                    if let (Some(path), Some((mut cache, key))) = (path, cache_key) {
                        debug!("storing arena in cache");
                        cache.insert(key.clone(), arena);
                        let mut file = File::create(&path)
                            .map_err(|_| format!("could not open file: {:?}", path))?;
                        bincode::serialize_into(BufWriter::new(&mut file), &cache)
                            .map_err(|e| format!("{}", e))?;
                        file.flush()
                            .map_err(|e| format!("{}", e))?;

                        cache.remove(&key).unwrap()
                    } else {
                        arena
                    }
                }
            };

        if log_enabled!(log::Level::Trace) {
            trace!("arena:");
            for (i, next) in arena.nodes.iter().enumerate() {
                trace!("    {}: {:?}", i, next);
            }
        }

        Ok(ReactiveParEnforcer {
            dict,
            //n_forall,
            //ltl,
            inputs,

            arena,
            state: 0,
            enforcing: false
        })
    }
}

impl ParEnforcer for ReactiveParEnforcer {
    fn enforce(&mut self, evts: &[Event]) -> Option<Vec<Event>> {
        let evts_c: Vec<_> = evts.iter().map(|e| e.compress(&self.dict)).collect();
        let mut node = &self.arena.nodes[self.state];

        if matches!(&node.kind, NodeKind::Top) {
            return None;
        }
        assert!(matches!(&node.kind, NodeKind::Env(_)));
        while let NodeKind::Env(edges) = &node.kind {
            assert!(node.winning.is_none());
            let (succ, _) = edges.iter()
                .find(|(_, exp)|
                    exp.eval(&|(ap, tr)|
                        evts_c[*tr].contains(ap)
                    )
                )
                .unwrap();
            node = &self.arena.nodes[*succ];
        }

        if self.enforcing {
            assert!(matches!(&node.kind, NodeKind::Sys(_) | NodeKind::Top));
            let mut assign = HashMap::new();
            while let NodeKind::Sys(_) = &node.kind {
                if let Some((succ, a)) = &node.winning {
                    node = &self.arena.nodes[*succ];
                    for (ap_tr, val) in a.iter() {
                        assert!(!assign.contains_key(ap_tr));
                        assign.insert(ap_tr, val);
                    }
                } else {
                    unreachable!();
                }
            }
            assert!(matches!(&node.kind, NodeKind::Env(_) | NodeKind::Top));

            let mut evts: Vec<_> = evts_c.iter()
                .map(|evt| {
                    let mut res = Event::new();
                    for next in evt.aps() {
                        if self.inputs.contains(next) {
                            res.add(*next);
                        }
                    }
                    res
                })
                .collect();

            for ((ap, tr), val) in assign {
                if *val {
                    evts[*tr].add(*ap);
                }
            }

            self.state = node.idx;
            Some(evts.into_iter()
                .map(|evt| Event::uncompress(&evt, &self.dict))
                .collect())

        } else {
            if matches!(&node.kind, NodeKind::Top) {
                return None;
            }
            assert!(matches!(&node.kind, NodeKind::Sys(_)));
            while let NodeKind::Sys(edges) = &node.kind {
                let next = edges.iter()
                    .find(|(_, exp)|
                        exp.eval(&|(ap, tr)|
                            evts_c[*tr].contains(ap)
                        )
                    );
                if let Some((succ, _)) = next {
                    node = &self.arena.nodes[*succ];
                } else {
                    break;
                }
            }

            match &node.kind {
                NodeKind::Env(_) => {
                    if node.winning.is_none() {
                        self.state = node.idx;
                        return None;
                    }
                    //violation here
                },

                NodeKind::Top => {
                    self.state = node.idx;
                    return None;
                },

                NodeKind::Sys(_) | NodeKind::Bottom => {
                    //violation here
                }
            }

            info!("violation detected, enforcing now [evt: enforcing]");
            self.enforcing = true;
            self.enforce(evts)
        }
    }

    fn finish(&mut self) -> Option<Vec<Event>> {
        None
    }
}
