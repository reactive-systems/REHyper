use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, hash_map::Entry};
use std::fmt;
use std::hash::Hash;

mod parser;

pub use parser::ParserError;

pub trait Identifier:
    Clone + PartialEq + Eq + PartialOrd + Ord + Hash
{}

impl<T> Identifier for T
    where T: Clone + PartialEq + Eq + PartialOrd + Ord + Hash
{}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Empty { }

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LTLUnaryOp {
    Not, Next, Finally, Globally
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LTLBinaryOp {
    And, Xor, Or, Impl, Equiv, Until, Weak, Release
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LTLAst<A: Identifier = String, T: Identifier = A> {
    AP(A, T),
    Const(bool),
    Unary(LTLUnaryOp, Box<LTLAst<A, T>>),
    Binary(LTLBinaryOp, Box<LTLAst<A, T>>, Box<LTLAst<A, T>>)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HyperLTLAst<A: Identifier = String, T: Identifier = A> {
    LTL(LTLAst<A, T>),
    Forall(T, Box<HyperLTLAst<A, T>>),
    Exists(T, Box<HyperLTLAst<A, T>>)
}

impl LTLBinaryOp {
    pub fn prec(self) -> (u16, bool) {
        match self {
            LTLBinaryOp::Until | LTLBinaryOp::Weak | LTLBinaryOp::Release =>
                (0, false),
            LTLBinaryOp::Equiv =>
                (1, true),
            LTLBinaryOp::Impl =>
                (2, false),
            LTLBinaryOp::Or =>
                (3, true),
            LTLBinaryOp::Xor =>
                (4, true),
            LTLBinaryOp::And =>
                (5, true)
        }
    }

    pub fn prec_pair(self) -> (u16, u16) {
        let (prec, lassoc) = self.prec();
        if lassoc {
            (2 * prec, 2 * prec + 1)
        } else {
            (2 * prec, 2 * prec)
        }
    }
}

#[derive(Debug, Clone)]
pub struct LTLDict<A: Identifier = String, T: Identifier = A> {
    pub ap: Dict<A>,
    pub tr: Dict<T>
}

impl<A: Identifier, T: Identifier> LTLDict<A, T> {
    pub fn new() -> LTLDict<A, T> {
        LTLDict {
            ap: Dict::new(),
            tr: Dict::new()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Dict<ID: Identifier = String> {
    id_to_index: HashMap<ID, usize>,
    index_to_id: Vec<ID>
}

impl<ID: Identifier> Dict<ID> {
    pub fn new() -> Dict<ID> {
        Dict {
            id_to_index: HashMap::new(),
            index_to_id: Vec::new()
        }
    }

    pub fn lookup_insert(&mut self, id: &ID) -> usize {
        match self.id_to_index.entry(id.clone()) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(v) => {
                let res = self.index_to_id.len();
                self.index_to_id.push(id.clone());
                v.insert(res);
                res
            }
        }
    }

    pub fn lookup<Q: ?Sized>(&self, id: &Q) -> Option<usize>
        where ID: Borrow<Q>, Q: Hash + Eq
    {
        self.id_to_index.get(id).cloned()
    }

    pub fn get(&self, i: usize) -> Option<&ID> {
        self.index_to_id.get(i)
    }

    pub fn size(&self) -> usize {
        self.index_to_id.len()
    }

    pub fn id_to_index(&self, id: &ID) -> usize {
        *self.id_to_index.get(id).unwrap()
    }

    pub fn index_to_id(&self, i: usize) -> &ID {
        &self.index_to_id[i]
    }
}




impl<A: Identifier, T: Identifier> LTLAst<A, T> {
    pub fn is_closed_under(&self, vars: &HashSet<T>) -> bool {
        match self {
            LTLAst::AP(_, t) =>
                vars.contains(t),
            LTLAst::Const(_) =>
                true,
            LTLAst::Unary(_, rhs) =>
                rhs.is_closed_under(vars),
            LTLAst::Binary(_, lhs, rhs) =>
                lhs.is_closed_under(vars) && rhs.is_closed_under(vars)
        }
    }

    pub fn compress(&self, dict: &mut LTLDict<A, T>) -> LTLAst<usize> {
        match self {
            LTLAst::AP(ap, tr) =>
                LTLAst::AP(dict.ap.lookup_insert(ap), dict.tr.lookup_insert(tr)),
            LTLAst::Const(b) =>
                LTLAst::Const(*b),
            LTLAst::Unary(op, rhs) =>
                LTLAst::Unary(*op, Box::new(rhs.compress(dict))),
            LTLAst::Binary(op, lhs, rhs) =>
                LTLAst::Binary(*op, Box::new(lhs.compress(dict)), Box::new(rhs.compress(dict)))
        }
    }
}

impl<A: Identifier, T: Identifier> HyperLTLAst<A, T> {
    pub fn is_universal(&self) -> bool {
        match self {
            HyperLTLAst::LTL(_ltl) =>
                true,
            HyperLTLAst::Forall(_tr, rhs) =>
                rhs.is_universal(),
            HyperLTLAst::Exists(_tr, _rhs) =>
                false
        }
    }

    pub fn is_existential(&self) -> bool {
        match self {
            HyperLTLAst::LTL(_ltl) =>
                true,
            HyperLTLAst::Forall(_tr, _rhs) =>
                false,
            HyperLTLAst::Exists(_tr, rhs) =>
                rhs.is_existential()
        }
    }

    pub fn is_closed(&self) -> bool {
        let mut vars = HashSet::new();
        let mut ast = self;
        loop {
            match ast {
                HyperLTLAst::LTL(ltl) =>
                    return ltl.is_closed_under(&vars),
                HyperLTLAst::Forall(tr, rhs)
              | HyperLTLAst::Exists(tr, rhs) => {
                    vars.insert(tr.clone());
                    ast = rhs;
                }
            }
        }
    }

    pub fn compress(&self, dict: &mut LTLDict<A, T>) -> HyperLTLAst<usize> {
        match self {
            HyperLTLAst::LTL(ltl) =>
                HyperLTLAst::LTL(ltl.compress(dict)),
            HyperLTLAst::Forall(tr, rhs) =>
                HyperLTLAst::Forall(dict.tr.lookup_insert(tr), Box::new(rhs.compress(dict))),
            HyperLTLAst::Exists(tr, rhs) =>
                HyperLTLAst::Exists(dict.tr.lookup_insert(tr), Box::new(rhs.compress(dict)))
        }
    }
}



impl fmt::Display for LTLUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LTLUnaryOp::Not =>
                write!(f, "!"),
            LTLUnaryOp::Next =>
                write!(f, "X"),
            LTLUnaryOp::Finally =>
                write!(f, "F"),
            LTLUnaryOp::Globally =>
                write!(f, "G")
        }
    }
}

impl fmt::Display for LTLBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LTLBinaryOp::And =>
                write!(f, "&"),
            LTLBinaryOp::Xor =>
                write!(f, "^"),
            LTLBinaryOp::Or =>
                write!(f, "|"),
            LTLBinaryOp::Impl =>
                write!(f, "->"),
            LTLBinaryOp::Equiv =>
                write!(f, "<->"),
            LTLBinaryOp::Until =>
                write!(f, "U"),
            LTLBinaryOp::Weak =>
                write!(f, "W"),
            LTLBinaryOp::Release =>
                write!(f, "R")
        }
    }
}

impl<A: Identifier + fmt::Display, T: Identifier + fmt::Display> fmt::Display for LTLAst<A, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LTLAst::AP(ap, idx) =>
                write!(f, "{}_{}", ap, idx),
            LTLAst::Const(true) =>
                write!(f, "true"),
            LTLAst::Const(false) =>
                write!(f, "false"),
            LTLAst::Unary(op, rhs) =>
                write!(f, "({} {})", op, rhs),
            LTLAst::Binary(op, lhs, rhs) =>
                write!(f, "({} {} {})", lhs, op, rhs)
        }
    }
}

impl<A: Identifier + fmt::Display> fmt::Display for LTLAst<A, Empty> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LTLAst::AP(ap, _) =>
                write!(f, "{}", ap),
            LTLAst::Const(true) =>
                write!(f, "true"),
            LTLAst::Const(false) =>
                write!(f, "false"),
            LTLAst::Unary(op, rhs) =>
                write!(f, "({} {})", op, rhs),
            LTLAst::Binary(op, lhs, rhs) =>
                write!(f, "({} {} {})", lhs, op, rhs)
        }
    }
}

impl<A: Identifier + fmt::Display, T: Identifier + fmt::Display> fmt::Display for HyperLTLAst<A, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            HyperLTLAst::LTL(ltl) =>
                write!(f, "{}", ltl),
            HyperLTLAst::Forall(id, rhs) =>
                write!(f, "forall {}. {}", id, rhs),
            HyperLTLAst::Exists(id, rhs) =>
                write!(f, "exists {}. {}", id, rhs)
        }
    }
}

pub fn parse_hyperltl(s: &str) -> Result<HyperLTLAst, ParserError> {
    parser::Parser::new(s.chars()).parse()
}
