use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;

use serde::{Serialize, Deserialize};

mod parser;
pub use parser::ParserError;

pub trait Identifier:
    Clone + PartialEq + Eq + PartialOrd + Ord + Hash
{}

impl<T> Identifier for T
    where T: Clone + PartialEq + Eq + PartialOrd + Ord + Hash
{}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Empty { }

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum BinaryOp {
    And, Or, Impl, Equiv
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum BoolExp<T: Identifier = String> {
    Id(T),
    Const(bool),
    Not(Box<BoolExp<T>>),
    Binary(BinaryOp, Box<BoolExp<T>>, Box<BoolExp<T>>)
}

impl BinaryOp {
    pub fn prec(self) -> (u16, bool) {
        match self {
            BinaryOp::Equiv =>
                (0, true),
            BinaryOp::Impl =>
                (1, false),
            BinaryOp::Or =>
                (2, true),
            BinaryOp::And =>
                (3, true)
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

impl<T: Identifier> BoolExp<T> {
    pub fn map_vars<U, F>(&self, f: &F) -> BoolExp<U>
        where U: Identifier, F: Fn(&T) -> U
    {
        match self {
            BoolExp::Id(id) =>
                BoolExp::Id(f(id)),
            BoolExp::Const(b) =>
                BoolExp::Const(*b),
            BoolExp::Not(rhs) =>
                BoolExp::Not(Box::new(rhs.map_vars(f))),
            BoolExp::Binary(op, lhs, rhs) =>
                BoolExp::Binary(*op, Box::new(lhs.map_vars(f)), Box::new(rhs.map_vars(f)))
        }
    }

    pub fn eval<F>(&self, f: &F) -> bool
        where F: Fn(&T) -> bool
    {
        match self {
            BoolExp::Id(id) =>
                f(id),
            BoolExp::Const(b) =>
                *b,
            BoolExp::Not(rhs) =>
                !rhs.eval(f),
            BoolExp::Binary(op, lhs, rhs) => {
                let l = lhs.eval(f);
                let r = rhs.eval(f);
                match op {
                    BinaryOp::And => l & r,
                    BinaryOp::Or => l || r,
                    BinaryOp::Impl => !l || r,
                    BinaryOp::Equiv => l == r
                }
            }
        }
    }

    pub fn vars(&self) -> HashSet<T> {
        let mut res = HashSet::new();
        self._vars(&mut res);
        res
    }

    fn _vars(&self, res: &mut HashSet<T>) {
        match self {
            BoolExp::Id(id) => {
                res.insert(id.clone());
            },
            BoolExp::Const(_b) => {},
            BoolExp::Not(rhs) => {
                rhs._vars(res);
            },
            BoolExp::Binary(_op, lhs, rhs) => {
                lhs._vars(res);
                rhs._vars(res);
            }
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::And =>
                write!(f, "&"),
            BinaryOp::Or =>
                write!(f, "|"),
            BinaryOp::Impl =>
                write!(f, "->"),
            BinaryOp::Equiv =>
                write!(f, "<->")
        }
    }
}

impl<T: Identifier + fmt::Display> fmt::Display for BoolExp<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BoolExp::Id(id) =>
                write!(f, "{}", id),
            BoolExp::Const(true) =>
                write!(f, "True"),
            BoolExp::Const(false) =>
                write!(f, "False"),
            BoolExp::Not(rhs) =>
                write!(f, "!{})", rhs),
            BoolExp::Binary(op, lhs, rhs) =>
                write!(f, "({} {} {})", lhs, op, rhs)
        }
    }
}

pub fn parse(s: &str) -> Result<BoolExp, ParserError> {
    parser::Parser::new(s.chars()).parse()
}
