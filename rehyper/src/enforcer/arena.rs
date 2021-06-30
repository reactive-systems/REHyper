use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write as _};
use std::io::{Read, Write as _, BufRead, BufReader};
use std::process::{Command, Stdio};

use boolexp::BoolExp;
use hyperltl::LTLAst;
use lazy_static::lazy_static;
use log::trace;
use regex::Regex;
use serde::{Serialize, Deserialize};
use varisat::{CnfFormula, ExtendFormula};

use crate::{find_executable, tseitin};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Arena {
    game_str: String,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node {
    pub idx: usize,
    pub color: u64,
    pub kind: NodeKind,
    pub winning: Option<(usize, HashMap<(usize, usize), bool>)>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NodeKind {
    Env(HashMap<usize, BoolExp<(usize, usize)>>),
    Sys(HashMap<usize, BoolExp<(usize, usize)>>),
    Top,
    Bottom
}

impl Arena {
    pub fn from_strix(n_forall: usize, ltl: &LTLAst<usize>, n: usize, inputs: &HashSet<usize>, outputs: &HashSet<usize>) -> Result<Arena, String> {
        lazy_static! {
            static ref RE_NODE: Regex = Regex::new(
                "^ *([0-9]*) *([0-9]*) *([0-9]*) *([0-9]*( *, *[0-9]*)*) *(\"[^\"]*\")?;$")
                .unwrap();
        }

        let mut arg_formula = String::new();
        let mut assign = vec![0; n_forall];
        'outer: loop {
            if !arg_formula.is_empty() {
                arg_formula += " & ";
            }
            write!(arg_formula, "{}", PrintStrix(&ltl, &assign)).unwrap();
            for i in 0.. {
                if i == n_forall {
                    break 'outer;
                }
                if assign[i] + 1 < n {
                    assign[i] += 1;
                    break;
                } else {
                    assign[i] = 0;
                }
            }
        }

        let mut ins = Vec::new();
        let mut outs = Vec::new();
        for tr in 0..n {
            for ap in inputs {
                ins.push((*ap, tr));
            }
            for ap in outputs {
                outs.push((*ap, tr));
            }
        }

        let mut arg_ins = String::new();
        for (ap, tr) in ins.iter() {
            if !arg_ins.is_empty() {
                arg_ins += ",";
            }
            write!(arg_ins, "a{}_{}", ap, tr).unwrap();
        }

        let mut arg_outs = String::new();
        for (ap, tr) in outs.iter() {
            if !arg_outs.is_empty() {
                arg_outs += ",";
            }
            write!(arg_outs, "a{}_{}", ap, tr).unwrap();
        }

        trace!("strix formula: {}", arg_formula);
        trace!("strix ins: {}", arg_ins);
        trace!("strix outs: {}", arg_outs);

        let cmd = find_executable("strix")
            .expect("could not find strix");
        let mut strix = Command::new(cmd)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            //.stderr(Stdio::inherit())
            .arg("--realizability")
            .arg("--parity-game")
            .arg("--no-onthefly")
            .arg("--monolithic")
            .arg("--no-simplify-formula")
            .arg("--no-compact-colors")
            .arg("-f")
            .arg(arg_formula)
            .arg("--ins")
            .arg(arg_ins)
            .arg("--outs")
            .arg(arg_outs)
            .spawn()
            .expect("could not start strix");

        let mut output = BufReader::new(strix.stdout.take().unwrap());
        let mut buf = String::new();

        output.read_line(&mut buf).expect("IO error");
        match buf.trim() {
            "REALIZABLE" => {},
            "UNREALIZABLE" =>
                return Err("specification is unrealizable".to_string()),
            _ =>
                unreachable!()
        };

        buf.clear();
        output.read_to_string(&mut buf).expect("IO error");

        let mut nodes = Vec::new();
        for next in buf.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
            if let Some(rest) = next.strip_prefix("parity ") {
                let idx: usize = rest.trim()
                    .strip_suffix(';').unwrap()
                    .parse().unwrap();
                nodes.resize(idx, None);

            } else if let Some(cap) = RE_NODE.captures(next) {
                let idx: usize = cap.get(1).unwrap().as_str().parse().unwrap();
                let color: u64 = cap.get(2).unwrap().as_str().parse().unwrap();
                let owner: u8 = cap.get(3).unwrap().as_str().parse().unwrap();
                let successors: Vec<usize> = cap.get(4).unwrap().as_str()
                    .split(',')
                    .map(|idx| idx.trim().parse().unwrap())
                    .collect();
                let name = cap.get(6).unwrap().as_str()
                    .strip_prefix('"').unwrap()
                    .strip_suffix('"').unwrap()
                    .trim();

                let kind =
                    if let Some(edges) = name.strip_prefix("env:") {
                        assert!(owner == 1);
                        let edges: HashMap<_, _> = successors.iter().cloned()
                            .zip(edges.split(',')
                                .map(|e| boolexp::parse(e.trim()).unwrap())
                                .map(|e| e.map_vars(&|id| {
                                    let idx = id.strip_prefix('x').unwrap()
                                        .parse::<usize>().unwrap();
                                    ins[idx]
                                }))
                            )
                            .collect();
                        assert!(edges.len() == successors.len());
                        NodeKind::Env(edges)

                    } else if let Some(edges) = name.strip_prefix("sys:") {
                        assert!(owner == 0);
                        let edges: HashMap<_, _> = successors.iter().cloned()
                            .zip(edges.split(',')
                                .map(|e| boolexp::parse(e.trim()).unwrap())
                                .map(|e| e.map_vars(&|id| {
                                    let idx = id.strip_prefix('x').unwrap()
                                        .parse::<usize>().unwrap();
                                    outs[idx]
                                }))
                            )
                            .collect();
                        assert!(edges.len() == successors.len());
                        NodeKind::Sys(edges)

                    } else if name == "top" {
                        assert!(owner == 0);
                        assert!(successors.len() == 1 && successors[0] == idx);
                        NodeKind::Top

                    } else if name == "bottom" || name == "unexplored" {
                        assert!(owner == 1);
                        assert!(successors.len() == 1 && successors[0] == idx);
                        NodeKind::Bottom

                    } else {
                        unreachable!()
                    };

                if idx >= nodes.len() {
                    nodes.resize(idx + 1, None);
                }
                nodes[idx] = Some(Node {
                    idx,
                    color,
                    kind,
                    winning: None
                });

            } else {
                unreachable!();
            }
        }

        Ok(Arena {
            game_str: buf,
            nodes: nodes.into_iter().map(Option::unwrap).collect()
        })
    }

    pub fn solve(&mut self) {
        let cmd = find_executable("pgsolver")
            .expect("could not find pgsolver");
        let mut pgsolver = Command::new(cmd)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            //.stderr(Stdio::inherit())
            .arg("-global")
            .arg("recursive")
            .arg("--printsolonly")
            .spawn()
            .expect("could not start pgsolver");

        let mut input = pgsolver.stdin.take().unwrap();
        write!(input, "{}", self.game_str).expect("IO error");
        input.flush().expect("IO error");
        drop(input);

        let mut output = BufReader::new(pgsolver.stdout.take().unwrap());
        let mut buf = String::new();
        output.read_to_string(&mut buf).expect("IO error");

        for next in buf.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
            if next.starts_with("paritysol ") {
                continue;
            }
            let split: Vec<_> = next.strip_suffix(';').unwrap()
                .split(' ').collect();
            let idx: usize = split[0].parse().unwrap();
            let winner: bool = split[1].parse::<u8>().unwrap() == 1;
            
            let mut node = &mut self.nodes[idx];
            if winner == node.owner() {
                let succ: usize = split[2].parse().unwrap();
                let assign =
                    match &node.kind {
                        NodeKind::Env(edges)
                      | NodeKind::Sys(edges) => {
                            let formula = &edges[&succ];
                            let vars = formula.vars();

                            let mut cnf = CnfFormula::new();
                            let lit_true = cnf.new_lit();
                            cnf.add_clause(&[lit_true]);

                            let mut lits = HashMap::new();
                            let mut lits_vars = HashMap::new();
                            for next in &vars {
                                let lit = cnf.new_lit();
                                lits.insert(*next, lit);
                                lits_vars.insert(lit, *next);
                            }
                            let lit = tseitin::encode(&mut cnf, formula, lit_true,
                                &|id| Some(lits[id])).unwrap();
                            cnf.add_clause(&[lit]);

                            let mut solver = varisat::Solver::new();
                            solver.add_formula(&cnf);
                            assert!(solver.solve().unwrap());

                            solver.model().unwrap()
                                .into_iter()
                                .flat_map(|lit| {
                                    if let Some(var) = lits_vars.get(&lit.var().positive()) {
                                        Some((*var, lit.is_positive()))
                                    } else {
                                        None
                                    }
                                })
                                .collect()
                        },
                        _ =>
                            HashMap::new()
                    };
                node.winning = Some((succ, assign));
            } else {
                node.winning = None
            }
        }
    }
}

impl Node {
    pub fn owner(&self) -> bool {
        self.kind.owner()
    }
}

impl NodeKind {
    pub fn owner(&self) -> bool {
        match self {
            NodeKind::Sys(_) | NodeKind::Top =>
                false,
            NodeKind::Env(_) | NodeKind::Bottom =>
                true
        }
    }
}

struct PrintStrix<'a, 'b>(&'a LTLAst<usize>, &'b [usize]);

impl<'a, 'b> fmt::Display for PrintStrix<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            LTLAst::AP(ap, tr) =>
                write!(f, "a{}_{}", ap, self.1[*tr]),
            LTLAst::Const(true) =>
                write!(f, "true"),
            LTLAst::Const(false) =>
                write!(f, "false"),
            LTLAst::Unary(op, rhs) =>
                write!(f, "({} {})", op, PrintStrix(rhs, self.1)),
            LTLAst::Binary(op, lhs, rhs) =>
                write!(f, "({} {} {})", PrintStrix(lhs, self.1), op, PrintStrix(rhs, self.1))
        }
    }
}
