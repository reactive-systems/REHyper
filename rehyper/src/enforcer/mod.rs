use std::borrow::Borrow;
use std::collections::{hash_set, HashSet};
use std::hash::Hash;
use std::fmt::{self, Write as _};
use std::fs::{File, OpenOptions};
use std::io::{Write as _, BufRead, BufReader, BufWriter};
use std::process::{Child, ChildStdout, Command, Stdio};

use hyperltl::*;
use log::{debug, error, info, log_enabled, trace};
use nix::{sys::stat, unistd};
use tempfile::TempDir;

use crate::find_executable;

mod arena;

#[derive(Debug, Clone)]
pub struct Event<ID: Identifier = String> {
    aps: HashSet<ID>,
}

impl<ID: Identifier> Event<ID> {
    pub fn new() -> Event<ID> {
        Event {
            aps: HashSet::new()
        }
    }

    pub fn add(&mut self, id: ID) {
        self.aps.insert(id);
    }

    pub fn aps(&self) -> hash_set::Iter<'_, ID> {
        self.aps.iter()
    }

    pub fn contains<Q>(&self, id: &Q) -> bool
        where ID: Borrow<Q>, Q: Hash + Eq
    {
        self.aps.contains(id)
    }

    pub fn compress(&self, dict: &LTLDict<ID>) -> Event<usize> {
        Event {
            aps: self.aps.iter().flat_map(|ap| dict.ap.lookup(ap)).collect()
        }
    }

    pub fn uncompress(this: &Event<usize>, dict: &LTLDict<ID>) -> Event<ID> {
        Event {
            aps: this.aps.iter().map(|ap| dict.ap.get(*ap).unwrap().clone()).collect()
        }
    }
}

impl fmt::Display for Event<String> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, next) in self.aps().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            if next.is_empty() {
                write!(f , "\"\"")?;
            } else {
                let noescape =
                    next.chars().next().unwrap().is_alphabetic() &&
                    next.chars().all(|c| c.is_alphanumeric());
                if noescape {
                    write!(f, "{}", next)?;
                } else if !next.contains('"') {
                    write!(f, "\"{}\"", next)?;
                } else {
                    write!(f, "'{}'", next)?;
                }
            }
        }
        Ok(())
    }
}

pub trait ParEnforcer {
    fn enforce(&mut self, evts: &[Event]) -> Option<Vec<Event>>;
    fn finish(&mut self) -> Option<Vec<Event>>;
}

pub trait SeqEnforcer {
    fn next(&mut self);
    fn enforce(&mut self, evts: &Event) -> Option<Event>;
    fn finish(&mut self) -> Option<Event>;
}

mod nonreactiveparenforcer;
pub use nonreactiveparenforcer::NonReactiveParEnforcer;

mod nonreactiveseqenforcer;
pub use nonreactiveseqenforcer::NonReactiveSeqEnforcer;

mod reactiveparenforcer;
pub use reactiveparenforcer::ReactiveParEnforcer;

mod reactiveseqenforcer;
pub use reactiveseqenforcer::ReactiveSeqEnforcer;



fn extract_formula(formula: &HyperLTLAst<String>) -> Result<(LTLDict, usize, LTLAst<usize>), String> {
    if !formula.is_closed() {
        return Err("formula is not closed".to_string());
    }
    if !formula.is_universal() {
        return Err("formula is not universal".to_string())
    }

    let mut dict = LTLDict::new();
    let mut formula = formula.compress(&mut dict);

    let mut n_forall = 0;
    let ltl = loop {
        match formula {
            HyperLTLAst::LTL(ltl) =>
                break ltl,
            HyperLTLAst::Forall(tr, rhs) => {
                if tr != n_forall {
                    return Err(format!("trace variable defined twice: {}",
                        dict.tr.get(tr).unwrap()));
                }
                n_forall += 1;
                formula = *rhs;
            }
            HyperLTLAst::Exists(_tr, _rhs) =>
                unreachable!()
        }
    };

    if log_enabled!(log::Level::Debug) {
        let mut buf = String::new();
        for next in 0..n_forall {
            write!(buf, "forall {}. ", next).unwrap();
        }
        debug!("processed universal formula: {}{}", buf, ltl);
    }

    Ok((dict, n_forall, ltl))
}

struct HyperLTLSatSolver {
    _tempdir: TempDir,
    _leviathan: Child,
    pipe: BufWriter<File>,
    output: BufReader<ChildStdout>
}

impl HyperLTLSatSolver {
    pub fn init() -> Result<HyperLTLSatSolver, String> {
        let tempdir = tempfile::Builder::new()
            .prefix("rehyper-leviathan")
            .rand_bytes(16)
            .tempdir()
            .map_err(|_| "could not create tempdir")?;

        let pipe_path = tempdir.path().join("ltl.pipe");
        unistd::mkfifo(&pipe_path, stat::Mode::S_IRWXU)
            .map_err(|_| "could not create pipe")?;

        let cmd = find_executable("leviathan")
            .expect("could not find leviathan");
        let mut leviathan = Command::new(cmd)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .arg("--model")
            .arg("--parsable")
            .arg(&pipe_path)
            .spawn()
            .expect("could not start leviathan");

        let output = BufReader::new(leviathan.stdout.take().unwrap());

        let pipe = BufWriter::new(OpenOptions::new()
            .append(true).open(&pipe_path)
            .map_err(|_| "could not open pipe")?);

        Ok(HyperLTLSatSolver {
            _tempdir: tempdir,
            _leviathan: leviathan,
            pipe,
            output
        })
    }

    pub fn is_sat(&mut self,
        n_exists: usize, n_forall: usize,
        formula_ex: &LTLAst<usize>, formula_fa: &LTLAst<usize>
    ) -> bool {
        self.query(n_exists, n_forall, formula_ex, formula_fa).starts_with("SAT;")
    }

    pub fn sat(&mut self,
        n_exists: usize, n_forall: usize,
        formula_ex: &LTLAst<usize>, formula_fa: &LTLAst<usize>
    ) -> Option<Vec<(Vec<Event<usize>>, Vec<Event<usize>>)>> {
        let output = self.query(n_exists, n_forall, formula_ex, formula_fa);
        let output = output.trim().strip_prefix("SAT;")?;

        let (len, str_cycle) = output.split(" -> ").enumerate().last().unwrap();
        let cycle_at = str_cycle.strip_prefix('#').unwrap().parse::<usize>().unwrap();
        let prefix_len = cycle_at;
        let cycle_len = len - cycle_at;

        trace!("{}; prefix: {}; cycle: {}; cycle_at: {}", len, prefix_len, cycle_len, cycle_at);

        let mut traces = vec![
            (vec![Event::new(); prefix_len], vec![Event::new(); cycle_len]);
            n_exists];

        for (i, next) in output.split(" -> ").enumerate() {
            if i >= len {
                break;
            }
            let next = next
                .strip_prefix('{').unwrap()
                .strip_suffix('}').unwrap();
            for evt_tr in next.split(',') {
                if !evt_tr.starts_with('!') {
                    let mut spl = evt_tr.strip_prefix('a').unwrap().split('u');
                    let ap = spl.next().unwrap()
                        .parse::<usize>().unwrap();
                    let tr = spl.next().unwrap()
                        .parse::<usize>().unwrap();
                    if i < cycle_at {
                        traces[tr].0[i].add(ap);
                    } else {
                        traces[tr].1[i - cycle_at].add(ap);
                    }
                }
            }
        }

        Some(traces)
    }

    pub fn query(&mut self,
        n_exists: usize, n_forall: usize,
        formula_ex: &LTLAst<usize>, formula_fa: &LTLAst<usize>
    ) -> String {
        trace!("sat query: exists {}: {}", n_exists, formula_ex);
        trace!("sat query: forall {}: {}", n_forall, formula_fa);

        let mut formula = String::new();
        write!(formula, "!empty & {}", PrintSolverEx(formula_ex)).unwrap();

        let mut assign = vec![0; n_forall];
        'outer: loop {
            write!(formula, " & {}", PrintSolverFa(formula_fa, &assign)).unwrap();
            for i in 0.. {
                if i == n_forall {
                    break 'outer;
                }
                if assign[i] + 1 < n_exists {
                    assign[i] += 1;
                    break;
                } else {
                    assign[i] = 0;
                }
            }
        }

        trace!("formula for leviathan: {}", formula);
        writeln!(self.pipe, "{}", formula).expect("IO error");
        self.pipe.flush().expect("IO error");

        let mut output = String::new();
        self.output.read_line(&mut output).expect("IO error");
        trace!("{}", output.trim());

        output
    }
}

struct PrintSolverEx<'a>(&'a LTLAst<usize>);

impl<'a> fmt::Display for PrintSolverEx<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            LTLAst::AP(ap, tr) =>
                write!(f, "a{}u{}", ap, tr),
            LTLAst::Const(true) =>
                write!(f, "(True | !True)"),
            LTLAst::Const(false) =>
                write!(f, "(False & !False)"),
            LTLAst::Unary(op, rhs) =>
                write!(f, "({} {})", op, PrintSolverEx(rhs)),
            LTLAst::Binary(LTLBinaryOp::Xor, lhs, rhs) =>
                write!(f, "(!({} <-> {}))", PrintSolverEx(lhs), PrintSolverEx(rhs)),
            LTLAst::Binary(LTLBinaryOp::Weak, lhs, rhs) =>
                write!(f, "(({} U {}) | G {})", PrintSolverEx(lhs), PrintSolverEx(rhs), PrintSolverEx(lhs)),
            LTLAst::Binary(op, lhs, rhs) =>
                write!(f, "({} {} {})", PrintSolverEx(lhs), op, PrintSolverEx(rhs))
        }
    }
}

struct PrintSolverFa<'a, 'b>(&'a LTLAst<usize>, &'b [usize]);

impl<'a, 'b> fmt::Display for PrintSolverFa<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            LTLAst::AP(ap, tr) =>
                write!(f, "a{}u{}", ap, self.1[*tr]),
            LTLAst::Const(true) =>
                write!(f, "(True | !True)"),
            LTLAst::Const(false) =>
                write!(f, "(False & !False)"),
            LTLAst::Unary(op, rhs) =>
                write!(f, "({} {})", op, PrintSolverFa(rhs, self.1)),
            LTLAst::Binary(LTLBinaryOp::Xor, lhs, rhs) =>
                write!(f, "(!({} <-> {}))", PrintSolverFa(lhs, self.1), PrintSolverFa(rhs, self.1)),
            LTLAst::Binary(LTLBinaryOp::Weak, lhs, rhs) =>
                write!(f, "(({} U {}) | G {})", PrintSolverFa(lhs, self.1), PrintSolverFa(rhs, self.1), PrintSolverFa(lhs, self.1)),
            LTLAst::Binary(op, lhs, rhs) =>
                write!(f, "({} {} {})", PrintSolverFa(lhs, self.1), op, PrintSolverFa(rhs, self.1))
        }
    }
}

struct PrintRVHyper<'a>(&'a LTLAst<usize>);

impl<'a> fmt::Display for PrintRVHyper<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            LTLAst::AP(ap, tr) =>
                write!(f, "a{}_t{}", ap, tr),
            LTLAst::Const(true) =>
                write!(f, "True"),
            LTLAst::Const(false) =>
                write!(f, "False"),
            LTLAst::Unary(op, rhs) =>
                write!(f, "({} {})", op, PrintRVHyper(rhs)),
            LTLAst::Binary(op, lhs, rhs) =>
                write!(f, "({} {} {})", PrintRVHyper(lhs), op, PrintRVHyper(rhs))
        }
    }
}
