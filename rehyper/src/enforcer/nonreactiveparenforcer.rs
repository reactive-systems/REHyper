use std::fs::{File, OpenOptions};
use std::process::{Child, ChildStdout, Command, Stdio};

use nix::{sys::stat, unistd};
use tempfile::TempDir;

use super::*;

pub struct NonReactiveParEnforcer {
    dict: LTLDict,
    n_forall: usize,
    ltl: LTLAst<usize>,
    n: usize,

    t: usize,
    traces: Vec<Vec<Event<usize>>>,
    enforcing: Option<Vec<(Vec<Event<usize>>, Vec<Event<usize>>)>>,

    solver: HyperLTLSatSolver,
    _tempdir: TempDir,
    _rvhyper: Child,
    pipes: Vec<BufWriter<File>>,
    rvhyper_out: BufReader<ChildStdout>
}

impl NonReactiveParEnforcer {
    pub fn new(formula: &HyperLTLAst, n: usize) -> Result<NonReactiveParEnforcer, String> {
        let (dict, n_forall, ltl) = extract_formula(formula)?;

        let mut solver = HyperLTLSatSolver::init()?;
        if !solver.is_sat(n, n_forall, &LTLAst::Const(true), &ltl) {
            Err("given formula is unsaitisfiable")?;
        }

        let mut arg_formula = String::new();
        for next in 0..n_forall {
            write!(arg_formula, "forall t{}. ", next).unwrap();
        }
        write!(arg_formula, "{}", PrintRVHyper(&ltl)).unwrap();
        debug!("formula for rvhyper: {}", arg_formula);

        let tempdir = tempfile::Builder::new()
            .prefix("rehyper-rvhyper")
            .rand_bytes(16)
            .tempdir()
            .map_err(|_| "could not create tempdir")?;

        let mut paths = Vec::new();
        for i in 0..n {
            let next = tempdir.path().join(format!("tr{}.pipe", i));
            unistd::mkfifo(&next, stat::Mode::S_IRWXU)
                .map_err(|_| "could not create pipe")?;
            paths.push(next);
        }

        let cmd = find_executable("rvhyper")
            .expect("could not find rvhyper");
        let mut rvhyper = Command::new(cmd)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            //.stderr(Stdio::inherit())
            .arg("--quiet")
            .arg("--parallel")
            .arg("-s")
            .arg(arg_formula)
            .args(&paths)
            .spawn()
            .expect("could not start rvhyper");

        let rvhyper_out = BufReader::new(rvhyper.stdout.take().unwrap());
        let mut pipes = Vec::new();

        for next in &paths {
            pipes.push(BufWriter::new(OpenOptions::new()
                .append(true).open(&next)
                .map_err(|_| "could not open pipe")?));
        }

        Ok(NonReactiveParEnforcer {
            dict,
            n_forall,
            ltl,
            n,

            t: 0,
            traces: vec![vec![]; n],
            enforcing: None,

            solver,
            _tempdir: tempdir,
            _rvhyper: rvhyper,
            pipes,
            rvhyper_out
        })
    }
}

impl ParEnforcer for NonReactiveParEnforcer {
    fn enforce(&mut self, evts: &[Event]) -> Option<Vec<Event>> {
        assert!(evts.len() == self.n);
        if let Some(traces) = &self.enforcing {
            let t = self.t;
            self.t += 1;

            let mut res = Vec::new();
            for (prefix, cycle) in traces {
                let len = prefix.len();
                if t < len {
                    res.push(Event::uncompress(&prefix[t], &self.dict));
                } else {
                    res.push(Event::uncompress(&cycle[(t - len) % cycle.len()], &self.dict));
                }
            }
            Some(res)

        } else {
            assert!(!self.pipes.is_empty());

            let evts_c: Vec<_> = evts.iter().map(|e| e.compress(&self.dict)).collect();
            for i in 0..self.n {
                for (j, next) in evts_c[i].aps().enumerate() {
                    if j == 0 {
                        write!(self.pipes[i], ";a{}", next).expect("IO error");
                    } else {
                        write!(self.pipes[i], ",a{}", next).expect("IO error");
                    }
                }
                writeln!(self.pipes[i], "").expect("IO error");
                self.pipes[i].flush().expect("IO error");
            }

            let mut res = String::new();
            self.rvhyper_out.read_line(&mut res).expect("IO error");
            if res.trim() == "ok" {
                for (i, evt) in evts_c.into_iter().enumerate() {
                    self.traces[i].push(evt);
                }
                self.t += 1;
                return None;
            }

            info!("violation detected, enforcing now [evt: enforcing]");

            let mut formula_ex = None;
            for i in (0..self.traces[0].len()).rev() {
                let mut conj = None;
                for tr in 0..self.n {
                    for ap in 0..self.dict.ap.size() {
                        let next = if self.traces[tr][i].contains(&ap) {
                            LTLAst::AP(ap, tr)
                        } else {
                            LTLAst::Unary(LTLUnaryOp::Not,
                                Box::new(LTLAst::AP(ap, tr)))
                        };
                        if let Some(prev) = conj {
                            conj = Some(LTLAst::Binary(LTLBinaryOp::And,
                                Box::new(prev),
                                Box::new(next)));
                        } else {
                            conj = Some(next);
                        }
                    }
                }
                let conj = conj.unwrap_or_else(|| LTLAst::Const(true));
                if let Some(prev) = formula_ex {
                    formula_ex = Some(LTLAst::Binary(LTLBinaryOp::And,
                        Box::new(conj),
                        Box::new(LTLAst::Unary(LTLUnaryOp::Next,
                            Box::new(prev)))));
                } else {
                    formula_ex = Some(conj);
                }
            }
            let formula_ex = formula_ex.unwrap_or_else(|| LTLAst::Const(true));

            let traces = self.solver.sat(self.n, self.n_forall,
                &formula_ex, &self.ltl).unwrap();
            self.enforcing = Some(traces);
            self.pipes.clear();
            self.enforce(evts)
        }
    }

    fn finish(&mut self) -> Option<Vec<Event>> {
        if self.enforcing.is_none() {
            assert!(!self.pipes.is_empty());

            self.pipes.clear();
            let mut res = String::new();
            self.rvhyper_out.read_line(&mut res).expect("IO error");
            if res.trim() == "ok" {
                None
            } else {
                error!("{}", res);
                unimplemented!();
            }
        } else {
            None
        }
    }
}

