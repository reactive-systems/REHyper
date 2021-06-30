use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

use super::*;

pub struct NonReactiveSeqEnforcer {
    dict: LTLDict,
    n_forall: usize,
    ltl: LTLAst<usize>,

    n: usize,
    traces: Vec<Vec<Event<usize>>>,
    enforcing: Option<(Vec<Event<usize>>, Vec<Event<usize>>)>,

    solver: HyperLTLSatSolver,
    _rvhyper: Child,
    rvhyper_in: BufWriter<ChildStdin>,
    rvhyper_out: BufReader<ChildStdout>
}

impl NonReactiveSeqEnforcer {
    pub fn new(formula: &HyperLTLAst) -> Result<NonReactiveSeqEnforcer, String> {
        let (dict, n_forall, ltl) = extract_formula(formula)?;

        let mut solver = HyperLTLSatSolver::init()?;
        if !solver.is_sat(1, n_forall, &LTLAst::Const(true), &ltl) {
            Err("given formula is unsaitisfiable")?;
        }

        let mut arg_formula = String::new();
        for next in 0..n_forall {
            write!(arg_formula, "forall t{}. ", next).unwrap();
        }
        write!(arg_formula, "{}", PrintRVHyper(&ltl)).unwrap();
        debug!("formula for rvhyper: {}", arg_formula);

        let cmd = find_executable("rvhyper")
            .expect("could not find rvhyper");
        let mut rvhyper = Command::new(cmd)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            //.stderr(Stdio::inherit())
            .arg("--quiet")
            .arg("-s")
            .arg(arg_formula)
            .arg("--stdin")
            .spawn()
            .expect("could not start rvhyper");

        let rvhyper_in = BufWriter::new(rvhyper.stdin.take().unwrap());
        let rvhyper_out = BufReader::new(rvhyper.stdout.take().unwrap());

        Ok(NonReactiveSeqEnforcer {
            dict,
            n_forall,
            ltl,

            n: 0,
            traces: vec![],
            enforcing: None,

            solver,
            _rvhyper: rvhyper,
            rvhyper_in,
            rvhyper_out
        })
    }
}

impl SeqEnforcer for NonReactiveSeqEnforcer {
    fn next(&mut self) {
        assert!(self.n == self.traces.len());

        self.traces.push(Vec::new());
        writeln!(self.rvhyper_in, "session start").expect("IO Error");
        self.rvhyper_in.flush().expect("IO Error");
    }

    fn enforce(&mut self, evt: &Event) -> Option<Event> {
        assert!(self.n == self.traces.len() - 1);

        if let Some((prefix, cycle)) = &self.enforcing {
            let t = self.traces[self.n].len();
            let evt_c =
                if t < prefix.len() {
                    &prefix[t]
                } else {
                    &cycle[(t - prefix.len()) % cycle.len()]
                };
            self.traces[self.n].push(evt_c.clone());

            //eprintln!("{:?}", evt_c);
            for (i, next) in evt_c.aps().enumerate() {
                if i == 0 {
                    write!(self.rvhyper_in, ";a{}", next).expect("IO error");
                } else {
                    write!(self.rvhyper_in, ",a{}", next).expect("IO error");
                }
            }
            writeln!(self.rvhyper_in, "").expect("IO error");
            self.rvhyper_in.flush().expect("IO error");

            let mut res = String::new();
            self.rvhyper_out.read_line(&mut res).expect("IO error");
            assert_eq!("ok", res.trim());

            Some(Event::uncompress(&evt_c, &self.dict))

        } else {
            let evt_c = evt.compress(&self.dict);
            for (i, next) in evt_c.aps().enumerate() {
                if i == 0 {
                    write!(self.rvhyper_in, ";a{}", next).expect("IO error");
                } else {
                    write!(self.rvhyper_in, ",a{}", next).expect("IO error");
                }
            }
            writeln!(self.rvhyper_in, "").expect("IO error");
            self.rvhyper_in.flush().expect("IO error");

            let mut res = String::new();
            self.rvhyper_out.read_line(&mut res).expect("IO error");
            if res.trim() == "ok" {
                self.traces[self.n].push(evt_c);
                return None;
            }

            info!("violation detected, enforcing now [evt: enforcing]");

            writeln!(self.rvhyper_in, "session end").expect("IO Error");
            writeln!(self.rvhyper_in, "session start").expect("IO Error");
            for evt in self.traces[self.n].iter() {
                //eprintln!("{:?}", evt);
                for (i, next) in evt.aps().enumerate() {
                    if i == 0 {
                        write!(self.rvhyper_in, ";a{}", next).expect("IO error");
                    } else {
                        write!(self.rvhyper_in, ",a{}", next).expect("IO error");
                    }
                }
                writeln!(self.rvhyper_in, "").expect("IO error");
            }
            self.rvhyper_in.flush().expect("IO error");

            let mut res = String::new();
            for _ in 0..self.traces[self.n].len() {
                res.clear();
                self.rvhyper_out.read_line(&mut res).expect("IO error");
                assert_eq!("ok", res.trim());
            }

            let mut formula_ex = None;
            let max = self.traces.iter().map(|tr| tr.len()).max().unwrap_or(0);
            for i in (0..max).rev() {
                let mut conj = None;

                for tr in 0..=self.n {
                    if i < self.traces[tr].len() {
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

            let traces = self.solver.sat(self.n + 1, self.n_forall,
                &formula_ex, &self.ltl).unwrap();
            self.enforcing = Some(traces.into_iter().last().unwrap());
            self.enforce(evt)
        }
    }

    fn finish(&mut self) -> Option<Event> {
        assert!(self.n == self.traces.len() - 1);

        if self.enforcing.is_some() {
            info!("trace ended while enforcing, monitoring again");
            self.enforcing = None;
        }

        self.n += 1;
        writeln!(self.rvhyper_in, "session end").expect("IO Error");
        None
    }
}
