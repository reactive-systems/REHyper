use super::*;

pub struct ReactiveSeqEnforcer {
    //dict: LTLDict,
    //n_forall: usize,
    //ltl: LTLAst<usize>,
    //inputs: HashSet<usize>,
    //traces: Vec<Vec<Event<usize>>>
}

impl ReactiveSeqEnforcer {
    pub fn new(_formula: &HyperLTLAst, _inputs: &[String]) -> Result<ReactiveSeqEnforcer, String> {
        unimplemented!();
        /*let (dict, n_forall, ltl) = extract_formula(formula)?;

        let inputs = inputs.iter().flat_map(|ap| dict.ap.lookup(ap)).collect();

        //TODO

        Ok(ReactiveSeqEnforcer {
            dict,
            n_forall,
            ltl,
            inputs,
            traces: vec![],
        })*/
    }
}

impl SeqEnforcer for ReactiveSeqEnforcer {
    fn next(&mut self) {
        //TODO
    }

    fn enforce(&mut self, _evt: &Event) -> Option<Event> {
        //TODO
        None
    }

    fn finish(&mut self) -> Option<Event> {
        //TODO
        None
    }
}
