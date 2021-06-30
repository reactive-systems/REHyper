use boolexp::{BinaryOp, BoolExp, Identifier};
use varisat::{CnfFormula, ExtendFormula, Lit};

///Encodes lhs <-> (rhs0 /\ rhs1)
pub fn encode_and(cnf: &mut CnfFormula, lhs: Lit, rhs0: Lit, rhs1: Lit) {
    cnf.add_clause(&[!lhs, rhs0]);
    cnf.add_clause(&[!lhs, rhs1]);
    cnf.add_clause(&[lhs, !rhs0, !rhs1]);
}

pub fn encode<T, F>(cnf: &mut CnfFormula, f: &BoolExp<T>, lit_true: Lit, lookup: &F)
    -> Result<Lit, T>
where T: Identifier, F: Fn(&T) -> Option<Lit>
{
    match f {
        BoolExp::Id(id) =>
            if let Some(lit) = lookup(id) {
                Ok(lit)
            } else {
                Err(id.clone())
            },
        BoolExp::Const(b) =>
            if *b {
                Ok(lit_true)
            } else {
                Ok(!lit_true)
            },
        BoolExp::Not(rhs) => {
            let rhs = encode(cnf, &rhs, lit_true, lookup)?;
            Ok(!rhs)
        }
        BoolExp::Binary(op, lhs, rhs) => {
            let lhs = encode(cnf, &lhs, lit_true, lookup)?;
            let rhs = encode(cnf, &rhs, lit_true, lookup)?;
            let res = cnf.new_lit();
            match op {
                BinaryOp::And => {
                    encode_and(cnf, res, lhs, rhs);
                    Ok(res)
                },
                BinaryOp::Or => {
                    //     res <-> (lhs \/ rhs)
                    //<=> !res <-> !(lhs \/ rhs)
                    //<=> !res <-> (!lhs /\ !rhs)
                    encode_and(cnf, !res, !lhs, !rhs);
                    Ok(res)
                },
                BinaryOp::Equiv => {
                    //     res <-> (lhs <-> rhs)
                    //<=>  res <-> ((lhs -> rhs) /\ (rhs -> lhs))
                    //<=>  (res -> ((lhs -> rhs) /\ (rhs -> lhs))) /\ (((lhs -> rhs) /\ (rhs -> lhs)) -> res)
                    //<=>  (res -> (lhs -> rhs)) /\ (res -> (rhs -> lhs)) /\ (((lhs -> rhs) /\ (rhs -> lhs)) -> res)
                    //<=>  (!res \/ !lhs \/ rhs) /\ (!res \/ !rhs \/ lhs) /\ (!((!lhs \/ rhs) /\ (!rhs \/ lhs)) \/ res)
                    //<=>  (!res \/ !lhs \/ rhs) /\ (!res \/ !rhs \/ lhs) /\ (!(!lhs \/ rhs) \/ !(!rhs \/ lhs) \/ res)
                    //<=>  (!res \/ !lhs \/ rhs) /\ (!res \/ !rhs \/ lhs) /\ ((lhs /\ !rhs) \/ (rhs /\ !lhs) \/ res)
                    //<=>  (!res \/ !lhs \/ rhs) /\ (!res \/ !rhs \/ lhs) /\ (((lhs \/ rhs) /\ (!rhs \/ !lhs)) \/ res)
                    //<=>  (!res \/ !lhs \/ rhs) /\ (!res \/ !rhs \/ lhs) /\ (lhs \/ rhs \/ res) /\ (!rhs \/ !lhs \/ res)
                    //<=>  (!res \/ !lhs \/ rhs) /\ (!res \/ lhs \/ !rhs) /\ (res \/ lhs \/ rhs) /\ (res \/ !lhs \/ !rhs)
                    cnf.add_clause(&[!res, !lhs, rhs]);
                    cnf.add_clause(&[!res, lhs, !rhs]);
                    cnf.add_clause(&[res, lhs, rhs]);
                    cnf.add_clause(&[res, !lhs, !rhs]);
                    Ok(res)
                },
                BinaryOp::Impl => {
                    //     res <-> (lhs -> rhs)
                    //<=>  res <-> (!lhs \/ rhs)
                    //<=> !res <-> !(!lhs \/ rhs)
                    //<=> !res <-> (lhs /\ !rhs)
                    encode_and(cnf, !res, lhs, !rhs);
                    Ok(res)
                }
            }
        }
    }
}
