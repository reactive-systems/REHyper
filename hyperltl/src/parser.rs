use std::error::Error;

use super::*;

pub type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    EOF,
    Id(String), True, False,
    Forall, Exists,
    LParen, RParen, Underscore, Dot,
    And, Hat, Or, Not, Impl, Equiv,
    Next, Until, Weak, Release, Finally, Globally,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenInfo {
    pub row: usize,
    pub col: usize,
    pub token: Token
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::EOF => write!(f, "end of file"),
            Token::Id(id) => write!(f, "{}", id),
            Token::Forall => write!(f, "forall"),
            Token::Exists => write!(f, "exists"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Underscore => write!(f, "_"),
            Token::Dot => write!(f, "."),
            Token::And => write!(f, "&"),
            Token::Hat => write!(f, "^"),
            Token::Or => write!(f, "|"),
            Token::Not => write!(f, "!"),
            Token::Impl => write!(f, "->"),
            Token::Equiv => write!(f, "<->"),
            Token::Next => write!(f, "X"),
            Token::Until => write!(f, "U"),
            Token::Weak => write!(f, "W"),
            Token::Release => write!(f, "R"),
            Token::Finally => write!(f, "F"),
            Token::Globally => write!(f, "G"),
        }
    }
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.token {
            Token::EOF =>
                write!(f, "end of file"),
            Token::Id(id) =>
                write!(f, "identifier `{}`", id),
            _ =>
                write!(f, "`{}`", self.token)
        }
    }
}

impl Token {
    pub fn as_unary_op(&self) -> Option<LTLUnaryOp> {
        match &self {
            Token::Not => Some(LTLUnaryOp::Not),
            Token::Next => Some(LTLUnaryOp::Next),
            Token::Finally => Some(LTLUnaryOp::Finally),
            Token::Globally => Some(LTLUnaryOp::Globally),
            _ => None
        }
    }

    pub fn as_binary_op(&self) -> Option<LTLBinaryOp> {
        match &self {
            Token::And => Some(LTLBinaryOp::And),
            Token::Or => Some(LTLBinaryOp::Or),
            Token::Hat => Some(LTLBinaryOp::Xor),
            Token::Impl => Some(LTLBinaryOp::Impl),
            Token::Equiv => Some(LTLBinaryOp::Equiv),
            Token::Until => Some(LTLBinaryOp::Until),
            Token::Weak => Some(LTLBinaryOp::Weak),
            Token::Release => Some(LTLBinaryOp::Release),
            _ => None
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub row: usize,
    pub col: usize,
    pub desc: String
}

impl ParserError {
    pub fn new(row: usize, col: usize, desc: String) -> ParserError {
        ParserError { row, col, desc }
    }

    pub fn from_token(t: &TokenInfo, desc: String) -> ParserError {
        ParserError { row: t.row, col: t.col, desc }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}: {}", self.row, self.col, self.desc)
    }
}

impl Error for ParserError {}

pub struct Lexer<I: Iterator<Item = char>> {
    input: I,
    row: usize,
    col: usize,
    ch: Option<char>,
    token: Result<TokenInfo>
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(input: I) -> Lexer<I> {
        let mut res = Lexer {
            input,
            row: 0,
            col: 0,
            ch: None,
            token: Ok(TokenInfo { row: 0, col: 0, token: Token::EOF })
        };
        res.next();
        res
    }

    pub fn peek(&self) -> Result<TokenInfo> {
        self.token.clone()
    }

    pub fn next(&mut self) {
        self.token = self._next();
    }

    fn _next(&mut self) -> Result<TokenInfo> {
        if self.row == 0 {
            self.row += 1;
            self._read();
        }

        loop {
            match self.ch {
                Some(ch) if ch.is_whitespace() => self._read(),
                _ => break
            }
        }

        if self.ch.is_none() {
            return Ok(TokenInfo {
                row: self.row,
                col: self.col,
                token: Token::EOF
            });
        }

        let (row, col) = (self.row, self.col);
        let token = match self.ch {
            Some('(') => { self._read(); Ok(Token::LParen) },
            Some(')') => { self._read(); Ok(Token::RParen) },
            Some('_') => { self._read(); Ok(Token::Underscore) },
            Some('.') => { self._read(); Ok(Token::Dot) },
            Some('&') => { self._read(); Ok(Token::And) },
            Some('^') => { self._read(); Ok(Token::Hat) },
            Some('|') => { self._read(); Ok(Token::Or) },
            Some('!') => { self._read(); Ok(Token::Not) },
            Some('~') => { self._read(); Ok(Token::Not) },
            Some('-') | Some('=') => {
                self._read();
                match self.ch {
                    Some('>') => { self._read(); Ok(Token::Impl) },
                    Some(ch) => Err(self._error_skip(format!("unexpected `{}`", ch))),
                    None => Err(self._error_skip(format!("unexpected end of file")))
                }
            },
            Some('<') => {
                self._read();
                match self.ch {
                    Some('-') | Some('=') => {
                        self._read();
                        match self.ch {
                            Some('>') => { self._read(); Ok(Token::Equiv) },
                            Some(ch) => Err(self._error_skip(format!("unexpected `{}`", ch))),
                            None => Err(self._error_skip(format!("unexpected end of file")))
                        }
                    },
                    Some(ch) => Err(self._error_skip(format!("unexpected `{}`", ch))),
                    None => Err(self._error_skip(format!("unexpected end of file")))
                }
            },
            Some(delim @ '\'') | Some(delim @ '"') => {
                self._read();
                let mut id = String::new();
                loop {
                    match self.ch {
                        Some(ch) if ch == delim => {
                            self._read();
                            break Ok(Token::Id(id));
                        },
                        Some(ch) => {
                            id.push(ch);
                            self._read();
                        },
                        None =>
                            break Err(self._error_skip(format!("unexpected end of file")))
                    }
                }
            },
            Some(ch) => {
                if ch.is_alphabetic() {
                    self._next_id()
                } else {
                    Err(self._error_skip(format!("unexpected `{}`", ch)))
                }
            },
            None =>
                Err(self._error_skip("unexpected end of file".to_string()))
        }?;

        Ok(TokenInfo { row, col, token })
    }

    fn _next_id(&mut self) -> Result<Token> {
        let mut name = String::new();
        loop {
            match self.ch {
                Some(ch) if ch.is_alphanumeric() || ch == '\'' => {
                    name.push(ch);
                    self._read();
                },
                _ => { break; }
            }
        }

        Ok(match name.as_ref() {
            "forall" => Token::Forall,
            "exists" => Token::Exists,
            "true" => Token::True,
            "false" => Token::False,
            "X" => Token::Next,
            "U" => Token::Until,
            "W" => Token::Weak,
            "R" => Token::Release,
            "F" => Token::Finally,
            "G" => Token::Globally,
            _ => Token::Id(name)
        })
    }

    fn _error_skip(&mut self, desc: String) -> ParserError {
        let res = ParserError::new(self.row, self.col, desc);
        self._read();
        res
    }

    fn _read(&mut self) {
        let ch = self.input.next();
        match ch {
            Some('\r') | Some('\n') => {
                match (self.ch, ch) {
                    (Some('\r'), Some('\n')) => {},
                    _ => {
                        self.row += 1;
                        self.col = 0;
                    }
                }
            },
            _ => {
                self.col += 1;
            }
        }
        self.ch = ch;
    }
}

pub struct Parser<I: Iterator<Item = char>> {
    lexer: Lexer<I>
}

impl<I: Iterator<Item = char>> Parser<I> {
    pub fn new(input: I) -> Parser<I> {
        Parser {
            lexer: Lexer::new(input)
        }
    }

    pub fn parse(&mut self) -> Result<HyperLTLAst> {
        let res = self.parse_hyper()?;
        let t = self.lexer.peek()?;
        if let Token::EOF = t.token {
            Ok(res)
        } else {
            Err(ParserError::from_token(&t,
                format!("unexpected {}, expected end of file", t)))
        }
    }

    fn parse_hyper(&mut self) -> Result<HyperLTLAst> {
        let t = self.lexer.peek()?;
        Ok(match &t.token {
            op @ Token::Forall | op @ Token::Exists => {
                self.lexer.next();
                let t = self.lexer.peek()?;
                match t.token {
                    Token::Id(id) => {
                        self.lexer.next();
                        let t = self.lexer.peek()?;
                        if t.token != Token::Dot {
                            return Err(ParserError::from_token(&t,
                                format!("unexpected {}, expected `.`", t)));
                        }
                        self.lexer.next();
                        let exp = self.parse_hyper()?;
                        if op == &Token::Forall {
                            HyperLTLAst::Forall(id, Box::new(exp))
                        } else {
                            HyperLTLAst::Exists(id, Box::new(exp))
                        }
                    },
                    _ =>
                        return Err(ParserError::from_token(&t,
                            format!("unexpected {}, expected identifier", t)))
                }
            }
            _ =>
                HyperLTLAst::LTL(self.parse_exp(0)?)
        })
    }

    fn parse_exp(&mut self, prec_min: u16) -> Result<LTLAst> {
        let mut t = self.lexer.peek()?;

        let mut exp = match t.token.as_unary_op() {
            Some(op) => {
                self.lexer.next();
                let exp = self.parse_exp(std::u16::MAX)?;
                LTLAst::Unary(op, Box::new(exp))
            },
            None =>
                self.parse_primary()?
        };
        
        loop {
            t = self.lexer.peek()?;
            match t.token.as_binary_op() {
                Some(op) => {
                    let (lprec, rprec) = op.prec_pair();
                    if lprec < prec_min {
                        return Ok(exp);
                    } else {
                        self.lexer.next();
                        let rhs = self.parse_exp(rprec)?;
                        exp = LTLAst::Binary(op, Box::new(exp), Box::new(rhs));
                    }
                },
                None =>
                    return Ok(exp)
            };
        }
    }

    fn parse_primary(&mut self) -> Result<LTLAst> {
        let t = self.lexer.peek()?;
        Ok(match t.token {
            Token::Id(id) => {
                self.lexer.next();
                let t = self.lexer.peek()?;
                if t.token != Token::Underscore {
                    return Err(ParserError::from_token(&t,
                        format!("unexpected {}, expected `_`", t)));
                }
                self.lexer.next();
                let t = self.lexer.peek()?;
                match t.token {
                    Token::Id(id2) => {
                        self.lexer.next();
                        LTLAst::AP(id, id2)
                    },
                    _ =>
                        return Err(ParserError::from_token(&t,
                            format!("unexpected {}, expected identifier", t)))
                }
            },
            Token::True => {
                self.lexer.next();
                LTLAst::Const(true)
            },
            Token::False => {
                self.lexer.next();
                LTLAst::Const(false)
            },
            Token::LParen => {
                self.lexer.next();
                let res = self.parse_exp(0)?;
                let t = self.lexer.peek()?;
                if let Token::RParen = t.token {
                    self.lexer.next();
                    res
                } else {
                    return Err(ParserError::from_token(&t,
                        format!("unexpected {}, expected `)`", t)));
                }
            },
            _ =>
                return Err(ParserError::from_token(&t,
                    format!("unexpected {}", t)))
        })
    }
}
