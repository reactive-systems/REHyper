use std::error::Error;

use super::*;

pub type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    EOF,
    Id(String), True, False,
    LParen, RParen,
    And, Or, Not, Impl, Equiv
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
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::And => write!(f, "&"),
            Token::Or => write!(f, "|"),
            Token::Not => write!(f, "!"),
            Token::Impl => write!(f, "->"),
            Token::Equiv => write!(f, "<->"),
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
    pub fn as_binary_op(&self) -> Option<BinaryOp> {
        match &self {
            Token::And => Some(BinaryOp::And),
            Token::Or => Some(BinaryOp::Or),
            Token::Impl => Some(BinaryOp::Impl),
            Token::Equiv => Some(BinaryOp::Equiv),
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
            Some('&') => { self._read(); Ok(Token::And) },
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
                Some(ch) if ch.is_alphanumeric() || ch == '_' || ch == '\'' => {
                    name.push(ch);
                    self._read();
                },
                _ => { break; }
            }
        }

        Ok(match name.as_ref() {
            "true" => Token::True,
            "false" => Token::False,
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

    pub fn parse(&mut self) -> Result<BoolExp> {
        let res = self.parse_exp(0)?;
        let t = self.lexer.peek()?;
        if let Token::EOF = t.token {
            Ok(res)
        } else {
            Err(ParserError::from_token(&t,
                format!("unexpected {}, expected end of file", t)))
        }
    }

    fn parse_exp(&mut self, prec_min: u16) -> Result<BoolExp> {
        let mut t = self.lexer.peek()?;

        let mut exp = match &t.token {
            Token::Not => {
                self.lexer.next();
                let exp = self.parse_exp(std::u16::MAX)?;
                BoolExp::Not(Box::new(exp))
            },
            _ =>
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
                        exp = BoolExp::Binary(op, Box::new(exp), Box::new(rhs));
                    }
                },
                None =>
                    return Ok(exp)
            };
        }
    }

    fn parse_primary(&mut self) -> Result<BoolExp> {
        let t = self.lexer.peek()?;
        Ok(match t.token {
            Token::Id(id) => {
                self.lexer.next();
                BoolExp::Id(id)
            },
            Token::True => {
                self.lexer.next();
                BoolExp::Const(true)
            },
            Token::False => {
                self.lexer.next();
                BoolExp::Const(false)
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
