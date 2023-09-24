use crate::bdd::{BDDArena, BoolVar, VarID, BDD};
use std::iter::Peekable;

// EBNF
// EXPR = EXPR2
// EXPR2 = EXPR1 EXPR2_Loop | EXPR1
// EXPR2_Loop = ("&" | "|" | "^") EXPR1 EXPR2_Loop | ε
// EXPR1 = "!" ATOM | ATOM
// ATOM = VAR | "(" EXPR2 ")"
// VAR = some string

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Loc(usize, usize);
impl Loc {
    fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.0, other.1))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Annot<T> {
    value: T,
    loc: Loc,
}
impl<T> Annot<T> {
    fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

/*************** Tokenizer ***************/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TokenKind {
    Var(BoolVar),
    And,
    Or,
    Xor,
    Not,
    LParen,
    RParen,
}
type Token = Annot<TokenKind>;
impl Token {
    fn var(v: BoolVar, loc: Loc) -> Self {
        Self::new(TokenKind::Var(v), loc)
    }
    fn and(loc: Loc) -> Self {
        Self::new(TokenKind::And, loc)
    }
    fn or(loc: Loc) -> Self {
        Self::new(TokenKind::Or, loc)
    }
    fn xor(loc: Loc) -> Self {
        Self::new(TokenKind::Xor, loc)
    }
    fn not(loc: Loc) -> Self {
        Self::new(TokenKind::Not, loc)
    }
    fn lparen(loc: Loc) -> Self {
        Self::new(TokenKind::LParen, loc)
    }
    fn rparen(loc: Loc) -> Self {
        Self::new(TokenKind::RParen, loc)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LexErrorKind {
    InvalidChar(char),
    Eof,
}
type LexError = Annot<LexErrorKind>;
impl LexError {
    fn invalid_char(c: char, loc: Loc) -> Self {
        LexError::new(LexErrorKind::InvalidChar(c), loc)
    }
    fn eof(loc: Loc) -> Self {
        LexError::new(LexErrorKind::Eof, loc)
    }
}
fn lex(input: &str, var_vec: &mut Vec<String>) -> Result<Vec<Token>, LexError> {
    let mut tokens = vec![];
    let input = input.as_bytes();
    let mut pos = 0;
    macro_rules! lex_a_token {
        ($lexer: expr) => {{
            let (token, p) = $lexer?;
            tokens.push(token);
            pos = p;
        }};
    }
    while pos < input.len() {
        match input[pos] {
            b'a'..=b'z' => lex_a_token!(lex_var(input, var_vec, pos)),
            b'A'..=b'Z' => lex_a_token!(lex_var(input, var_vec, pos)),
            b'&' => lex_a_token!(lex_and(input, pos)),
            b'|' => lex_a_token!(lex_or(input, pos)),
            b'^' => lex_a_token!(lex_xor(input, pos)),
            b'!' => lex_a_token!(lex_not(input, pos)),
            b'(' => lex_a_token!(lex_lparen(input, pos)),
            b')' => lex_a_token!(lex_rparen(input, pos)),
            b' ' | b'\n' | b'\t' => {
                let ((), p) = skip_spaces(input, pos);
                pos = p;
            }
            b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1))),
        }
    }
    Ok(tokens)
}
fn consume_byte(input: &[u8], pos: usize, b: u8) -> Result<(u8, usize), LexError> {
    if input.len() <= pos {
        return Err(LexError::eof(Loc(pos, pos)));
    }
    if input[pos] != b {
        return Err(LexError::invalid_char(
            input[pos] as char,
            Loc(pos, pos + 1),
        ));
    }
    Ok((b, pos + 1))
}
fn lex_var(
    input: &[u8],
    var_vec: &mut Vec<String>,
    mut pos: usize,
) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    let chars = b"1234567890-abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let start = pos;
    while pos < input.len() && chars.contains(&input[pos]) {
        pos += 1;
    }
    let var_name = from_utf8(&input[start..pos]).unwrap();
    let index_of_var = var_vec.iter().position(|x| x == var_name);
    let index_of_var = if index_of_var.is_some() {
        index_of_var.unwrap()
    } else {
        var_vec.push(var_name.to_string());
        var_vec.len() - 1
    };

    Ok((
        Token::var(BoolVar::new(index_of_var as VarID), Loc(start, pos)),
        pos,
    ))
}
fn lex_and(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'&').map(|(_, end)| (Token::and(Loc(start, end)), end))
}
fn lex_or(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'|').map(|(_, end)| (Token::or(Loc(start, end)), end))
}
fn lex_xor(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'^').map(|(_, end)| (Token::xor(Loc(start, end)), end))
}
fn lex_not(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'!').map(|(_, end)| (Token::not(Loc(start, end)), end))
}
fn lex_lparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'(').map(|(_, end)| (Token::lparen(Loc(start, end)), end))
}
fn lex_rparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b')').map(|(_, end)| (Token::rparen(Loc(start, end)), end))
}
fn skip_spaces(input: &[u8], mut pos: usize) -> ((), usize) {
    while pos < input.len() && b" \n\t".contains(&input[pos]) {
        pos += 1;
    }
    ((), pos)
}

/*************** Parser ***************/
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AstKind {
    Var(BoolVar),
    UniOp { op: UniOp, e: Box<Ast> },
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
}
type Ast = Annot<AstKind>;
impl Ast {
    fn var(var: BoolVar, loc: Loc) -> Self {
        Self::new(AstKind::Var(var), loc)
    }
    fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(AstKind::UniOp { op, e: Box::new(e) }, loc)
    }
    fn binop(op: BinOp, l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::BinOp {
                op,
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UniOpKind {
    Not,
}
type UniOp = Annot<UniOpKind>;
impl UniOp {
    fn not(loc: Loc) -> Self {
        Self::new(UniOpKind::Not, loc)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BinOpKind {
    And,
    Or,
    Xor,
}
type BinOp = Annot<BinOpKind>;
impl BinOp {
    fn and(loc: Loc) -> Self {
        Self::new(BinOpKind::And, loc)
    }
    fn or(loc: Loc) -> Self {
        Self::new(BinOpKind::Or, loc)
    }
    fn xor(loc: Loc) -> Self {
        Self::new(BinOpKind::Xor, loc)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ParseError {
    // UnexpectedToken(Token),
    NotExpression(Token),
    // NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    Eof,
}
fn parse_impl(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let ret = parse_expr(&mut tokens)?;
    match tokens.next() {
        Some(tok) => Err(ParseError::RedundantExpression(tok)),
        None => Ok(ret),
    }
}
fn parse_expr<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    parse_expr2(tokens)
}
fn parse_expr2<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut e = parse_expr1(tokens)?;
    loop {
        match tokens.peek().map(|tok| tok.value) {
            Some(TokenKind::And) | Some(TokenKind::Or) | Some(TokenKind::Xor) => {
                let op = match tokens.next().unwrap() {
                    Token {
                        value: TokenKind::And,
                        loc,
                    } => BinOp::and(loc),
                    Token {
                        value: TokenKind::Or,
                        loc,
                    } => BinOp::or(loc),
                    Token {
                        value: TokenKind::Xor,
                        loc,
                    } => BinOp::xor(loc),
                    _ => unreachable!(),
                };
                let r = parse_expr1(tokens)?;
                let loc = e.loc.merge(&r.loc);
                e = Ast::binop(op, e, r, loc)
            }
            _ => return Ok(e),
        }
    }
}
fn parse_expr1<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().map(|tok| tok.value) {
        Some(TokenKind::Not) => {
            let op = match tokens.next() {
                Some(Token {
                    value: TokenKind::Not,
                    loc,
                }) => UniOp::not(loc),
                _ => unreachable!(),
            };
            let e = parse_atom(tokens)?;
            let loc = op.loc.merge(&e.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_atom(tokens),
    }
}
fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Var(var) => Ok(Ast::var(var, tok.loc)),
            TokenKind::LParen => {
                let e = parse_expr(tokens)?;
                match tokens.next() {
                    Some(Token {
                        value: TokenKind::RParen,
                        ..
                    }) => Ok(e),
                    Some(t) => Err(ParseError::RedundantExpression(t)),
                    _ => Err(ParseError::UnclosedOpenParen(tok)),
                }
            }
            _ => Err(ParseError::NotExpression(tok)),
        })
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ErrorEnum {
    Lexer(LexError),
    Parser(ParseError),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Error {
    value: ErrorEnum,
}
impl From<LexError> for Error {
    fn from(e: LexError) -> Self {
        Error {
            value: ErrorEnum::Lexer(e),
        }
    }
}
impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error {
            value: ErrorEnum::Parser(e),
        }
    }
}
impl Error {
    pub fn name(&self) -> String {
        match &self.value {
            ErrorEnum::Lexer(l) => match l.value {
                LexErrorKind::InvalidChar(c) => format!("Invalid char: {}.", c),
                LexErrorKind::Eof => "End of file!".to_string(),
            },
            ErrorEnum::Parser(p) => match p {
                ParseError::UnclosedOpenParen(_) => "Parenthesis is not closed.".to_string(),
                ParseError::RedundantExpression(_) => "Redundant exprssion.".to_string(),
                ParseError::NotExpression(_) => "Not a start of expression".to_string(),
                ParseError::Eof => "End of file!".to_string(),
            },
        }
    }
    pub fn loc(&self) -> Option<(usize, usize)> {
        match &self.value {
            ErrorEnum::Lexer(l) => Some((l.loc.0, l.loc.1)),
            ErrorEnum::Parser(p) => match p {
                ParseError::UnclosedOpenParen(t) => Some((t.loc.0, t.loc.1)),
                ParseError::RedundantExpression(t) => Some((t.loc.0, t.loc.1)),
                ParseError::NotExpression(t) => Some((t.loc.0, t.loc.1)),
                ParseError::Eof => None,
            },
        }
    }
}
fn interpret(ast: Box<Ast>, var_vec: &Vec<BDD>) -> BDD {
    match ast.value {
        AstKind::Var(var) => var_vec[var.x() as usize],
        AstKind::UniOp { op, e } => match op {
            UniOp {
                value: UniOpKind::Not,
                ..
            } => !interpret(e, var_vec),
        },
        AstKind::BinOp { op, l, r } => match op {
            BinOp {
                value: BinOpKind::And,
                ..
            } => interpret(l, var_vec) & interpret(r, var_vec),
            BinOp {
                value: BinOpKind::Or,
                ..
            } => interpret(l, var_vec) | interpret(r, var_vec),
            BinOp {
                value: BinOpKind::Xor,
                ..
            } => interpret(l, var_vec) ^ interpret(r, var_vec),
        },
    }
}
pub fn parse(input: &str) -> Result<(Box<BDDArena>, BDD), Error> {
    let mut var_vec = vec![];
    let tokens = lex(input, &mut var_vec)?;
    let ast = parse_impl(tokens)?;
    let ast = Box::new(ast);
    let mut arena = Box::new(BDDArena::new());
    let mut vars = vec![];
    for var in var_vec.into_iter() {
        vars.push(arena.new_var(var));
    }

    Ok((arena, interpret(ast, &vars)))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer() {
        let exp = "!(x0 & x1 ^ x2 | x0)";
        let mut var_vec = vec![];
        assert_eq!(
            lex(exp, &mut var_vec).unwrap(),
            vec![
                Token::not(Loc(0, 1)),
                Token::lparen(Loc(1, 2)),
                Token::var(BoolVar::new(0), Loc(2, 4)),
                Token::and(Loc(5, 6)),
                Token::var(BoolVar::new(1), Loc(7, 9)),
                Token::xor(Loc(10, 11)),
                Token::var(BoolVar::new(2), Loc(12, 14)),
                Token::or(Loc(15, 16)),
                Token::var(BoolVar::new(0), Loc(17, 19)),
                Token::rparen(Loc(19, 20)),
            ]
        );
    }
    #[test]
    fn test_parser_impl() {
        let exp = "!(x0 & x1 ^ x2)";
        let mut var_vec = vec![];
        let tokens = lex(exp, &mut var_vec).unwrap();
        assert_eq!(
            parse_impl(tokens),
            Ok(Ast::uniop(
                UniOp::not(Loc(0, 1)),
                Ast::binop(
                    BinOp::xor(Loc(10, 11)),
                    Ast::binop(
                        BinOp::and(Loc(5, 6)),
                        Ast::var(BoolVar::new(0), Loc(2, 4)),
                        Ast::var(BoolVar::new(1), Loc(7, 9)),
                        Loc(2, 9)
                    ),
                    Ast::var(BoolVar::new(2), Loc(12, 14)),
                    Loc(2, 14)
                ),
                Loc(0, 14)
            ))
        );
    }
    #[test]
    fn test_parser() {
        let exp = "!(x0 & x1 ^ x2)";
        let result = parse(exp);
        assert!(result.is_ok());
        let (_arena, bdd) = result.unwrap();
        assert_eq!(4, bdd.num_answers());
    }
}
