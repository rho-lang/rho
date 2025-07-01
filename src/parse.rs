use std::{num::ParseIntError, str::FromStr};

use thiserror::Error;

use crate::code::{
    Expr, Func, Literal, Stmt,
    syntax::{Intrinsic, Match},
};

#[derive(Debug, Default)]
pub struct Source {
    pub inputs: Vec<String>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("expect {0}")]
    Expect(&'static str),
    #[error("invalid package")]
    InvalidPackage,
    #[error("{0}")]
    Int(#[from] ParseIntError),
}

impl FromStr for Source {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut source = Self::default();
        source.parse(&mut trim(s))?;
        Ok(source)
    }
}

impl Source {
    // for simplifying parsing, `s` must end with '\n'
    // the convention is to accept `s` without leading whitespace, but may produce
    // `s` with leading whitespace. it's caller's responsibility to `trim`` before
    // the following calling, if any
    fn parse(&mut self, s: &mut &str) -> Result<(), ParseError> {
        while !s.is_empty() {
            if let Some(rest) = s.strip_prefix("!input") {
                *s = trim(rest);
                self.parse_input(s)?
            } else {
                self.stmts.extend(parse_directive(s)?)
            }
            *s = trim(s)
        }
        Ok(())
    }

    fn parse_input(&mut self, s: &mut &str) -> Result<(), ParseError> {
        let value;
        (value, *s) = s.split_once('\n').expect("ending newline");
        self.inputs.push(value.into());
        Ok(())
    }
}

// directives are always "in the front". they cannot be composed to the end of
// something else
fn parse_directive(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    for (prefix, method) in [
        (
            "@export",
            parse_export as fn(&mut &str) -> Result<Vec<Stmt>, ParseError>,
        ),
        ("@import", parse_import),
        ("@package", parse_package),
        ("break", |_| Ok(vec![Stmt::Break])),
        ("continue", |_| Ok(vec![Stmt::Continue])),
        ("intrinsic", parse_intrinsic),
        ("let", parse_let),
        ("loop", parse_loop),
        ("notify", parse_notify),
        ("return", parse_return),
        ("spawn", parse_spawn),
        ("wait", parse_wait),
    ] {
        if let Some(rest) = s.strip_prefix(prefix) {
            *s = trim(rest);
            return method(s);
        }
    }
    let expr = parse_expr(s)?;
    let stmts = if let Expr::Var(id) = &expr
        && let Some(rest) = trim(s).strip_prefix('=')
    {
        *s = trim(rest);
        vec![Stmt::Mut(id.into(), parse_expr(s)?)]
    } else if let Some(rest) = trim(s).strip_prefix("mut") {
        *s = trim(rest);
        parse_mut(s, expr)?
    } else {
        vec![Stmt::Expr(expr)]
    };
    Ok(stmts)
}

fn parse_expr(s: &mut &str) -> Result<Expr, ParseError> {
    parse_expr0(s)
}

fn parse_expr0(s: &mut &str) -> Result<Expr, ParseError> {
    let expr1 = parse_expr1(s)?;
    parse_infix(s, expr1, ["!=", "==", ">=", "<=", ">", "<"], parse_expr1)
}

fn parse_expr1(s: &mut &str) -> Result<Expr, ParseError> {
    let expr1 = parse_expr2(s)?;
    parse_infix(s, expr1, ["+", "-"], parse_expr2)
}

fn parse_expr2(s: &mut &str) -> Result<Expr, ParseError> {
    let expr1 = parse_expr3(s)?;
    parse_infix(s, expr1, ["*", "/", "%"], parse_expr3)
}

fn parse_expr3(s: &mut &str) -> Result<Expr, ParseError> {
    let mut expr = parse_expr4(s)?;
    'expr: loop {
        *s = trim(s);
        for (prefix, method) in [
            (
                "(",
                parse_call as fn(&mut &str, Expr) -> Result<Expr, ParseError>,
            ),
            (".", parse_dot),
        ] {
            if let Some(rest) = s.strip_prefix(prefix) {
                *s = trim(rest);
                expr = method(s, expr)?;
                continue 'expr;
            }
        }
        break Ok(expr);
    }
}

fn parse_expr4(s: &mut &str) -> Result<Expr, ParseError> {
    for (prefix, method) in [
        (
            "{",
            parse_compound as fn(&mut &str) -> Result<Expr, ParseError>,
        ),
        ("\"", parse_string_literal),
        ("func", parse_func),
        ("signal", |_| Ok(Expr::Literal(Literal::Signal))),
        ("match", parse_match),
        ("new", parse_new),
        ("type", parse_type),
        // just write `{}`
        // ("unit", |_| Ok(Expr::Literal(Literal::Unit))),
    ] {
        if let Some(rest) = s.strip_prefix(prefix) {
            *s = trim(rest);
            return method(s);
        }
    }
    if s.starts_with(char::is_alphabetic) {
        parse_var(s)
    } else if s.starts_with(|c: char| c.is_ascii_digit()) {
        parse_i32(s)
    } else {
        Err(ParseError::Expect("atomic expression"))
    }
}

fn parse_export(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    let id = extract_identifier(s)?;
    Ok(vec![Stmt::Export(id.into())])
}

fn parse_import(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    let name = s
        .chars()
        .take_while(|&c| c.is_alphanumeric() || c == '.' || c == '_')
        .collect::<String>();
    if name.is_empty() {
        return Err(ParseError::InvalidPackage);
    }
    *s = s.strip_prefix(&name).unwrap();

    *s = consume(trim(s), '[', "imported name list")?;
    let imports = extract_delimited(s, |s| {
        let id = extract_identifier(s)?;
        let imported_id = if let Some(rest) = trim(s).strip_prefix("=") {
            *s = trim(rest);
            extract_identifier(s)?
        } else {
            id
        };
        Ok((id.into(), imported_id.into()))
    });
    *s = consume(trim(s), ']', "closing bracket of imported name list")?;
    let stmts = imports
        .into_iter()
        .map(|(id, imported_id)| Stmt::Bind(id, Expr::Import(name.clone(), imported_id)))
        .collect();
    Ok(stmts)
}

fn parse_package(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    let name = s
        .chars()
        .take_while(|&c| c.is_alphanumeric() || c == '.' || c == '_')
        .collect::<String>();
    if name.is_empty() {
        return Err(ParseError::InvalidPackage);
    }
    *s = s.strip_prefix(&name).unwrap();
    Ok(vec![Stmt::Package(name)])
}

fn parse_intrinsic(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    let id = extract_identifier(s)?.into();
    let mut bindings = Vec::new();
    if let Some(rest) = trim(s).strip_prefix('[') {
        *s = trim(rest);
        bindings = extract_delimited(s, |s| extract_identifier(s).map(Into::into));
        *s = consume(trim(s), ']', "closing bracket of intrinsic bindings")?
    }
    let mut args = Vec::new();
    if let Some(rest) = trim(s).strip_prefix('(') {
        *s = trim(rest);
        args = extract_delimited(s, parse_expr);
        *s = consume(
            trim(s),
            ')',
            "closing parenthesis of intrinsic argument list",
        )?
    }
    Ok(vec![Stmt::Intrinsic(Intrinsic { id, bindings, args })])
}

fn parse_let(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    let id = extract_identifier(s)?;
    *s = consume(trim(s), '=', "\"=\" in let binding")?;
    Ok(vec![Stmt::Bind(id.into(), parse_expr(s)?)])
}

fn parse_loop(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    Ok(vec![Stmt::Loop(parse_expr(s)?)])
}

fn parse_notify(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    Ok(vec![Stmt::Notify(parse_expr(s)?)])
}

fn parse_return(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    Ok(vec![Stmt::Return(parse_expr(s)?)])
}

fn parse_spawn(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    Ok(vec![Stmt::Spawn(parse_expr(s)?)])
}

fn parse_wait(s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
    Ok(vec![Stmt::Wait(parse_expr(s)?)])
}

fn parse_compound(s: &mut &str) -> Result<Expr, ParseError> {
    let mut stmts = Vec::new();
    loop {
        if let Some(rest) = s.strip_prefix('}') {
            *s = rest;
            break;
        }
        stmts.extend(parse_directive(s)?);
        *s = trim(s)
    }
    let expr = if let Some(Stmt::Expr(_)) = stmts.last() {
        let Some(Stmt::Expr(expr)) = stmts.pop() else {
            unreachable!()
        };
        expr
    } else {
        Expr::Literal(Literal::Unit)
    };
    Ok(Expr::Compound(stmts, expr.into()))
}

fn parse_string_literal(s: &mut &str) -> Result<Expr, ParseError> {
    let mut escaping = false;
    let mut literal = String::new();
    for (i, c) in s.char_indices() {
        if escaping {
            let cc = c; // TODO
            literal.push(cc);
            escaping = false;
            continue;
        }
        if c == '"' {
            // `i + 1` must be byte boundary; we know we are skipping a '"'
            (_, *s) = s.split_at(i + 1);
            return Ok(Expr::Literal(Literal::String(literal)));
        }
        if c == '\\' {
            escaping = true
        } else {
            literal.push(c)
        }
    }
    Err(ParseError::Expect("closing quote of string literal"))
}

fn parse_func(s: &mut &str) -> Result<Expr, ParseError> {
    *s = consume(s, '(', "function parameter list")?;
    let params = extract_delimited(s, |s| extract_identifier(s).map(Into::into));
    *s = consume(s, ')', "closing parenthesis of function parameter list")?;
    Ok(Expr::Func(Func {
        params,
        body: parse_expr(s)?.into(),
    }))
}

fn parse_match(s: &mut &str) -> Result<Expr, ParseError> {
    let scrutinee = parse_expr(s)?;
    let mut cases = Vec::new();
    while let Some(rest) = trim(s).strip_prefix("case") {
        *s = trim(rest);
        let pattern = parse_expr(s)?;
        *s = trim(s);
        let expr = parse_expr(s)?;
        cases.push((pattern, expr))
    }
    if cases.is_empty() {
        Err(ParseError::Expect("match case(s)"))
    } else {
        Ok(Expr::Match(Box::new(Match { scrutinee, cases })))
    }
}

fn parse_new(s: &mut &str) -> Result<Expr, ParseError> {
    let expr = parse_expr(s)?;
    let mut attrs = Vec::new();
    if let Some(rest) = trim(s).strip_prefix("[") {
        *s = trim(rest);
        // by accident these two have the same structure :)
        attrs = extract_delimited(s, parse_let)
            .into_iter()
            .flatten()
            .map(|stmt| {
                if let Stmt::Bind(id, expr) = stmt {
                    (id, expr)
                } else {
                    unreachable!()
                }
            })
            .collect();
        *s = consume(trim(s), ']', "closing bracket of record attribute list")?
    }
    Ok(Expr::Record(expr.into(), attrs))
}

fn parse_type(s: &mut &str) -> Result<Expr, ParseError> {
    let mut attrs = Vec::new();
    if let Some(rest) = s.strip_prefix('[') {
        *s = trim(rest);
        attrs = extract_delimited(s, |s| extract_identifier(s).map(Into::into));
        *s = consume(trim(s), ']', "closing bracket of attribute list")?;
    }
    Ok(Expr::Type(attrs))
}

fn parse_var(s: &mut &str) -> Result<Expr, ParseError> {
    Ok(Expr::Var(extract_identifier(s)?.into()))
}

fn parse_i32(s: &mut &str) -> Result<Expr, ParseError> {
    let value_str = s
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>();
    *s = s.strip_prefix(&value_str).unwrap();
    Ok(Expr::Literal(Literal::I32(value_str.parse()?)))
}

fn parse_call(s: &mut &str, closure: Expr) -> Result<Expr, ParseError> {
    let args = extract_delimited(s, parse_expr);
    *s = consume(trim(s), ')', "closing parenthesis of argument list")?;
    Ok(Expr::Call(closure.into(), args))
}

fn parse_dot(s: &mut &str, record: Expr) -> Result<Expr, ParseError> {
    let attr = extract_identifier(s)?;
    Ok(Expr::GetAttr(record.into(), attr.into()))
}

fn parse_mut(s: &mut &str, record: Expr) -> Result<Vec<Stmt>, ParseError> {
    *s = consume(trim(s), '.', "dot in record mutation")?;
    let attr = extract_identifier(s)?;
    *s = consume(trim(s), '=', "\"=\" in record mutation")?;
    Ok(vec![Stmt::MutAttr(record, attr.into(), parse_expr(s)?)])
}

fn parse_infix(
    s: &mut &str,
    lhs: Expr,
    ops: impl IntoIterator<Item = &'static str> + Clone,
    parse_rhs: fn(&mut &str) -> Result<Expr, ParseError>,
) -> Result<Expr, ParseError> {
    let mut expr = lhs;
    'expr: loop {
        for op in ops.clone() {
            if let Some(rest) = s.strip_prefix(op) {
                *s = trim(rest);
                expr = Expr::Op(op.into(), vec![expr, parse_rhs(s)?]);
                continue 'expr;
            }
        }
        break Ok(expr);
    }
}

// helpers
fn trim(mut s: &str) -> &str {
    while {
        s = s.trim_start();
        s.starts_with('#')
    } {
        (_, s) = s.split_once('\n').expect("ending newline")
    }
    s
}

fn consume<'a>(s: &'a str, prefix: char, expect: &'static str) -> Result<&'a str, ParseError> {
    if let Some(rest) = s.strip_prefix(prefix) {
        Ok(trim(rest))
    } else {
        Err(ParseError::Expect(expect))
    }
}

fn extract_identifier<'a>(s: &mut &'a str) -> Result<&'a str, ParseError> {
    for (i, c) in s.char_indices() {
        if !(c.is_alphanumeric() || c == '_') {
            let id;
            (id, *s) = s.split_at(i);
            return if i == 0 {
                Err(ParseError::Expect("identifier"))
            } else {
                Ok(id)
            };
        }
    }
    unreachable!("expect ending newline")
}

fn extract_delimited<T>(s: &mut &str, parse: fn(&mut &str) -> Result<T, ParseError>) -> Vec<T> {
    let Ok(item) = parse(s) else { return vec![] };
    let mut items = match consume(trim(s), ',', "(ignored)") {
        Ok(rest) => {
            *s = trim(rest);
            extract_delimited(s, parse)
        }
        Err(_) => Vec::new(),
    };
    items.insert(0, item);
    items
}
