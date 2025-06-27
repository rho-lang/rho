use std::str::FromStr;

use thiserror::Error;

use crate::code::{Expr, Func, Literal, Stmt};

#[derive(Debug, Default)]
pub struct Source {
    inputs: Vec<String>,
    package: String,
    stmts: Vec<Stmt>,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("expect {0}")]
    Expect(&'static str),
    #[error("double package")]
    DoublePackage,
    #[error("invalid package")]
    InvalidPackage,
}

impl FromStr for Source {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut source = Self::default();
        source.parse(&mut trim(s))?;
        Ok(source)
    }
}

fn trim(mut s: &str) -> &str {
    while {
        s = s.trim_start();
        s.starts_with('#')
    } {
        (_, s) = s.split_once('\n').expect("ending newline")
    }
    s
}

impl Source {
    // for simplifying parsing, `s` must end with '\n'
    // the convention is to accept `s` without leading whitespace, but may produce
    // `s` with leading whitespace. it's caller's responsibility to `trim`` before
    // the following calling, if any
    fn parse(&mut self, s: &mut &str) -> Result<(), ParseError> {
        while !s.is_empty() {
            let stmts = self.parse_directive(s)?;
            self.stmts.extend(stmts);
            *s = trim(s)
        }
        Ok(())
    }

    // directives are always "in the front". they cannot be composed to the end of
    // something else
    fn parse_directive(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        for (prefix, method) in [
            (
                "!input",
                Self::parse_input as fn(&mut Self, &mut &str) -> Result<Vec<Stmt>, ParseError>,
            ),
            ("@export", Self::parse_export),
            ("@import", Self::parse_import),
            ("@package", Self::parse_package),
            ("break", |_, _| Ok(vec![Stmt::Break])),
            ("continue", |_, _| Ok(vec![Stmt::Continue])),
            ("let", Self::parse_assign),
            ("loop", Self::parse_loop),
            ("notify", Self::parse_notify),
            ("return", Self::parse_return),
            ("spawn", Self::parse_spawn),
            ("wait", Self::parse_wait),
        ] {
            if let Some(rest) = s.strip_prefix(prefix) {
                *s = trim(rest);
                return method(self, s);
            }
        }
        Ok(vec![Stmt::Expr(self.parse_expr(s)?)])
    }

    fn parse_expr(&mut self, s: &mut &str) -> Result<Expr, ParseError> {
        for (prefix, method) in [
            (
                "{",
                Self::parse_compound as fn(&mut Self, &mut &str) -> Result<Expr, ParseError>,
            ),
            ("\"", Self::parse_string_literal),
            ("func", Self::parse_func),
            ("future", |_, _| Ok(Expr::Future)),
            ("if", Self::parse_match),
            // just write `{}`
            // ("unit", |_, _| Ok(Expr::Literal(Literal::Unit))),
        ] {
            if let Some(rest) = s.strip_prefix(prefix) {
                *s = trim(rest);
                return method(self, s);
            }
        }
        if s.starts_with(|c: char| c.is_alphabetic()) {
            self.parse_var(s)
        } else {
            Err(ParseError::Expect("expression"))
        }
    }

    fn parse_input(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        let value;
        (value, *s) = s.split_once('\n').expect("ending newline");
        self.inputs.push(value.into());
        Ok(vec![])
    }

    fn parse_package(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        if !self.package.is_empty() {
            return Err(ParseError::DoublePackage);
        }
        let value;
        (value, *s) = s.split_once('\n').expect("ending newline");
        if value.is_empty() || value.contains(|c: char| !c.is_alphanumeric() && c != '.') {
            return Err(ParseError::InvalidPackage);
        }
        self.package = value.into();
        Ok(vec![])
    }

    fn parse_export(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        let id = extract_identifier(s)?;
        Ok(vec![Stmt::Export(id.into())])
    }

    fn parse_import(&mut self, _: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        todo!()
    }

    fn parse_assign(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        let id = extract_identifier(s)?;
        let Some(rest) = trim(s).strip_prefix('=') else {
            return Err(ParseError::Expect("="));
        };
        *s = trim(rest);
        Ok(vec![Stmt::Assign(id.into(), self.parse_expr(s)?)])
    }

    fn parse_loop(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        Ok(vec![Stmt::Loop(self.parse_expr(s)?)])
    }

    fn parse_notify(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        Ok(vec![Stmt::Notify(self.parse_expr(s)?)])
    }

    fn parse_return(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        Ok(vec![Stmt::Return(self.parse_expr(s)?)])
    }

    fn parse_spawn(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        Ok(vec![Stmt::Spawn(self.parse_expr(s)?)])
    }

    fn parse_wait(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        Ok(vec![Stmt::Wait(self.parse_expr(s)?)])
    }

    fn parse_compound(&mut self, s: &mut &str) -> Result<Expr, ParseError> {
        let mut stmts = Vec::new();
        loop {
            if let Some(rest) = s.strip_prefix('}') {
                *s = rest;
                break;
            }
            stmts.extend(self.parse_directive(s)?);
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

    fn parse_string_literal(&mut self, s: &mut &str) -> Result<Expr, ParseError> {
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

    fn parse_func(&mut self, s: &mut &str) -> Result<Expr, ParseError> {
        //
        Ok(Expr::Literal(Literal::Func(Func {
            params: vec![],
            body: self.parse_expr(s)?.into(),
        })))
    }

    fn parse_match(&mut self, _: &mut &str) -> Result<Expr, ParseError> {
        todo!()
    }

    fn parse_var(&mut self, s: &mut &str) -> Result<Expr, ParseError> {
        Ok(Expr::Var(extract_identifier(s)?.into()))
    }
}

fn extract_identifier<'a>(s: &mut &'a str) -> Result<&'a str, ParseError> {
    for (i, c) in s.char_indices() {
        if !c.is_alphanumeric() {
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
