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
    #[error("unexpected source")]
    Unexpected,
    #[error("double package")]
    DoublePackage,
    #[error("invalid package")]
    InvalidPackage,
}

impl FromStr for Source {
    type Err = ParseError;

    fn from_str(mut s: &str) -> Result<Self, Self::Err> {
        let mut source = Self::default();
        source.parse(&mut trim(&mut s))?;
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
            ("unit", |_, _| Ok(Expr::Literal(Literal::Unit))),
        ] {
            if s.starts_with(prefix) {
                return method(self, s);
            }
        }
        if s.starts_with(|c: char| c.is_alphabetic()) {
            self.parse_var(s)
        } else {
            Err(ParseError::Unexpected)
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

    fn parse_import(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        Ok(vec![])
    }

    fn parse_assign(&mut self, s: &mut &str) -> Result<Vec<Stmt>, ParseError> {
        let id = extract_identifier(s)?;
        let Some(rest) = trim(s).strip_prefix('=') else {
            return Err(ParseError::Unexpected);
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
        //
        Ok(Expr::Compound(vec![], Expr::Literal(Literal::Unit).into()))
    }

    fn parse_string_literal(&mut self, s: &mut &str) -> Result<Expr, ParseError> {
        //
        Ok(Expr::Literal(Literal::String("".into())))
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
    for (i, c) in s.chars().enumerate() {
        if !c.is_alphanumeric() {
            let id;
            (id, *s) = s.split_at(i);
            return if i == 0 {
                Err(ParseError::Unexpected)
            } else {
                Ok(id)
            };
        }
    }
    unreachable!("expect ending newline")
}
