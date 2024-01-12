use nom::bytes::complete::tag;
use nom::{IResult, InputTake};

use crate::lex::*;
use crate::parse::error::Error as InternalParseError;
use crate::parse::*;

#[allow(unused)]
pub(crate) fn tokvs<'a>(
    tokvs: &'a [Tokv],
) -> impl Fn(TokenStream<'a>) -> IResult<TokenStream<'_>, TokenStream<'_>> {
    tag(TokvStream(tokvs))
}

#[allow(unused)]
pub(crate) fn tokts<'a>(
    tokt: &'a [Tokt],
) -> impl Fn(TokenStream<'a>) -> IResult<TokenStream<'_>, TokenStream<'_>> {
    tag(ToktStream(tokt))
}

fn single<'a, 'b, T>(
    t: &'b T,
) -> impl Fn(TokenStream<'a>) -> IResult<TokenStream<'a>, &'a Token, InternalParseError<'a, 'b>>
where
    Token: PartialEq<T>,
    (&'a Token, &'b T): Into<InternalParseError<'a, 'b>>,
{
    move |toks: TokenStream<'_>| {
        if let Some(tok) = toks.0.first() {
            if tok == t {
                let (_, tail) = toks.take_split(1);
                Ok((tail, tok))
            } else {
                Err(nom::Err::Error((tok, t).into()))
            }
        } else {
            Err(nom::Err::Error(InternalParseError::reached_eof()))
        }
    }
}

/// Consume a single token that matches a given [`Tokv`].
/// than
pub(crate) fn tokv<'a, 'b>(
    tokv: &'b Tokv,
) -> impl Fn(TokenStream<'a>) -> IResult<TokenStream<'a>, &'a Token, InternalParseError<'a, 'b>>
where
    'a: 'b,
{
    single(tokv)
}

/// Consume a single token that matches a given [`Tokt`]
pub(crate) fn tokt<'a, 'b>(
    tokt: &'b Tokt,
) -> impl Fn(TokenStream<'a>) -> IResult<TokenStream<'a>, &'a Token, InternalParseError<'a, 'b>>
where
    'a: 'b,
{
    single(tokt)
}
