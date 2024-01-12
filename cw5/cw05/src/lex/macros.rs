/// Match the head of a slice, return a tuple with the value we match to and the
/// head we matched.
///
/// # Example
/// ```no_run
/// let (n, head): (i32, &[u8]) = starts_with!(needle, {
///     b"head" => 0,
///     b"head2" => 1,
///     _ => 2,
/// })
/// ```
#[macro_export]
macro_rules! starts_with {
    ($slice:expr, {$($needle:expr => $result:expr),*,_ => $err:expr $(,)?}) => {
        if false {
            unreachable!()
        } $(else if $slice.starts_with($needle) {
            ($result, &$needle[..])
        })* else {
            #[allow(unreachable_code)]
            ($err, &b""[..])
        }
    }
}

/// Try lots of matcher functions on a `Lexer`, returning the most descriptive
/// error if all munchers fail.
///
/// # Example
/// ```no_run
/// try_all_munchers!(
///     lexer,
///     [
///         Self::munch_string,
///         Self::munch_bool,
///         Self::munch_char,
///         Self::munch_number,
///     ]
/// )
/// ```
#[macro_export]
macro_rules! try_all_munchers {
    ($lexer:expr, [$first_muncher:expr, $($muncher:expr),*$(,)?]) => {
        'noerr: {
            let mut err = match $first_muncher($lexer) {
                Ok(tok) => break 'noerr Ok(tok),
                Err(e) => e,
            };
            $(
                match $muncher($lexer) {
                    Ok(tok) => break 'noerr Ok(tok),
                    Err(e) => {
                        err.swap_if_better(e);
                    },
                }
            )*
            Err(err)
        }
    };
}

#[macro_export]
macro_rules! count_expr {
    ($($ignore:expr),*) => {
        $({
            _ = stringify!($ignore);
            1
        } +)* 0
    };
}

/// output a token or return an error. Roughly 'equivalent' to this
/// impossible function:
///
/// ```no_run
/// fn any_muncher(
///     lexer: &mut Lexer<'_>,
///     /// tokens are output here
///     tokens: &mut Vec<Token>,
///     /// Use the `AnyMuncher` as a fallback if this is false
///     strict: bool,
///     /// first match these, if one is valid, push it to `tokens`
///     greedy_munchers: [impl Muncher],
/// ) -> Result<(), LexingError>;
/// ```
#[macro_export]
macro_rules! any_muncher {
    (
        $lexer:expr,
        $tokens:expr,
        $strict:expr,
        [$FirstMuncher:expr $(,$($Muncher:expr),*)?$(,)?]$(,)?
    ) => {{
        let result: Result<(), LexingError> = 'noerr: {
            use either::Either;

            let mut expected = Vec::with_capacity($crate::count_expr!($($($Muncher),*)?));
            let mut err = match $FirstMuncher.munch($lexer) {
                Ok(tok) => {
                    $tokens.push(tok);
                    break 'noerr Ok(())
                }
                Err(e) => {
                    expected.push(e.expected_single());
                    e
                },
            };
            $($(
                match $Muncher.munch($lexer) {
                    Ok(tok) => {
                        $tokens.push(tok);
                        break 'noerr Ok(())
                    }
                    Err(e) => {
                        expected.push(e.expected_single());
                        err.swap_if_better(e);
                    },
                }
            )*)?
            err.expected = Either::Right(expected.into());
            if $strict {
                Err(err)
            } else {
                match AnyMuncher.munch($lexer) {
                    Ok(tok) => {
                        $tokens.push(tok);
                        break 'noerr Ok(())
                    }
                    Err(_) => unreachable!(),
                }
            }
        };
        result
    }
    };
}
