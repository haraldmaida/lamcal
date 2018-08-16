use galvanic_assert::matchers::*;

use combine::error::StringStreamError;

use super::*;

mod variable {

    use super::*;

    #[test]
    fn parse_empty_string() {
        let parsed = variable().parse("");

        assert_that!(&parsed, eq(Err(StringStreamError::UnexpectedParse)));
    }

    proptest! {
        #[test]
        fn parse_one_letter_name(
            name in "[a-z]"
        ) {
            let parsed = variable().parse(&name[..]);

            prop_assert_eq!(parsed, Ok((Var(name.to_string()), "")));
        }

        #[test]
        fn parse_one_digit_name(
            name in "[0-9]"
        ) {
            let parsed = variable().parse(&name[..]);

            prop_assert_eq!(parsed, Err(StringStreamError::UnexpectedParse));
        }

        #[test]
        fn parse_multi_character_name(
            name in "[a-z][a-z0-9]*'*"
        ) {
            let parsed = variable().parse(&name[..]);

            prop_assert_eq!(parsed, Ok((Var(name.to_string()), "")));
        }

        #[test]
        fn result_of_to_string_is_parseable_as_variable(
            name in "[a-z][a-z0-9]*'*"
        ) {
            let expr = Var::new(&name[..]);

            let display: String = expr.to_string();
            let parsed = variable().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((expr, "")));
        }
    }
}

mod abstraction {

    use super::*;

    use term::{lam, var};

    #[test]
    fn parse_empty_string() {
        let lambda = abstraction().parse("");

        assert_that!(&lambda, eq(Err(StringStreamError::UnexpectedParse)));
    }

    proptest! {
        #[test]
        fn result_of_to_string_is_parseable_as_abstraction(
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*"
        ) {
            let expr: Term = lam(param, var(body));

            let display: String = expr.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((expr, "")));
        }
    }

    proptest! {
        #[test]
        fn ignores_whitespace_between_lambda_and_bound_variable(
            whitespace in "[\\s]*",
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*"
        ) {
            let input = format!("λ{}{}.{}", whitespace, param, body);

            let bound = Var::new(param.to_string());
            let expr = var(body);

            let parsed = abstraction().parse(&input[..]);

            prop_assert_eq!(parsed, Ok(((bound, expr), "")));
        }
    }

    proptest! {
        #[test]
        fn ignores_whitespace_between_dot_and_body(
            whitespace in "[\\s]*",
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*"
        ) {
            let input = format!("λ{}.{}{}", param, whitespace, body);

            let bound = Var::new(param.to_string());
            let expr = var(body);

            let parsed = abstraction().parse(&input[..]);

            prop_assert_eq!(parsed, Ok(((bound, expr), "")));
        }
    }
}

mod application {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn parse_empty_string() {
        let application = application().parse("");

        assert_that!(&application, eq(Err(StringStreamError::UnexpectedParse)));
    }

    proptest! {
        #[test]
        fn result_of_to_string_is_parseable_as_application_of_var_to_lambda(
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*",
            name in "[a-z][a-z0-9]*'*"
        ) {
            let expr: Term = app(lam(param, var(body)), var(name));

            let display: String = expr.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((expr, "")));
        }
    }

    proptest! {
        #[test]
        fn result_of_to_string_is_parseable_as_application_of_var_to_var(
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let expr: Term = app(var(name1), var(name2));

            let display: String = expr.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((expr, "")));
        }
    }

    proptest! {
        #[test]
        fn result_of_to_string_is_parseable_as_application_of_lambda_to_lambda(
            param1 in "[a-z][a-z0-9]*'*",
            body1 in "[a-z][a-z0-9]*'*",
            param2 in "[a-z][a-z0-9]*'*",
            body2 in "[a-z][a-z0-9]*'*"
        ) {
            let lambda1: Term = lam(param1, var(body1));
            let lambda2: Term = lam(param2, var(body2));
            let expr: Term = app(lambda1, lambda2);

            let display: String = expr.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((expr, "")));
        }
    }

    proptest! {
        #[test]
        fn ignores_whitespace_between_open_paren_and_expression(
            whitespace in "[\\s]*",
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*",
            name in "[a-z][a-z0-9]*'*"
        ) {
            let input = format!("({}λ{}.{}){}", whitespace, param, body, name);

            let expr1 = lam(param, var(body));
            let expr2 = var(name);

            let parsed = application().parse(&input[..]);

            prop_assert_eq!(parsed, Ok(((expr1, expr2), "")));
        }
    }

    proptest! {
        #[test]
        fn ignores_whitespace_between_expression_and_closing_paren(
            whitespace in "[\\s]*",
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*",
            name in "[a-z][a-z0-9]*'*"
        ) {
            let input = format!("(λ{}.{}{}){}", param, body, whitespace, name);

            let expr1 = lam(param, var(body));
            let expr2 = var(name);

            let parsed = application().parse(&input[..]);

            prop_assert_eq!(parsed, Ok(((expr1, expr2), "")));
        }
    }

    proptest! {
        #[test]
        fn ignores_whitespace_between_closing_paren_and_expr2(
            whitespace in "[\\s]*",
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*",
            name in "[a-z][a-z0-9]*'*"
        ) {
            let input = format!("(λ{}.{}){}{}", param, body, whitespace, name);

            let expr1 = lam(param, var(body));
            let expr2 = var(name);

            let parsed = application().parse(&input[..]);

            prop_assert_eq!(parsed, Ok(((expr1, expr2), "")));
        }
    }
}

mod expression {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn parse_empty_string() {
        let expr = expression().parse("");

        assert_that!(&expr, eq(Err(StringStreamError::UnexpectedParse)));
    }

    proptest! {
        #[test]
        fn parse_variable_name(
            name in "[a-z][a-z0-9]*'*"
        ) {
            let parsed = expression().parse(&name[..]);

            prop_assert_eq!(parsed, Ok((var(&name[..]), "")));
        }
    }

    proptest! {
        #[test]
        fn parse_sequence_of_three_variables(
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*",
            name3 in "[a-z][a-z0-9]*'*"
        ) {
            let name1 = &name1[..];
            let name2 = &name2[..];
            let name3 = &name3[..];
            let input = format!("{} {} {}", name1, name2, name3);
            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(parsed, Ok((app(app(var(name1), var(name2)), var(name3)), "")));
        }
    }

    proptest! {
        #[test]
        fn parse_identity(
            name in "[a-z][a-z0-9]*'*"
        ) {
            let param = &name[..];
            let input = format!("λ{}.{}", param, param);

            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(parsed, Ok((lam(param, var(param)), "")));
        }
    }

    proptest! {
        #[test]
        fn parse_abstraction_of_abstraction_with_application_in_body(
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let param1 = &name1[..];
            let param2 = &name2[..];
            let input = format!("λ{}.λ{}.{} {}", param2, param1, param2, param1);

            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(
                parsed,
                Ok((lam(param2, lam(param1, app(var(param2), var(param1)))), ""))
            );
        }
    }

    proptest! {
        #[test]
        fn parse_application_of_variable_to_abstraction(
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let param1 = &name1[..];
            let param2 = &name2[..];
            let input = format!("(λ{}.{} {}) {}", param1, param1, param2, param2);

            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(
                parsed,
                Ok((app(lam(param1, app(var(param1), var(param2))), var(param2)), ""))
            );
        }
    }

    proptest! {
        #[test]
        fn parse_application_of_abstraction_to_abstraction(
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let param1 = &name1[..];
            let param2 = &name2[..];
            let input = format!("(λ{}.{} {}) λ{}.{}", param1, param1, param1, param2, param2);

            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(
                parsed,
                Ok((app(lam(param1, app(var(param1), var(param1))), lam(param2, var(param2))), ""))
            );
        }
    }

    proptest! {
        #[test]
        fn ignores_leading_whitespace(
            whitespace in "[\\s]*",
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let param1 = &name1[..];
            let param2 = &name2[..];
            let input = format!("{}(λ{}.{} {}) λ{}.{}", whitespace, param1, param1, param1, param2, param2);

            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(
                parsed,
                Ok((app(lam(param1, app(var(param1), var(param1))), lam(param2, var(param2))), ""))
            );
        }
    }

    proptest! {
        #[test]
        fn ignores_trailing_whitespace(
            whitespace in "[\\s]*",
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let param1 = &name1[..];
            let param2 = &name2[..];
            let input = format!("(λ{}.{} {}) λ{}.{}{}", param1, param1, param1, param2, param2, whitespace);

            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(
                parsed,
                Ok((app(lam(param1, app(var(param1), var(param1))), lam(param2, var(param2))), ""))
            );
        }
    }
}
