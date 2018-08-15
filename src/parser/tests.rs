use galvanic_assert::matchers::*;

use combine::error::StringStreamError;

use super::*;

mod variable {

    use super::*;

    #[test]
    fn parse_empty_string() {
        let variable = variable().parse("");

        assert_that!(&variable, eq(Err(StringStreamError::UnexpectedParse)));
    }

    proptest! {
        #[test]
        fn parse_one_letter_name(
            name in "[a-z]"
        ) {
            let variable = variable().parse(&name[..]);

            prop_assert_eq!(variable, Ok((Var(name.to_string()), "")));
        }

        #[test]
        fn parse_one_digit_name(
            name in "[0-9]"
        ) {
            let variable = variable().parse(&name[..]);

            prop_assert_eq!(variable, Err(StringStreamError::UnexpectedParse));
        }

        #[test]
        fn parse_multi_character_name(
            name in "[a-z][a-z0-9]*'*"
        ) {
            let variable = variable().parse(&name[..]);

            prop_assert_eq!(variable, Ok((Var(name.to_string()), "")));
        }

        #[test]
        fn result_of_to_string_is_parseable_as_variable(
            name in "[a-z][a-z0-9]*'*"
        ) {
            let variable = Var::new(&name[..]);

            let display: String = variable.to_string();
            let parsed = super::variable().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((variable, "")));
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
            let lambda: Term = lam(param, var(body));

            let display: String = lambda.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((lambda, "")));
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
            let lambda: Term = lam(param, var(body));
            let z: Term = var(name);
            let application: Term = app(lambda, z);

            let display: String = application.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((application, "")));
        }
    }

    proptest! {
        #[test]
        fn result_of_to_string_is_parseable_as_application_of_var_to_var(
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let var1: Term = var(name1);
            let var2: Term = var(name2);
            let application: Term = app(var1, var2);

            let display: String = application.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((application, "")));
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
            let application: Term = app(lambda1, lambda2);

            let display: String = application.to_string();
            let parsed = expression().parse(&display[..]);

            prop_assert_eq!(parsed, Ok((application, "")));
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
        fn parse_application_of_abstraction_to_abstraction_with_whitespaces(
            name1 in "[a-z][a-z0-9]*'*",
            name2 in "[a-z][a-z0-9]*'*"
        ) {
            let param1 = &name1[..];
            let param2 = &name2[..];
            let input = format!(" (  λ {} .  {}   {} )\t λ  {}  . {}", param1, param1, param1, param2, param2);

            let parsed = expression().parse(&input[..]);

            prop_assert_eq!(
                parsed,
                Ok((app(lam(param1, app(var(param1), var(param1))), lam(param2, var(param2))), ""))
            );
        }
    }
}
