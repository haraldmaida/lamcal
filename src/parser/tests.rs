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
            let variable: Var = Var::new(&name[..]);

            let display: String = variable.to_string();
            let (parsed, remaining) = super::variable().parse(&display[..]).unwrap();

            prop_assert_eq!(parsed, variable);
            prop_assert_eq!(remaining, "");
        }
    }
}

mod abstraction {

    use super::*;

    use syntax::{lam, var};

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
            let lambda: Expr = lam(param, var(body));

            let display: String = lambda.to_string();
            let (parsed, remaining) = expression().parse(&display[..]).unwrap();

            prop_assert_eq!(parsed, lambda);
            prop_assert_eq!(remaining, "");
        }
    }
}

mod application {

    use super::*;

    use syntax::{apply, lam, var};

    #[test]
    fn parse_empty_string() {
        let application = application().parse("");

        assert_that!(&application, eq(Err(StringStreamError::UnexpectedParse)));
    }

    proptest! {
        #[test]
        fn result_of_to_string_is_parseable_as_application(
            param in "[a-z][a-z0-9]*'*",
            body in "[a-z][a-z0-9]*'*",
            name in "[a-z][a-z0-9]*'*"
        ) {
            let lambda: Expr = lam(param, var(body));
            let z: Expr = var(name);
            let application: Expr = apply(lambda, z);

            let display: String = application.to_string();
            let (parsed, remaining) = expression().parse(&display[..]).unwrap();

            prop_assert_eq!(parsed, application);
            prop_assert_eq!(remaining, "");
        }
    }
}
