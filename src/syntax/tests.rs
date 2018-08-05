use super::*;

mod var {

    use super::*;

    proptest! {
        #[test]
        fn display_string_is_the_variable_name(
            variable in "[a-z][a-z0-9]*"
        ) {
            let display = Var(variable.to_string()).to_string();

            prop_assert_eq!(display, variable);
        }
    }
}

mod expr {

    use super::*;

    proptest! {
        #[test]
        fn display_string_of_a_variable(
            variable in "[a-z][a-z0-9]*"
        ) {
            let display = Term::var(variable.to_string()).to_string();

            prop_assert_eq!(display, variable);
        }
    }

    proptest! {
        #[test]
        fn display_string_of_an_abstraction_with_variable_in_body(
            param in "[a-z][a-z0-9]*",
            body in "λ([a-z][a-z0-9]*)\\.(λ([a-z][a-z0-9]*)\\.)|([a-z][a-z0-9]*)"
        ) {
            let display = Term::lam(Var(param.to_string()), Term::var(body.to_string())).to_string();

            prop_assert_eq!(display, format!("λ{}.{}", param, body));
        }
    }

    proptest! {
        #[test]
        fn display_string_of_an_application(
            expr1 in "\\(λ([a-z][a-z0-9]*)\\.(λ([a-z][a-z0-9]*)\\.)|([a-z][a-z0-9]*)\\)",
            expr2 in "[a-z][a-z0-9]*"
        ) {
            let display = Term::app(Term::var(expr1.to_string()), Term::var(expr2.to_string())).to_string();

            prop_assert_eq!(display, format!("({}) {}", expr1, expr2));
        }
    }
}
