use super::*;

use term::any_identifier;

mod var_name {

    use super::*;

    proptest! {
        #[test]
        fn display_string_is_the_variable_name(
            name in any_identifier()
        ) {
            let display = VarName(name.to_string()).to_string();

            prop_assert_eq!(display, name);
        }
    }
}

mod term {

    use super::*;

    proptest! {

        #[test]
        fn display_string_of_a_variable(
            name in any_identifier()
        ) {
            let name = &name[..];

            let display = var(name).to_string();

            prop_assert_eq!(display, name);
        }

        #[test]
        fn display_string_of_an_abstraction_with_variable_in_body(
            param in any_identifier(),
            body in any_identifier()
        ) {
            let param = &param[..]; let body = &body[..];

            let display = lam(param, var(body)).to_string();

            prop_assert_eq!(display, format!("λ{}.{}", param, body));
        }

        #[test]
        fn display_string_of_an_application(
            name1 in any_identifier(),
            name2 in any_identifier(),
        ) {
            let name1 = &name1[..]; let name2 = &name2[..];

            let display = app(lam(name1, var(name1)), var(name2)).to_string();

            prop_assert_eq!(display, format!("(λ{}.{}) {}", name1, name1, name2));
        }

        #[test]
        fn display_string_of_an_application_omitting_outermost_parens(
            name1 in any_identifier(),
            name2 in any_identifier(),
        ) {
            let name1 = &name1[..]; let name2 = &name2[..];

            let display = app(var(name1), var(name2)).to_string();

            prop_assert_eq!(display, format!("{} {}", name1, name2));
        }

        #[test]
        fn display_string_of_an_application_omitting_parens_in_abtraction_body(
            name1 in any_identifier(),
            name2 in any_identifier(),
            name3 in any_identifier(),
        ) {
            let name1 = &name1[..]; let name2 = &name2[..]; let name3 = &name3[..];

            let display = app(lam(name1, app(var(name1), var(name2))), var(name3)).to_string();

            prop_assert_eq!(display, format!("(λ{}.{} {}) {}", name1, name1, name2, name3));
        }

        #[test]
        fn display_string_of_an_application_of_an_application_must_not_omit_parens(
            name1 in any_identifier(),
            name2 in any_identifier(),
            name3 in any_identifier(),
        ) {
            let name1 = &name1[..]; let name2 = &name2[..]; let name3 = &name3[..];

            let display = app(var(name1), app(var(name2), var(name3))).to_string();

            prop_assert_eq!(display, format!("{} ({} {})", name1, name2, name3));
        }
    }

    #[test]
    fn free_vars_in_variable() {
        let expr = var("a");

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("a".into())));
        assert_eq!(free_vars.len(), 1);
    }

    #[test]
    fn free_vars_in_lambda_abstraction_contains_none() {
        let expr = lam("a", app(var("a"), var("a")));

        let free_vars = expr.free_vars();

        assert_eq!(free_vars.len(), 0);
    }

    #[test]
    fn free_vars_in_lambda_abstraction_contains_one() {
        let expr = lam("a", app(var("a"), var("b")));

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("b".into())));
        assert_eq!(free_vars.len(), 1);
    }

    #[test]
    fn free_vars_in_lambda_abstraction_contains_two() {
        let expr = lam("a", app![var("c"), var("b"), var("a")]);

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("b".into())));
        assert!(free_vars.contains(&VarName("c".into())));
        assert_eq!(free_vars.len(), 2);
    }

    #[test]
    fn free_vars_in_lambda_abstraction_within_lambda_abstraction_contains_one() {
        let expr = lam("a", lam("b", app![var("b"), var("a"), var("c")]));

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("c".into())));
        assert_eq!(free_vars.len(), 1);
    }

    #[test]
    fn free_vars_in_lambda_abstraction_on_third_level_contains_one() {
        let expr = lam(
            "a",
            lam("b", app![var("a"), lam("c", var("c")), var("d"), var("b")]),
        );

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("d".into())));
        assert_eq!(free_vars.len(), 1);
    }

    #[test]
    fn free_vars_in_function_application_contains_one_from_rhs() {
        let expr = app(lam("a", var("a")), var("b"));

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("b".into())));
        assert_eq!(free_vars.len(), 1);
    }

    #[test]
    fn free_vars_in_function_application_contains_one_from_lhs() {
        let expr = app(var("b"), lam("a", var("a")));

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("b".into())));
        assert_eq!(free_vars.len(), 1);
    }

    #[test]
    fn free_vars_in_function_application_of_vars_contains_two() {
        let expr = app(var("a"), var("b"));

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("a".into())));
        assert!(free_vars.contains(&VarName("b".into())));
        assert_eq!(free_vars.len(), 2);
    }

    #[test]
    fn free_vars_in_function_application_of_lambda_and_var_contains_two() {
        let expr = app(lam("a", app(var("c"), var("a"))), var("b"));

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("b".into())));
        assert!(free_vars.contains(&VarName("c".into())));
        assert_eq!(free_vars.len(), 2);
    }

    #[test]
    fn free_vars_in_function_application_of_two_closed_lambdas_contains_none() {
        let expr = app(lam("a", app(var("a"), var("a"))), lam("b", var("b")));

        let free_vars = expr.free_vars();

        assert_eq!(free_vars.len(), 0);
    }

    #[test]
    fn free_vars_in_function_application_of_two_lambdas_contains_three() {
        let expr = app(
            lam("a", app(var("b"), var("a"))),
            lam("b", app![var("c"), var("b"), var("a")]),
        );

        let free_vars = expr.free_vars();

        assert!(free_vars.contains(&VarName("a".into())));
        assert!(free_vars.contains(&VarName("b".into())));
        assert!(free_vars.contains(&VarName("c".into())));
        assert_eq!(free_vars.len(), 3);
    }
}

mod app_macro {

    use super::*;

    #[test]
    fn app_with_2_terms() {
        let expr = app!(var("a"), var("b"));

        assert_eq!(expr, app(var("a"), var("b")));
    }

    #[test]
    fn app_with_3_terms() {
        let expr = app!(var("a"), var("b"), var("c"));

        assert_eq!(expr, app(app(var("a"), var("b")), var("c")));
    }
}
//
//mod lam_macro {
//
//    use super::*;
//
//    #[test]
//    fn lam_with_2_params() {
//        let expr = lam!("x", "y", app(var("x"), var("y")));
//
//        assert_eq!(expr, lam("x", lam("y", app(var("x"), var("y")))));
//    }
//
//    #[test]
//    fn lam_with_3_params() {
//        let expr = lam!("x", "y", "z", app!(var("x"), var("y"), var("z")));
//
//        assert_eq!(
//            expr,
//            lam(
//                "x",
//                lam("y", lam("z", app(app(var("x"), var("y")), var("z"))))
//            )
//        );
//    }
//}
