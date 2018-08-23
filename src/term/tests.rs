use super::*;

mod var {

    use super::*;

    proptest! {
        #[test]
        fn display_string_is_the_variable_name(
            name in "[a-z][a-z0-9_']*"
        ) {
            let display = Var(name.to_string()).to_string();

            prop_assert_eq!(display, name);
        }
    }
}

mod term {

    use super::*;

    proptest! {

        #[test]
        fn display_string_of_a_variable(
            name in "[a-z][a-z0-9_']*"
        ) {
            let name = &name[..];

            let display = var(name).to_string();

            prop_assert_eq!(display, name);
        }

        #[test]
        fn display_string_of_an_abstraction_with_variable_in_body(
            param in "[a-z][a-z0-9_']*",
            body in "[a-z][a-z0-9_']*"
        ) {
            let param = &param[..]; let body = &body[..];

            let display = lam(param, var(body)).to_string();

            prop_assert_eq!(display, format!("λ{}.{}", param, body));
        }

        #[test]
        fn display_string_of_an_application(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
        ) {
            let name1 = &name1[..]; let name2 = &name2[..];

            let display = app(lam(name1, var(name1)), var(name2)).to_string();

            prop_assert_eq!(display, format!("(λ{}.{}) {}", name1, name1, name2));
        }

        #[test]
        fn display_string_of_an_application_omitting_outermost_parens(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
        ) {
            let name1 = &name1[..]; let name2 = &name2[..];

            let display = app(var(name1), var(name2)).to_string();

            prop_assert_eq!(display, format!("{} {}", name1, name2));
        }

        #[test]
        fn display_string_of_an_application_omitting_parens_in_abtraction_body(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            name3 in "[a-z][a-z0-9_']*",
        ) {
            let name1 = &name1[..]; let name2 = &name2[..]; let name3 = &name3[..];

            let display = app(lam(name1, app(var(name1), var(name2))), var(name3)).to_string();

            prop_assert_eq!(display, format!("(λ{}.{} {}) {}", name1, name1, name2, name3));
        }
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
