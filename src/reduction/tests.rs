use super::*;

mod alpha_enumerate {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn renames_bound_x_to_x1_in_lambda1() {
        let expr = app(lam("y", lam("x", app(var("x"), var("y")))), var("x"));
        let a_expr = app(lam("y", lam("x1", app(var("x1"), var("y")))), var("x"));

        let result = alpha::<Enumerate>(expr);

        assert_eq!(result, a_expr);
    }

    #[test]
    fn renames_bound_x_to_x1_in_lambda2() {
        let expr = app(lam("y", app(var("y"), var("x"))), var("x"));
        let a_expr = app(lam("y", app(var("y"), var("x1"))), var("x"));

        let result = alpha::<Enumerate>(expr);

        assert_eq!(result, a_expr);
    }

    #[test]
    fn renames_bound_x_to_x1_in_lambda3() {
        let expr = app(
            app(
                lam("y", app(var("y"), var("x"))),
                lam("y", app(var("y"), var("x"))),
            ),
            var("x"),
        );
        let a_expr = app(
            app(
                lam("y", app(var("y"), var("x1"))),
                lam("y", app(var("y"), var("x1"))),
            ),
            var("x"),
        );

        let result = alpha::<Enumerate>(expr);

        assert_eq!(result, a_expr);
    }

    #[test]
    fn renames_bound_x1_to_x2_in_lambda1() {
        let expr = app(lam("y", lam("x1", app(var("x1"), var("y")))), var("x1"));
        let a_expr = app(lam("y", lam("x2", app(var("x2"), var("y")))), var("x1"));

        let result = alpha::<Enumerate>(expr);

        assert_eq!(result, a_expr);
    }

    #[test]
    fn renames_bound_x19_to_x20_in_lambda1() {
        let expr = app(lam("y", lam("x19", app(var("x19"), var("y")))), var("x19"));
        let a_expr = app(lam("y", lam("x20", app(var("x20"), var("y")))), var("x19"));

        let result = alpha::<Enumerate>(expr);

        assert_eq!(result, a_expr);
    }

    #[test]
    fn renames_bound_x1x19_to_x1x20_in_lambda1() {
        let expr = app(
            lam("y", lam("x1x19", app(var("x1x19"), var("y")))),
            var("x1x19"),
        );
        let a_expr = app(
            lam("y", lam("x1x20", app(var("x1x20"), var("y")))),
            var("x1x19"),
        );

        let result = alpha::<Enumerate>(expr);

        assert_eq!(result, a_expr);
    }
}

mod beta {

    use term::{app, lam, var};

    #[test]
    fn test_for_beta_redex_on_identity_application() {
        let expr = app(lam("x", var("x")), var("a"));

        assert!(expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_redex_on_abstraction() {
        let expr = lam("x", var("x"));

        assert!(!expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_redex_on_variable() {
        let expr = var("x");

        assert!(!expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_redex_on_application_in_abstraction_body() {
        let expr = lam("x", app(var("x"), var("y")));

        assert!(!expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_redex_on_redex_in_abstraction_body() {
        let expr = lam("x", app(lam("y", var("y")), var("a")));

        assert!(expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_normal_on_identity_application() {
        let expr = app(lam("x", var("x")), var("a"));

        assert!(!expr.is_beta_normal());
    }

    #[test]
    fn test_for_beta_normal_on_abstraction() {
        let expr = lam("x", var("x"));

        assert!(expr.is_beta_normal());
    }

    #[test]
    fn test_for_beta_normal_on_variable() {
        let expr = var("x");

        assert!(expr.is_beta_normal());
    }

    #[test]
    fn test_for_beta_normal_on_application_in_abstraction_body() {
        let expr = lam("x", app(var("x"), var("y")));

        assert!(expr.is_beta_normal());
    }

    #[test]
    fn test_for_beta_normal_on_redex_in_abstraction_body() {
        let expr = lam("x", app(lam("y", var("y")), var("a")));

        assert!(!expr.is_beta_normal());
    }
}

mod beta_call_by_name {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn identity_x() {
        let expr = app(lam("x", var("x")), var("a"));

        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("a"));
    }

    #[test]
    fn constant_y() {
        let expr = app(lam("x", var("y")), var("a"));

        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("y"));
    }

    #[test]
    fn complex_term1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_term2() {
        // ((\a.a)\b.\c.b)(x)\e.f
        let expr = app(
            app(
                app(lam("a", var("a")), lam("b", lam("c", var("b")))),
                var("x"),
            ),
            lam("e", var("f")),
        );

        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("x"));
    }
}

mod beta_normal_order {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn identity_x() {
        let expr = app(lam("x", var("x")), var("a"));

        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("a"));
    }

    #[test]
    fn constant_y() {
        let expr = app(lam("x", var("y")), var("a"));

        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("y"));
    }

    #[test]
    fn complex_term1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_term2() {
        // ((\a.a)\b.\c.b)(x)\e.f
        let expr = app(
            app(
                app(lam("a", var("a")), lam("b", lam("c", var("b")))),
                var("x"),
            ),
            lam("e", var("f")),
        );

        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("x"));
    }
}
