use super::*;

mod convert {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn renames_bound_x_to_x1_in_lambda1() {
        let expr = app(lam("y", lam("x", app(var("x"), var("y")))), var("x"));

        let a_expr = convert(expr);

        assert_eq!(a_expr.to_string(), "(λy.λx1.(x1) y) x");
    }

    #[test]
    fn renames_bound_x_to_x1_in_lambda2() {
        let expr = app(lam("y", app(var("y"), var("x"))), var("x"));

        let a_expr = convert(expr);

        assert_eq!(a_expr.to_string(), "(λy.(y) x1) x");
    }

    #[test]
    fn renames_bound_x_to_x1_in_lambda3() {
        let expr = app(
            lam("y", app(var("y"), var("x"))),
            lam("y", app(var("y"), var("x"))),
        );

        let a_expr = convert(expr);

        assert_eq!(a_expr.to_string(), "(λy.(y) x) λy.(y) x");
    }
}
