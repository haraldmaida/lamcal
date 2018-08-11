use galvanic_assert::matchers::*;

use super::*;

mod enumerate {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn renames_bound_x_to_x1_in_lambda1() {
        let expr = app(lam("y", lam("x", app(var("x"), var("y")))), var("x"));
        let a_expr = app(lam("y", lam("x1", app(var("x1"), var("y")))), var("x"));

        let result = alpha::<Enumerate, _>(expr);

        assert_that!(&result, eq(a_expr));
    }

    #[test]
    fn renames_bound_x_to_x1_in_lambda2() {
        let expr = app(lam("y", app(var("y"), var("x"))), var("x"));
        let a_expr = app(lam("y", app(var("y"), var("x1"))), var("x"));

        let result = alpha::<Enumerate, _>(expr);

        assert_that!(&result, eq(a_expr));
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

        let result = alpha::<Enumerate, _>(expr);

        assert_that!(&result, eq(a_expr));
    }

    #[test]
    fn renames_bound_x1_to_x2_in_lambda1() {
        let expr = app(lam("y", lam("x1", app(var("x1"), var("y")))), var("x1"));
        let a_expr = app(lam("y", lam("x2", app(var("x2"), var("y")))), var("x1"));

        let result = alpha::<Enumerate, _>(expr);

        assert_that!(&result, eq(a_expr));
    }

    #[test]
    fn renames_bound_x1x19_to_x1x20_in_lambda1() {
        let expr = app(lam("y", lam("x19", app(var("x19"), var("y")))), var("x19"));
        let a_expr = app(lam("y", lam("x20", app(var("x20"), var("y")))), var("x19"));

        let result = alpha::<Enumerate, _>(expr);

        assert_that!(&result, eq(a_expr));
    }
}
