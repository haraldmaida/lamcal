use super::*;

mod alpha {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn renames_bound_x_in_second_level_lambda_abstraction() {
        let expr = lam("y", lam("x", app(var("x"), var("y"))));
        let expected = lam("y", lam("x1", app(var("x1"), var("y"))));

        let result = alpha::<Enumerate>(&expr, &var("x").free_vars());

        assert_eq!(result, expected);
    }

    #[test]
    fn does_not_rename_free_x_in_body() {
        let expr = lam("y", app(var("x"), var("y")));
        let expected = lam("y", app(var("x"), var("y")));

        let result = alpha::<Enumerate>(&expr, &var("x").free_vars());

        assert_eq!(result, expected);
    }

    #[test]
    fn does_not_rename_free_x_in_all_bodies() {
        let expr = app(
            lam("y", app(var("y"), var("x"))),
            lam("y", app(var("y"), var("x"))),
        );
        let expected = app(
            lam("y", app(var("y"), var("x"))),
            lam("y", app(var("y"), var("x"))),
        );

        let result = alpha::<Enumerate>(&expr, &var("x").free_vars());

        assert_eq!(result, expected);
    }

    #[test]
    fn renames_bound_x_to_x2_when_x1_is_free_variable_in_body() {
        let expr = lam("y", lam("x", app![var("x"), var("x1"), var("y")]));
        let expected = lam("y", lam("x2", app![var("x2"), var("x1"), var("y")]));

        let result = alpha::<Enumerate>(&expr, &var("x").free_vars());

        assert_eq!(result, expected);
    }

    #[test]
    fn renames_outer_bound_x_and_shadowing_inner_bound_x() {
        let expr = lam("x", lam("x", app![var("z"), var("y"), var("x")]));
        let expected = lam("x1", lam("x1", app![var("z"), var("y"), var("x1")]));

        let result = alpha::<Enumerate>(&expr, &var("x").free_vars());

        assert_eq!(result, expected);
    }
}

mod alpha_rename_enumerate {

    use super::*;

    use std::u32;

    proptest! {

        #[test]
        fn appends_digit_1_if_name_does_not_end_with_digit(
            name in "[a-z][a-zA-Z0-9_']*[a-zA-Z_']",
        ) {
            let expected = name.clone() + "1";
            let mut name = name.clone();

            Enumerate::rename(&mut name);

            prop_assert_eq!(name, expected);
        }

        #[test]
        fn increases_number_by_1_if_name_ends_with_number(
            name in "[a-z][a-zA-Z0-9_']*[a-zA-Z_']",
            digit in 0..u32::MAX,
        ) {
            let expected = name.clone() + &(digit + 1).to_string();
            let mut name = name.clone() + &digit.to_string();

            Enumerate::rename(&mut name);

            prop_assert_eq!(name, expected);
        }
    }
}

mod alpha_rename_prime {

    use super::*;

    proptest! {

        #[test]
        fn appends_tick_symbol(
            name in "[a-z][a-zA-Z0-9_']*",
        ) {
            let expected = name.clone() + "'";
            let mut name = name.clone();

            Prime::rename(&mut name);

            prop_assert_eq!(name, expected);
        }

        #[test]
        fn appends_another_tick_symbol_if_name_ends_with_tick(
            name in "[a-z][a-zA-Z0-9_']*",
        ) {
            let expected = name.clone() + "''";
            let mut name = name.clone() + "'";

            Prime::rename(&mut name);

            prop_assert_eq!(name, expected);
        }
    }
}

mod apply {

    use super::*;

    use term::{app, lam, var};

    #[test]
    fn applying_variable_returns_the_variable() {
        let expr = var("x");

        let result = apply::<Enumerate>(&expr, &var("y"));

        assert_eq!(result, var("x"));
    }

    #[test]
    fn applying_identity_returns_rhs() {
        let expr = lam("x", var("x"));

        let result = apply::<Enumerate>(&expr, &var("y"));

        assert_eq!(result, var("y"));
    }

    #[test]
    fn applying_constant_returns_constant() {
        let expr = lam("x", var("y"));

        let result = apply::<Enumerate>(&expr, &var("a"));

        assert_eq!(result, var("y"));
    }

    #[test]
    fn applying_replication_returns_application_of_rhs() {
        let expr = lam("x", app(var("x"), var("x")));

        let result = apply::<Enumerate>(&expr, &var("a"));

        assert_eq!(result, app(var("a"), var("a")));
    }

    #[test]
    fn applying_simple_abstraction() {
        let expr = lam("x", app(var("x"), var("y")));

        let result = apply::<Enumerate>(&expr, &var("a"));

        assert_eq!(result, app(var("a"), var("y")));
    }

    #[test]
    fn applying_abstraction_of_abstraction() {
        let expr = lam("x", lam("y", app(var("y"), var("x"))));

        let result = apply::<Enumerate>(&expr, &var("a"));

        assert_eq!(result, lam("y", app(var("y"), var("a"))));
    }

    #[test]
    fn applying_abstraction_of_abstraction_with_same_bound_var() {
        let expr = lam("x", lam("x", app(var("x"), var("y"))));

        let result = apply::<Enumerate>(&expr, &var("a"));

        assert_eq!(result, lam("x", app(var("x"), var("y"))));
    }
}

mod beta_reduction_properties {

    use term::{app, lam, var};

    #[test]
    fn test_for_beta_redex_on_variable() {
        let expr = var("x");

        assert!(!expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_redex_on_abstraction() {
        let expr = lam("x", var("x"));

        assert!(!expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_redex_on_identity_application() {
        let expr = app(lam("x", var("x")), var("a"));

        assert!(expr.is_beta_redex());
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
    fn test_for_beta_redex_on_application_with_redex_on_left_hand_side() {
        let expr = app(app(lam("x", var("x")), var("a")), var("b"));

        assert!(expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_redex_on_application_with_redex_on_right_hand_side() {
        let expr = app(var("b"), app(lam("x", var("x")), var("a")));

        assert!(expr.is_beta_redex());
    }

    #[test]
    fn test_for_beta_normal_on_variable() {
        let expr = var("x");

        assert!(expr.is_beta_normal());
    }

    #[test]
    fn test_for_beta_normal_on_abstraction() {
        let expr = lam("x", var("x"));

        assert!(expr.is_beta_normal());
    }

    #[test]
    fn test_for_beta_normal_on_identity_application() {
        let expr = app(lam("x", var("x")), var("a"));

        assert!(!expr.is_beta_normal());
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

    #[test]
    fn test_for_beta_normal_on_application_with_redex_on_left_hand_side() {
        let expr = app(app(lam("x", var("x")), var("a")), var("b"));

        assert!(!expr.is_beta_normal());
    }

    #[test]
    fn test_for_beta_normal_on_application_with_redex_on_right_hand_side() {
        let expr = app(var("b"), app(lam("x", var("x")), var("a")));

        assert!(!expr.is_beta_normal());
    }
}

mod beta_call_by_name {

    use super::*;

    use term::{any_term, app, lam, var};

    proptest! {
        #[test]
        fn reduce_any_expr_compared_to_recursive_impl(
            expr in any_term()
        ) {
            let mut expected = expr.clone();
            CallByName::<Enumerate>::reduce_rec(&mut expected);

            let result = CallByName::<Enumerate>::reduce(expr.clone());

            prop_assert_eq!(result, expected);
        }
    }

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
    fn application_of_free_variable() {
        let expr = app(var("x"), app(lam("z", var("z")), var("v")));

        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("x"), app(lam("z", var("z")), var("v"))));
    }

    #[test]
    fn application_of_free_variables_2() {
        let expr = app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")));

        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(
            reduced,
            app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")))
        );
    }

    #[test]
    fn complex_expr1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_expr2() {
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

    #[test]
    fn complex_expr3() {
        // ( \x.(\y.x (\z.z) y) ) ( (\a.a) (\b.b) )
        let expr = app(
            lam(
                "x",
                lam("y", app(var("x"), app(lam("z", var("z")), var("y")))),
            ),
            app(lam("a", var("a")), lam("b", var("b"))),
        );

        let reduced = CallByName::<Enumerate>::reduce(expr);

        assert_eq!(
            reduced,
            lam(
                "y",
                app![
                    lam("a", var("a")),
                    lam("b", var("b")),
                    app(lam("z", var("z")), var("y"))
                ]
            )
        );
    }
}

mod beta_normal_order {

    use super::*;

    use term::{any_term, app, lam, var};

    proptest! {
        #[test]
        fn reduce_any_expr_compared_to_recursive_impl(
            expr in any_term()
        ) {
            let mut expected = expr.clone();
            NormalOrder::<Enumerate>::reduce_rec(&mut expected);

            let result = NormalOrder::<Enumerate>::reduce(expr.clone());
            prop_assert_eq!(result, expected);
        }
    }

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
    fn application_of_free_variable() {
        let expr = app(var("x"), app(lam("z", var("z")), var("v")));

        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("x"), var("v")));
    }

    #[test]
    fn application_of_free_variables_2() {
        let expr = app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")));

        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(app(var("x"), var("y")), var("v")));
    }

    #[test]
    fn complex_expr1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_expr2() {
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

    #[test]
    fn complex_expr3() {
        // ( \x.(\y.x (\z.z) y) ) ( (\a.a) (\b.b) )
        let expr = app(
            lam(
                "x",
                lam("y", app(var("x"), app(lam("z", var("z")), var("y")))),
            ),
            app(lam("a", var("a")), lam("b", var("b"))),
        );

        let reduced = NormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("y", var("y")));
    }
}

mod beta_call_by_value {

    use super::*;

    use term::{any_term, app, lam, var};

    proptest! {
        #[test]
        fn reduce_any_expr_compared_to_recursive_impl(
            expr in any_term()
        ) {
            let mut expected = expr.clone();
            CallByValue::<Enumerate>::reduce_rec(&mut expected);

            let result = CallByValue::<Enumerate>::reduce(expr.clone());

            prop_assert_eq!(result, expected);
        }
    }

    #[test]
    fn identity_x() {
        let expr = app(lam("x", var("x")), var("a"));

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("a"));
    }

    #[test]
    fn constant_y() {
        let expr = app(lam("x", var("y")), var("a"));

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("y"));
    }

    #[test]
    fn application_of_free_variable() {
        let expr = app(var("x"), app(lam("z", var("z")), var("v")));

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("x"), var("v")));
    }

    #[test]
    fn application_of_free_variables_2() {
        let expr = app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")));

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(app(var("x"), var("y")), var("v")));
    }

    #[test]
    fn complex_expr1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_expr2() {
        // ((\a.a)\b.\c.b)(x)\e.f
        let expr = app(
            app(
                app(lam("a", var("a")), lam("b", lam("c", var("b")))),
                var("x"),
            ),
            lam("e", var("f")),
        );

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("x"));
    }

    #[test]
    fn complex_expr3() {
        // ( \x.(\y.x (\z.z) y) ) ( (\a.a) (\b.b) )
        let expr = app(
            lam(
                "x",
                lam("y", app(var("x"), app(lam("z", var("z")), var("y")))),
            ),
            app(lam("a", var("a")), lam("b", var("b"))),
        );

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(
            reduced,
            lam(
                "y",
                app(lam("b", var("b")), app(lam("z", var("z")), var("y")))
            )
        );
    }

    #[test]
    fn complex_expr4() {
        // (\a.a) (A 0)
        let expr = app(lam("a", var("a")), app(var("A"), var("0")));

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("A"), var("0")));
    }

    #[test]
    fn complex_expr5() {
        //
        let expr = app(app(var("0"), app(lam("A", var("a")), var("A"))), var("0"));

        let reduced = CallByValue::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(app(var("0"), var("a")), var("0")));
    }
}

mod beta_applicative_order {

    use super::*;

    use term::{any_term, app, lam, var};

    proptest! {
        #[test]
        fn reduce_any_expr_compared_to_recursive_impl(
            expr in any_term()
        ) {
            let mut expected = expr.clone();
            ApplicativeOrder::<Enumerate>::reduce_rec(&mut expected);

            let result = ApplicativeOrder::<Enumerate>::reduce(expr.clone());

            prop_assert_eq!(result, expected);
        }
    }

    #[test]
    fn identity_x() {
        let expr = app(lam("x", var("x")), var("a"));

        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("a"));
    }

    #[test]
    fn constant_y() {
        let expr = app(lam("x", var("y")), var("a"));

        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("y"));
    }

    #[test]
    fn application_of_free_variable() {
        let expr = app(var("x"), app(lam("z", var("z")), var("v")));

        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("x"), var("v")));
    }

    #[test]
    fn application_of_free_variables_2() {
        let expr = app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")));

        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(app(var("x"), var("y")), var("v")));
    }

    #[test]
    fn complex_expr1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_expr2() {
        // ((\a.a)\b.\c.b)(x)\e.f
        let expr = app(
            app(
                app(lam("a", var("a")), lam("b", lam("c", var("b")))),
                var("x"),
            ),
            lam("e", var("f")),
        );

        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("x"));
    }

    #[test]
    fn complex_expr3() {
        // ( \x.(\y.x (\z.z) y) ) ( (\a.a) (\b.b) )
        let expr = app(
            lam(
                "x",
                lam("y", app(var("x"), app(lam("z", var("z")), var("y")))),
            ),
            app(lam("a", var("a")), lam("b", var("b"))),
        );

        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("y", var("y")));
    }

    #[test]
    fn complex_expr4() {
        // ( \x.\y.z ) ( z ((\x.u) y) )
        let expr = app(
            lam("x", lam("y", var("z"))),
            app(var("z"), app(lam("x", var("u")), var("y"))),
        );

        let reduced = ApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("y", var("z")));
    }
}

mod beta_hybrid_applicative_order {

    use super::*;

    use term::{any_term, app, lam, var};

    proptest! {
        #[test]
        fn reduce_any_expr_compared_to_recursive_impl(
            expr in any_term()
        ) {
            let mut expected = expr.clone();
            HybridApplicativeOrder::<Enumerate>::reduce_rec(&mut expected);

            let result = HybridApplicativeOrder::<Enumerate>::reduce(expr.clone());

            prop_assert_eq!(result, expected);
        }
    }

    #[test]
    fn identity_x() {
        let expr = app(lam("x", var("x")), var("a"));

        let reduced = HybridApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("a"));
    }

    #[test]
    fn constant_y() {
        let expr = app(lam("x", var("y")), var("a"));

        let reduced = HybridApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("y"));
    }

    #[test]
    fn application_of_free_variable() {
        let expr = app(var("x"), app(lam("z", var("z")), var("v")));

        let reduced = HybridApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("x"), var("v")));
    }

    #[test]
    fn application_of_free_variables_2() {
        let expr = app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")));

        let reduced = HybridApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(app(var("x"), var("y")), var("v")));
    }

    #[test]
    fn complex_expr1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = HybridApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_expr2() {
        // ((\a.a)\b.\c.b)(x)\e.f
        let expr = app(
            app(
                app(lam("a", var("a")), lam("b", lam("c", var("b")))),
                var("x"),
            ),
            lam("e", var("f")),
        );

        let reduced = HybridApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("x"));
    }

    #[test]
    fn complex_expr3() {
        // ( \x.(\y.x (\z.z) y) ) ( (\a.a) (\b.b) )
        let expr = app(
            lam(
                "x",
                lam("y", app(var("x"), app(lam("z", var("z")), var("y")))),
            ),
            app(lam("a", var("a")), lam("b", var("b"))),
        );

        let reduced = HybridApplicativeOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("y", var("y")));
    }
}

mod beta_head_spine_order {

    use super::*;

    use term::{any_term, app, lam, var};

    proptest! {
        #[test]
        fn reduce_any_expr_compared_to_recursive_impl(
            expr in any_term()
        ) {
            let mut expected = expr.clone();
            HeadSpine::<Enumerate>::reduce_rec(&mut expected);

            let result = HeadSpine::<Enumerate>::reduce(expr.clone());

            prop_assert_eq!(result, expected);
        }
    }

    #[test]
    fn identity_x() {
        let expr = app(lam("x", var("x")), var("a"));

        let reduced = HeadSpine::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("a"));
    }

    #[test]
    fn constant_y() {
        let expr = app(lam("x", var("y")), var("a"));

        let reduced = HeadSpine::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("y"));
    }

    #[test]
    fn application_of_free_variable() {
        let expr = app(var("x"), app(lam("z", var("z")), var("v")));

        let reduced = HeadSpine::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("x"), app(lam("z", var("z")), var("v"))));
    }

    #[test]
    fn application_of_free_variables_2() {
        let expr = app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")));

        let reduced = HeadSpine::<Enumerate>::reduce(expr);

        assert_eq!(
            reduced,
            app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")))
        );
    }

    #[test]
    fn complex_expr1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = HeadSpine::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_expr2() {
        // ((\a.a)\b.\c.b)(x)\e.f
        let expr = app(
            app(
                app(lam("a", var("a")), lam("b", lam("c", var("b")))),
                var("x"),
            ),
            lam("e", var("f")),
        );

        let reduced = HeadSpine::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("x"));
    }

    #[test]
    fn complex_expr3() {
        // ( \x.(\y.x (\z.z) y) ) ( (\a.a) (\b.b) )
        let expr = app(
            lam(
                "x",
                lam("y", app(var("x"), app(lam("z", var("z")), var("y")))),
            ),
            app(lam("a", var("a")), lam("b", var("b"))),
        );

        let reduced = HeadSpine::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("y", var("y")));
    }
}

mod beta_hybrid_normal_order {

    use super::*;

    use term::{any_term, app, lam, var};

    proptest! {
        #[test]
        fn reduce_any_expr_compared_to_recursive_impl(
            expr in any_term()
        ) {
            let mut expected = expr.clone();
            HybridNormalOrder::<Enumerate>::reduce_rec(&mut expected);

            let result = HybridNormalOrder::<Enumerate>::reduce(expr.clone());

            prop_assert_eq!(result, expected);
        }
    }

    #[test]
    fn identity_x() {
        let expr = app(lam("x", var("x")), var("a"));

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("a"));
    }

    #[test]
    fn constant_y() {
        let expr = app(lam("x", var("y")), var("a"));

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("y"));
    }

    #[test]
    fn application_of_free_variable() {
        let expr = app(var("x"), app(lam("z", var("z")), var("v")));

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("x"), var("v")));
    }

    #[test]
    fn application_of_free_variables_2() {
        let expr = app(app(var("x"), var("y")), app(lam("z", var("z")), var("v")));

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(app(var("x"), var("y")), var("v")));
    }

    #[test]
    fn complex_expr1() {
        // (λx.(λy.x y) a) b
        let expr = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
            var("b"),
        );
        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, app(var("b"), var("a")));
    }

    #[test]
    fn complex_expr2() {
        // ((\a.a)\b.\c.b)(x)\e.f
        let expr = app(
            app(
                app(lam("a", var("a")), lam("b", lam("c", var("b")))),
                var("x"),
            ),
            lam("e", var("f")),
        );

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, var("x"));
    }

    #[test]
    fn complex_expr3() {
        // ( \x.(\y.x (\z.z) y) ) ( (\a.a) (\b.b) )
        let expr = app(
            lam(
                "x",
                lam("y", app(var("x"), app(lam("z", var("z")), var("y")))),
            ),
            app(lam("a", var("a")), lam("b", var("b"))),
        );

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("y", var("y")));
    }

    #[test]
    fn complex_expr4() {
        // ( \x.\y.z ) ( z ((\x.u) y) )
        let expr = app(
            lam("x", lam("y", var("z"))),
            app(var("z"), app(lam("x", var("u")), var("y"))),
        );

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("y1", var("z")));
    }

    #[test]
    fn complex_expr5() {
        // \x.((\x.a) x)
        let expr = lam("x", app(lam("x", var("a")), var("x")));

        let reduced = HybridNormalOrder::<Enumerate>::reduce(expr);

        assert_eq!(reduced, lam("x", var("a")));
    }
}
