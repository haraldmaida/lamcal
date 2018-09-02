use super::*;

mod token {

    use super::*;

    #[test]
    fn display_format_a_lambda_token() {
        let token = Lambda;

        let formatted = token.to_string();

        assert_eq!(formatted, "lambda");
    }

    #[test]
    fn display_format_a_body_separator_token() {
        let token = BodySeparator;

        let formatted = token.to_string();

        assert_eq!(formatted, "'.'");
    }

    #[test]
    fn display_format_a_left_paren_token() {
        let token = LParen;

        let formatted = token.to_string();

        assert_eq!(formatted, "'('");
    }

    #[test]
    fn display_format_a_right_paren_token() {
        let token = RParen;

        let formatted = token.to_string();

        assert_eq!(formatted, "')'");
    }

    proptest! {
        #[test]
        fn display_format_an_identifier_token(
            name in "[A-Za-z][A-Za-z0-9_']*",
        ) {
            let token = Identifier(name);

            let formatted = token.to_string();

            assert_eq!(formatted, "identifier");
        }
    }
}

mod tokenize {

    use super::*;

    #[test]
    fn empty_string() {
        let input = "";

        let tokens = tokenize(input.chars());

        assert_eq!(tokens, Ok(vec![]));
    }

    proptest! {

        #[test]
        fn identifier(
            name in "[A-Za-z][A-Za-z0-9_']*",
        ) {
            let input = format!("{}", name);

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![(Identifier(name), pos(1, 1))]));
        }

        #[test]
        fn identifier_starting_with_an_invalid_character(
            name in "[0-9_'][A-Za-z0-9_']*",
        ) {
            let input = format!("{}", name);

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Err(ParseError::new(
                InvalidCharacter,
                pos(1, 1),
                name.chars().next().unwrap(),
                "any unicode letter or \'λ\', \'.\', \'(\', \')\'",
                None,
            )));
        }

        #[test]
        fn identifier_surrounded_by_whitespace(
            name in "[A-Za-z][A-Za-z0-9_']*",
            pre_whitespace in "[ \\t]+",
            post_whitespace in "[ \\t]+",
        ) {
            let input = format!("{}{}{}", pre_whitespace, name, post_whitespace);
            let col = 1 + pre_whitespace.len();

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![(Identifier(name), pos(1, col))]));
        }

        #[test]
        fn lambda_with_bound_identifier(
            lambda in "[λ\\\\]",
            name in "[A-Za-z][A-Za-z0-9_']*",
        ) {
            let input = format!("{}{}", lambda, name);

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![(Lambda, pos(1, 1)), (Identifier(name), pos(1, 2))]));
        }

        #[test]
        fn a_sequence_of_two_identifiers(
            name1 in "[A-Za-z][A-Za-z0-9_']*",
            name2 in "[A-Za-z][A-Za-z0-9_']*",
            whitespace in "[ \\t]+",
        ) {
            let input = format!("{}{}{}", name1, whitespace, name2);
            let col2 = 1 + name1.len() + whitespace.len();

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![
                (Identifier(name1), pos(1, 1)),
                (Identifier(name2), pos(1, col2)),
            ]));
        }

        #[test]
        fn a_sequence_of_three_identifiers(
            name1 in "[A-Za-z][A-Za-z0-9_']*",
            name2 in "[A-Za-z][A-Za-z0-9_']*",
            name3 in "[A-Za-z][A-Za-z0-9_']*",
            whitespace1 in "[ \\t]+",
            whitespace2 in "[ \\t]+",
        ) {
            let input = format!("{}{}{}{}{}", name1, whitespace1, name2, whitespace2, name3);
            let col2 = 1 + name1.len() + whitespace1.len();
            let col3 = col2 + name2.len() + whitespace2.len();

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![
                (Identifier(name1), pos(1, 1)),
                (Identifier(name2), pos(1, col2)),
                (Identifier(name3), pos(1, col3 )),
            ]));
        }

        #[test]
        fn a_sequence_of_two_identifiers_with_parens(
            name1 in "[A-Za-z][A-Za-z0-9_']*",
            name2 in "[A-Za-z][A-Za-z0-9_']*",
            whitespace in "[ \\t]*",
        ) {
            let input = format!("({}){}{}", name1, whitespace, name2);
            let col3 = 2 + name1.len();
            let col4 = col3 + 1 + whitespace.len();

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![
                (LParen, pos(1, 1)),
                (Identifier(name1), pos(1, 2)),
                (RParen, pos(1, col3)),
                (Identifier(name2), pos(1, col4)),
            ]));
        }

        #[test]
        fn an_identifier_bound_to_an_abstraction(
            lambda in "[λ\\\\]",
            name1 in "[A-Za-z][A-Za-z0-9_']*",
            name2 in "[A-Za-z][A-Za-z0-9_']*",
        ) {
            let input = format!("{}{}.{}", lambda, name1, name2);
            let col3 = 2 + name1.len();
            let col4 = col3 + 1;

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![
                (Lambda, pos(1, 1)),
                (Identifier(name1), pos(1, 2)),
                (BodySeparator, pos(1, col3)),
                (Identifier(name2), pos(1, col4)),
            ]));
        }

        #[test]
        fn an_abstraction_with_a_sequence_of_identifiers_in_the_body(
            lambda in "[λ\\\\]",
            name1 in "[A-Za-z][A-Za-z0-9_']*",
            name2 in "[A-Za-z][A-Za-z0-9_']*",
            name3 in "[A-Za-z][A-Za-z0-9_']*",
            whitespace in "[ \\t]+",
        ) {
            let input = format!("{}{}.{}{}{}", lambda, name1, name2, whitespace, name3);
            let col3 = 2 + name1.len();
            let col4 = col3 + 1;
            let col5 = col4 + name2.len() + whitespace.len();

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![
                (Lambda, pos(1, 1)),
                (Identifier(name1), pos(1, 2)),
                (BodySeparator, pos(1, col3)),
                (Identifier(name2), pos(1, col4)),
                (Identifier(name3), pos(1, col5)),
            ]));
        }

        #[test]
        fn an_identifier_before_left_paren_no_whitespace(
            name1 in "[A-Za-z][A-Za-z0-9_']*",
            name2 in "[A-Za-z][A-Za-z0-9_']*",
        ) {
            let input = format!("{}({})", name1, name2);
            let col2 = 1 + name1.len();
            let col3 = col2 + 1;
            let col4 = col3 + name2.len();

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![
                (Identifier(name1), pos(1, 1)),
                (LParen, pos(1, col2)),
                (Identifier(name2), pos(1, col3)),
                (RParen, pos(1, col4)),
            ]))
        }

        #[test]
        fn an_identifier_before_lambda_no_whitespace(
            lambda in "[λ\\\\]",
            name1 in "[A-Za-z][A-Za-z0-9_']*",
            name2 in "[A-Za-z][A-Za-z0-9_']*",
            name3 in "[A-Za-z][A-Za-z0-9_']*",
        ) {
            let input = format!("{}{}{}.{}", name1, lambda, name2, name3);
            let col2 = 1 + name1.len();
            let col3 = col2 + 1;
            let col4 = col3 + name2.len();
            let col5 = col4 + 1;

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Ok(vec![
                (Identifier(name1), pos(1, 1)),
                (Lambda, pos(1, col2)),
                (Identifier(name2), pos(1, col3)),
                (BodySeparator, pos(1, col4)),
                (Identifier(name3), pos(1, col5)),
            ]))
        }

        #[test]
        fn invalid_character(
            valid_input1 in "([A-Za-z][A-Za-z0-9_']*)?",
            valid_input2 in "([A-Za-z][A-Za-z0-9_']*)?",
            invalid_input in "[\\U{1f300}-\\U{1f5ff}]"
        ) {
            let input = format!("{}{}{}", valid_input1, invalid_input, valid_input2);
            let col2 = 1 + valid_input1.len();

            let tokens = tokenize(input.chars());

            prop_assert_eq!(tokens, Err(ParseError::new(
                InvalidCharacter,
                pos(1, col2),
                invalid_input,
                "any unicode letter, digit or 'λ', '.', '(', ')', '_', '\''",
                None,
            )));
        }
    }
}

mod parse_tokens {

    use super::*;

    #[test]
    fn parse_empty_token_list() {
        let tokens = vec![];

        let parsed = parse_tokens(tokens);

        assert_eq!(parsed, Err(ParseError::new(
            EmptyExpression,
            CharPosition::default(),
            "invalid lambda expression",
            "at least one variable, abstraction or application",
            hint("a lambda expression must consist of at least one term, like a variable, an abstraction or an application"),
        )));
    }
}

mod parse {

    use super::*;

    use term::{app, lam, var};

    proptest! {

        #[test]
        fn result_of_to_string_is_parsable_as_variable(
            name in "[a-z][a-z0-9_']*",
        ) {
            let expr = var(name);

            let result = parse(expr.to_string().chars());

            prop_assert_eq!(result, Ok(expr));
        }

        #[test]
        fn result_of_to_string_is_parsable_as_abstraction(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
        ) {
            let expr = lam(name1, var(name2));

            let result = parse(expr.to_string().chars());

            prop_assert_eq!(result, Ok(expr));
        }

        #[test]
        fn result_of_to_string_is_parsable_as_application_of_var_to_lambda(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            name3 in "[a-z][a-z0-9_']*",
        ) {
            let expr = app(lam(name1, var(name2)), var(name3));

            let result = parse(expr.to_string().chars());

            prop_assert_eq!(result, Ok(expr));
        }

        #[test]
        fn result_of_to_string_is_parsable_as_application_of_lambda_to_lambda(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            name3 in "[a-z][a-z0-9_']*",
            name4 in "[a-z][a-z0-9_']*",
        ) {
            let expr = app(lam(name1, var(name2)), lam(name3, var(name4)));

            let result = parse(expr.to_string().chars());

            prop_assert_eq!(result, Ok(expr));
        }

        #[test]
        fn result_of_to_string_is_parsable_as_application_of_var_to_var(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
        ) {
            let expr = app(var(name1), var(name2));

            let result = parse(expr.to_string().chars());

            prop_assert_eq!(result, Ok(expr));
        }

        #[test]
        fn result_of_to_string_is_parsable_as_application_of_var_to_application(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            name3 in "[a-z][a-z0-9_']*",
        ) {
            let expr = app(app(var(name1), var(name2)), var(name3));

            let result = parse(expr.to_string().chars());

            prop_assert_eq!(result, Ok(expr));
        }

        #[test]
        fn result_of_to_string_is_parsable_as_constant(
            name in "[A-Z][A-Za-z0-9_']*",
        ) {
            let expr = con(name);

            let result = parse(expr.to_string().chars());

            prop_assert_eq!(result, Ok(expr));
        }

        #[test]
        fn ignores_whitespace_between_lambda_and_bound_variable(
            whitespace in "[\\s]+",
            param in "[a-z][a-z0-9_']*",
            body in "[a-z][a-z0-9_']*",
        ) {
            let input = format!("λ{}{}.{}", whitespace, param, body);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(lam(param, var(body))));
        }

        #[test]
        fn ignores_whitespace_between_bound_variable_and_dot(
            whitespace in "[\\s]+",
            param in "[a-z][a-z0-9_']*",
            body in "[a-z][a-z0-9_']*",
        ) {
            let input = format!("λ{}{}.{}", param, whitespace, body);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(lam(param, var(body))));
        }

        #[test]
        fn ignores_whitespace_between_dot_and_abstraction_body(
            whitespace in "[\\s]+",
            param in "[a-z][a-z0-9_']*",
            body in "[a-z][a-z0-9_']*",
        ) {
            let input = format!("λ{}.{}{}", param, whitespace, body);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(lam(param, var(body))));
        }

        #[test]
        fn ignores_whitespace_between_opening_paren_and_expression(
            whitespace in "[\\s]+",
            param in "[a-z][a-z0-9_']*",
            body in "[a-z][a-z0-9_']*",
            name in "[a-z][a-z0-9_']*",
        ) {
            let input = format!("({}λ{}.{}){}", whitespace, param, body, name);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(lam(param, var(body)), var(name))));
        }

        #[test]
        fn ignores_whitespace_between_expression_and_closing_paren(
            whitespace in "[\\s]+",
            param in "[a-z][a-z0-9_']*",
            body in "[a-z][a-z0-9_']*",
            name in "[a-z][a-z0-9_']*",
        ) {
            let input = format!("(λ{}.{}{}){}", param, body, whitespace, name);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(lam(param, var(body)), var(name))));
        }

        #[test]
        fn ignores_whitespace_between_closing_paren_and_expr2(
            whitespace in "[\\s]+",
            param in "[a-z][a-z0-9_']*",
            body in "[a-z][a-z0-9_']*",
            name in "[a-z][a-z0-9_']*",
        ) {
            let input = format!("(λ{}.{}){}{}", param, body, whitespace, name);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(lam(param, var(body)), var(name))));
        }

        #[test]
        fn parse_variable(
            name in "[a-z][a-z0-9_']*",
        ) {
            let input = format!("{}", name);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(var(name)));
        }

        #[test]
        fn parse_variable_surrounded_by_whitespace(
            name in "[a-z][a-z0-9_']*",
            whitespace1 in "[\\s]*",
            whitespace2 in "[\\s]*",
        ) {
            let input = format!("{}{}{}", whitespace1, name, whitespace2);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(var(name)));
        }

        #[test]
        fn parse_sequence_of_two_variables(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            separator in "[\\s]+",
        ) {
            let input = format!("{}{}{}", name1, separator, name2);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(var(name1), var(name2))));
        }

        #[test]
        fn parse_sequence_of_three_variables(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            name3 in "[a-z][a-z0-9_']*",
            separator1 in "[\\s]+",
            separator2 in "[\\s]+",
        ) {
            let input = format!("{}{}{}{}{}", name1, separator1, name2, separator2, name3);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(app(var(name1), var(name2)), var(name3))));
        }

        #[test]
        fn parse_application_ignoring_leading_whitespace(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            whitespace in "[\\s]+",
        ) {
            let name1 = &name1[..]; let name2 = &name2[..];
            let input = format!("{}(λ{}.{} {}) λ{}.{}", whitespace, name1, name1, name1, name2, name2);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(lam(name1, app(var(name1), var(name1))), lam(name2, var(name2)))));
        }

        #[test]
        fn parse_application_ignoring_trailing_whitespace(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
            whitespace in "[\\s]+",
        ) {
            let name1 = &name1[..]; let name2 = &name2[..];
            let input = format!("(λ{}.{} {}) λ{}.{}{}", name1, name1, name1, name2, name2, whitespace);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(lam(name1, app(var(name1), var(name1))), lam(name2, var(name2)))));
        }

        #[test]
        fn parse_named_constant(
            name in "[A-Z][A-Za-z0-9_']*",
        ) {
            let name = &name[..];
            let input = format!("{}", name);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(con(name)));
        }

        #[test]
        fn parse_named_constant_surrounded_by_whitespace(
            name in "[A-Z][A-Za-z0-9_']*",
            whitespace1 in "[\\s]*",
            whitespace2 in "[\\s]*",
        ) {
            let input = format!("{}{}{}", whitespace1, name, whitespace2);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(con(name)));
        }

        #[test]
        fn parse_sequence_of_two_named_constants(
            name1 in "[A-Z][A-Za-z0-9_']*",
            name2 in "[A-Z][A-Za-z0-9_']*",
            separator in "[\\s]+",
        ) {
            let input = format!("{}{}{}", name1, separator, name2);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(con(name1), con(name2))));
        }

        #[test]
        fn parse_sequence_of_three_named_constants(
            name1 in "[A-Z][A-Za-z0-9_']*",
            name2 in "[A-Z][A-Za-z0-9_']*",
            name3 in "[A-Z][A-Za-z0-9_']*",
            separator1 in "[\\s]+",
            separator2 in "[\\s]+",
        ) {
            let input = format!("{}{}{}{}{}", name1, separator1, name2, separator2, name3);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(app(app(con(name1), con(name2)), con(name3))));
        }

        #[test]
        fn parse_identity_combinator(
            name in "[a-z][a-z0-9_']*",
        ) {
            let name = &name[..];
            let input = format!("λ{}.{}", name, name);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(lam(name, var(name))));
        }

        #[test]
        fn parse_abstraction_of_abstraction_with_application_in_body(
            name1 in "[a-z][a-z0-9_']*",
            name2 in "[a-z][a-z0-9_']*",
        ) {
            let name1 = &name1[..]; let name2 = &name2[..];
            let input = format!("λ{}.λ{}.{} {}", name2, name1, name2, name1);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Ok(lam(name2, lam(name1, app(var(name2), var(name1))))));
        }

        #[test]
        fn parse_abstraction_with_named_constant_in_head(
            lambda in "[λ\\\\]",
            const_name1 in "[A-Z][A-Za-z0-9_']*",
            const_name2 in "[A-Z][A-Za-z0-9_']*",
            var_name1 in "[a-z][a-z0-9_']*",
        ) {
            let const_name1 = &const_name1[..];
            let const_name2 = &const_name2[..];
            let var_name1 = &var_name1[..];
            let input = format!("{}{}.{} {}", lambda, const_name1, const_name2, var_name1);

            let parsed = parse(input.chars());

            prop_assert_eq!(parsed, Err(ParseError::new(
                NamedConstantNotAllowedInLambdaHead,
                pos(1, 2),
                const_name1,
                "a variable name",
                hint("the first character in variable names must be a lowercase unicode letter"),
            )));
        }
    }

    #[test]
    fn parse_complex_example1() {
        let input = r#"(\x.(\y.x y) a) b"#;

        let parsed = parse(input.chars());

        assert_eq!(
            parsed,
            Ok(app(
                lam("x", app(lam("y", app(var("x"), var("y"))), var("a"))),
                var("b")
            ))
        );
    }
}
