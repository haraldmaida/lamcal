use super::*;

mod bitand_stop {

    use super::*;

    #[test]
    fn no_and_no_returns_no() {
        assert_eq!(Stop::No & Stop::No, Stop::No);
    }

    #[test]
    fn yes_and_no_returns_no() {
        assert_eq!(Stop::Yes & Stop::No, Stop::No);
    }

    #[test]
    fn no_and_yes_returns_no() {
        assert_eq!(Stop::No & Stop::Yes, Stop::No);
    }

    #[test]
    fn yes_and_yes_returns_yes() {
        assert_eq!(Stop::Yes & Stop::Yes, Stop::Yes);
    }
}

mod bitor_stop {

    use super::*;

    #[test]
    fn no_and_no_returns_no() {
        assert_eq!(Stop::No | Stop::No, Stop::No);
    }

    #[test]
    fn yes_and_no_returns_yes() {
        assert_eq!(Stop::Yes | Stop::No, Stop::Yes);
    }

    #[test]
    fn no_and_yes_returns_yes() {
        assert_eq!(Stop::No | Stop::Yes, Stop::Yes);
    }

    #[test]
    fn yes_and_yes_returns_yes() {
        assert_eq!(Stop::Yes | Stop::Yes, Stop::Yes);
    }
}

mod limit {

    use super::*;

    use term::any_term;

    proptest! {

        #[test]
        fn calling_inspect_any_times_below_the_limit_returns_dont_stop(
            number in (1..=::std::u32::MAX),
            term in any_term(),
        ) {
            let mut limit = Limit { limit: number, count: number - 1 };

            let stop = limit.inspect(&term);

            prop_assert_eq!(stop, Stop::No);
        }

        #[test]
        fn calling_inspect_when_the_limit_is_reached_it_returns_stop(
            number in (0..=::std::u32::MAX),
            term in any_term(),
        ) {
            let mut limit = Limit { limit: number, count: number };

            let stop = limit.inspect(&term);

            prop_assert_eq!(stop, Stop::Yes);
        }
    }
}

mod trace {

    use super::*;

    use reduction::{Enumerate, NormalOrder};
    use term::{any_term, app, lam, var};

    proptest! {

        #[test]
        fn calling_inspect_with_any_term_always_returns_dont_stop(
            term in any_term(),
        ) {
            let mut trace = Trace::new(|_to_trace| ());

            let stop = trace.inspect(&term);

            prop_assert_eq!(stop, Stop::No);
        }
    }

    #[test]
    fn reduction_of_expression_1() {
        let mut term = app(
            lam("x", app(lam("y", app(var("x"), var("y"))), var("z"))),
            var("a"),
        );

        term.reduce_inspected::<NormalOrder<Enumerate>, _>(&mut Trace::new(|to_reduce| {
            println!("reducing: {}", to_reduce)
        }));
    }
}

mod collect {

    use super::*;

    use term::any_term;

    proptest! {

        #[test]
        fn calling_inspect_with_any_term_always_returns_dont_stop(
            term in any_term(),
        ) {
            let mut collect = Collect::default();

            let stop = collect.inspect(&term);

            prop_assert_eq!(stop, Stop::No);
        }
    }
}
