use super::*;

mod from_unsigned_integer {
    use super::*;

    #[test]
    fn zero() {
        let num: Term = 0usize.into();

        assert_eq!(num, Zero());
    }

    #[test]
    fn one() {
        let num: Term = 1usize.into();

        assert_eq!(num, One());
    }

    #[test]
    fn two() {
        let num: Term = 2usize.into();

        assert_eq!(
            num,
            lam("f", lam("a", app(var("f"), app(var("f"), var("a")))))
        );
    }

    #[test]
    fn three() {
        let num: Term = 3usize.into();

        assert_eq!(
            num,
            lam(
                "f",
                lam("a", app(var("f"), app(var("f"), app(var("f"), var("a")))))
            )
        );
    }

    #[test]
    fn u8_two() {
        let num: Term = 2u8.into();

        assert_eq!(
            num,
            lam("f", lam("a", app(var("f"), app(var("f"), var("a")))))
        );
    }

    #[test]
    fn u16_two() {
        let num: Term = 2u16.into();

        assert_eq!(
            num,
            lam("f", lam("a", app(var("f"), app(var("f"), var("a")))))
        );
    }

    #[test]
    fn u32_two() {
        let num: Term = 2u32.into();

        assert_eq!(
            num,
            lam("f", lam("a", app(var("f"), app(var("f"), var("a")))))
        );
    }

    #[test]
    fn u64_two() {
        let num: Term = 2u64.into();

        assert_eq!(
            num,
            lam("f", lam("a", app(var("f"), app(var("f"), var("a")))))
        );
    }

    #[test]
    fn u128_two() {
        let num: Term = 2u128.into();

        assert_eq!(
            num,
            lam("f", lam("a", app(var("f"), app(var("f"), var("a")))))
        );
    }
}
