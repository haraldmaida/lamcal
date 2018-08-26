use std::fmt::{self, Display};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AlphaRenamingStrategy {
    Enumerate,
    Prime,
}

impl Default for AlphaRenamingStrategy {
    fn default() -> Self {
        AlphaRenamingStrategy::Enumerate
    }
}

impl Display for AlphaRenamingStrategy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AlphaRenamingStrategy::*;
        f.write_str(match *self {
            Enumerate => "enumerate",
            Prime => "prime",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BetaReductionStrategy {
    ApplicativeOrder,
    CallByName,
    CallByValue,
    HeadSpine,
    HybridApplicativeOrder,
    HybridNormalOrder,
    NormalOrder,
}

impl Default for BetaReductionStrategy {
    fn default() -> Self {
        BetaReductionStrategy::NormalOrder
    }
}

impl Display for BetaReductionStrategy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BetaReductionStrategy::*;
        f.write_str(match *self {
            ApplicativeOrder => "applicative-order",
            CallByName => "call-by-name",
            CallByValue => "call-by-value",
            HeadSpine => "head-spine",
            HybridApplicativeOrder => "hybrid-applicative-order",
            HybridNormalOrder => "hybrid-normal-order",
            NormalOrder => "normal-order",
        })
    }
}
