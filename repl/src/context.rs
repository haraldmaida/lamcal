use model::{AlphaRenamingStrategy, BetaReductionStrategy};

/// Context of the REPL.
#[allow(missing_copy_implementations)]
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Context {
    alpha_renaming_strategy: AlphaRenamingStrategy,
    beta_reduction_strategy: BetaReductionStrategy,
}

impl Context {
    pub fn alpha_renaming_strategy(&self) -> AlphaRenamingStrategy {
        self.alpha_renaming_strategy
    }

    pub fn set_alpha_renaming_strategy(&mut self, strategy: AlphaRenamingStrategy) {
        self.alpha_renaming_strategy = strategy;
    }

    pub fn beta_reduction_strategy(&self) -> BetaReductionStrategy {
        self.beta_reduction_strategy
    }

    pub fn set_beta_reduction_strategy(&mut self, strategy: BetaReductionStrategy) {
        self.beta_reduction_strategy = strategy;
    }
}
