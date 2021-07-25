import Language.Elab.Deriving.Eq
import Language.Elab.Deriving.Ord

%language ElabReflection

data Flippy = Dolphin | Rudolph

%runElab deriveEq Private `{{Flippy}}

-- testFlippy : Bool
-- testFlippy = Dolphin == Rudolph

--%runElab deriveOrd Private `{{Flippy}}
