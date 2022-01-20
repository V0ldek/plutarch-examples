module PlutarchExamples (exampleFunction) where

import Plutarch.Integer (PInteger)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

exampleFunction :: Term s (PInteger :--> PInteger)
exampleFunction = plam $ \x -> P.do
  x' <- plet $ x + 1 + 2 + 3
  x' + x'
