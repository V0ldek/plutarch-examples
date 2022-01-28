{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PlutarchExamples.Gift.Validator.Plutarch (plutarchValidator) where

import Plutarch
import Plutarch.Api.V1
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.ByteString (PByteString)
import Plutarch.DataRepr
import Plutarch.List
import Plutarch.Trace
import Plutarch.Unit (PUnit (..))
import Plutus.V1.Ledger.Scripts

plutarchValidator :: Validator
plutarchValidator = Validator $ compile validator

validator :: ClosedTerm (PAsData PPubKeyHash :--> PByteString :--> PScriptContext :--> PUnit)
validator =
  plam $ \datum (_redeemer :: Term s PByteString) ctxT ->
    pletFields @'["txInfo"] ctxT $ \ctx ->
      let signatories = pfromData $ pfield @"signatories" # hrecField @"txInfo" ctx
       in plet (pelem # datum # signatories) $ \(isBeneficiary :: Term s PBool) ->
            pif isBeneficiary (pcon PUnit) $ ptrace "plu:not-beneficiary" perror