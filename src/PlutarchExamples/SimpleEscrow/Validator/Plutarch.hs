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

module PlutarchExamples.SimpleEscrow.Validator.Plutarch (validator) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Plutarch
import Plutarch.Api.V1
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.List
import Plutarch.Trace
import Plutarch.Unit (PUnit (..))
import Plutus.V1.Ledger.Scripts

validator :: Validator
validator = Validator $ compile mkValidator

newtype PEscrowParams (s :: S)
  = PEscrowParams
      ( Term
          s
          ( PDataRecord
              '[ "payee" ':= PPubKeyHash,
                 "paying" ':= PValue,
                 "expecting" ':= PValue,
                 "deadline" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances PEscrowParams)

data PAction (s :: S)
  = PRedeem (Term s (PDataRecord '[]))
  | PRefund (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via (PIsDataReprInstances PAction)

instance POrd PBool where
  b1 #<= b2 = pmatch b1 $ \case
    PFalse -> pcon PTrue
    PTrue -> pmatch b2 $ \case
      PTrue -> pcon PTrue
      PFalse -> pcon PFalse

  b1 #< b2 = pmatch b1 $ \case
    PFalse -> pmatch b2 $ \case
      PFalse -> pcon PTrue
      PTrue -> pcon PFalse
    PTrue -> pcon PFalse

instance (PIsData a, POrd a) => POrd (PExtended a) where
  ext1 #<= ext2 =
    pmatch ext1 $ \case
      PNegInf _ -> pcon PTrue
      PPosInf _ -> pmatch ext2 $ \case
        PPosInf _ -> pcon PTrue
        _ -> pcon PFalse
      PFinite v1T -> pmatch ext2 $ \case
        PNegInf _ -> pcon PFalse
        PPosInf _ -> pcon PTrue
        PFinite v2T -> pletFields @'["_0"] v1T $ \v1 ->
          pletFields @'["_0"] v2T $ \v2 ->
            pfromData (hrecField @"_0" v1) #<= pfromData (hrecField @"_0" v2)
  ext1 #< ext2 =
    pmatch ext1 $ \case
      PNegInf _ -> pmatch ext2 $ \case
        PNegInf _ -> pcon PFalse
        _ -> pcon PTrue
      PPosInf _ -> pcon PFalse
      PFinite v1T -> pmatch ext2 $ \case
        PNegInf _ -> pcon PFalse
        PPosInf _ -> pcon PTrue
        PFinite v2T -> pletFields @'["_0"] v1T $ \v1 ->
          pletFields @'["_0"] v2T $ \v2 ->
            pfromData (hrecField @"_0" v1) #< pfromData (hrecField @"_0" v2)

instance (PIsData a, POrd a) => POrd (PLowerBound a) where
  lb1T #<= lb2T =
    pletFields @'["_0", "_1"] lb1T $ \lb1 ->
      pletFields @'["_0", "_1"] lb2T $ \lb2 ->
        let v1 = pfromData $ hrecField @"_0" lb1
            v2 = pfromData $ hrecField @"_0" lb2
            cl1 = pfromData $ hrecField @"_1" lb1
            cl2 = pfromData $ hrecField @"_1" lb2
         in pif (v2 #< v1) (pcon PFalse) (cl2 #<= cl1)
  lb1T #< lb2T =
    pletFields @'["_0", "_1"] lb1T $ \lb1 ->
      pletFields @'["_0", "_1"] lb2T $ \lb2 ->
        let v1 = pfromData $ hrecField @"_0" lb1
            v2 = pfromData $ hrecField @"_0" lb2
            cl1 = pfromData $ hrecField @"_1" lb1
            cl2 = pfromData $ hrecField @"_1" lb2
         in pif (v1 #< v2) (pcon PTrue) (cl2 #< cl1)

instance (PIsData a, POrd a) => POrd (PUpperBound a) where
  lb1T #<= lb2T =
    pletFields @'["_0", "_1"] lb1T $ \lb1 ->
      pletFields @'["_0", "_1"] lb2T $ \lb2 ->
        let v1 = pfromData $ hrecField @"_0" lb1
            v2 = pfromData $ hrecField @"_0" lb2
            cl1 = pfromData $ hrecField @"_1" lb1
            cl2 = pfromData $ hrecField @"_1" lb2
         in pif (v2 #< v1) (pcon PFalse) (cl1 #<= cl2)
  lb1T #< lb2T =
    pletFields @'["_0", "_1"] lb1T $ \lb1 ->
      pletFields @'["_0", "_1"] lb2T $ \lb2 ->
        let v1 = pfromData $ hrecField @"_0" lb1
            v2 = pfromData $ hrecField @"_0" lb2
            cl1 = pfromData $ hrecField @"_1" lb1
            cl2 = pfromData $ hrecField @"_1" lb2
         in pif (v1 #< v2) (pcon PTrue) (cl1 #< cl2)

pbeforeOrAT :: (PIsData a, POrd a) => Term s a -> Term s (PInterval a) -> Term s PBool
pbeforeOrAT _h iT =
  pletFields @'["from"] iT $ \i ->
    let from = pfromData $ hrecField @"from" i
        lowerh = error "TODO: We want Plutarch equivalent of PLowerBound (PFinite h) PTrue here"
     in lowerh #<= from

pafter :: (PIsData a, POrd a) => Term s a -> Term s (PInterval a) -> Term s PBool
pafter _h iT =
  pletFields @'["to"] iT $ \i ->
    let to = pfromData $ hrecField @"to" i
        upperh = error "TODO: We want Plutarch equivalent of PUpperBound (PFinite h) PTrue here"
     in upperh #< to

mkValidator :: ClosedTerm (PEscrowParams :--> PAction :--> PScriptContext :--> PUnit)
mkValidator =
  plam $ \datumT redeemerT ctxT ->
    pletFields @'["txInfo"] ctxT $ \ctx ->
      pletFields @'["payee", "paying", "expecting", "deadline"] datumT $ \datum ->
        let deadline = pfromData $ hrecField @"deadline" datum
            payee = hrecField @"payee" datum
            validRange = pfromData $ pfield @"validRange" # hrecField @"txInfo" ctx
            signatories = pfromData $ pfield @"signatories" # hrecField @"txInfo" ctx
         in pmatch redeemerT $ \case
              PRedeem _ ->
                plet (deadline `pafter` validRange) $ \notLapsed ->
                  plet (error "TODO: We want Plutarch equivalent of (valuePaidTo txInfo (payee params) `geq` expecting params) here") $ \paid ->
                    pif
                      notLapsed
                      (pif paid (pcon PUnit) (ptrace "escrow-not-paid" perror))
                      (ptrace "escrow-deadline-lapsed" perror)
              PRefund _ ->
                plet (deadline `pbeforeOrAT` validRange) $ \lapsed ->
                  plet (pelem # payee # signatories) $ \signed ->
                    pif
                      lapsed
                      (pif signed (pcon PUnit) (ptrace "escrow-not-signed" perror))
                      (ptrace "refund-too-early" perror)
