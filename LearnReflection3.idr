import Language.Reflection
import Language.Reflection.TT

%language ElabReflection

record Foo where
  constructor MkFoo
  Fee : Int
  Bar : Char
  
record MFoo where
  constructor MkMFoo
  MFee : Maybe Int
  MBar : Maybe Char
  
logCons : Name -> Elab ()
logCons name = do ns <- getType name
                  traverse (\ (n, ty) =>
                              do logMsg "" 0 ("Name: " ++ (show n))
                                 logTerm 0 "Type" ty
                                 ) ns
                  --fail "Not really trying"
                  pure ()

  
logRecord : Name -> Elab ()
logRecord name = do ns <- getType name
                    -- logMsg 0 ("Resolved name: " ++ show n)
                    -- logMsg 0 ("Constructors: " ++ show !(getCons n))
                    traverse (\ (n, ty) =>
                                do logMsg "" 0 ("Name: " ++ show n)
                                   logTerm 0 "Type" ty
                                   cons <- getCons n
                                   logMsg 0 ("Constructors: " ++ show cons)
                                   traverse logCons cons
                                   ) ns
                    --fail "Not really trying"
                    pure ()

logDataCons : Elab a
logDataCons
    = do [(n, _)] <- getType `{{ Nat }}
             | _ => fail "Ambiguous name"
         logMsg 0 ("Resolved name: " ++ show n)
         logMsg 0 ("Constructors: " ++ show !(getCons n))
         fail "Still not trying"

-- LOG 0: Constructors: [Main.MkFoo]
-- LOG 0: Name: Main.MkFoo
-- LOG 0: Type: (%pi Rig1 Explicit (Just Fee) Int (%pi Rig1 Explicit (Just Bar) Char Main.Foo))
-- LOG 0: Name: Main.MFoo
-- LOG 0: Type: %type
-- LOG 0: Constructors: [Main.MkMFoo]
-- LOG 0: Name: Main.MkMFoo
-- LOG 0: Type: (%pi Rig1 Explicit (Just MFee) (Prelude.Types.Maybe Int) (%pi Rig1 Explicit (Just MBar) (Prelude.Types.Maybe Char) Main.MFoo))

-- co: too noisy
-- dummy1 : ()
-- dummy1 = %runElab  (do
--    logRecord `{{ Foo }}
--    logRecord `{{ MFoo }})

mkMaybeRecord : ()
mkMaybeRecord = %runElab (mkMaybeRecordE `{{ Main.Foo }})
