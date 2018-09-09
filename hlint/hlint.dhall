   let warn           = ./warn.dhall
in let warnReexport   = warn.warnReexport
in let warnReexportOp = warn.warnReexportOp
in let warnLifted     = warn.warnLifted
in let warnSimple     = warn.warnSimple
in let warnNote       = warn.warnNote
in let hintNote       = warn.hintNote
in let rule           = constructors ./Rule.dhall
in [ rule.Arguments { arguments =
       [ "-XConstraintKinds"
       , "-XDeriveGeneric"
       , "-XGeneralizedNewtypeDeriving"
       , "-XLambdaCase"
       , "-XOverloadedStrings"
       , "-XRecordWildCards"
       , "-XScopedTypeVariables"
       , "-XStandaloneDeriving"
       , "-XTupleSections"
       , "-XTypeApplications"
       , "-XViewPatterns"
       ]
     }
   -------------
   -- Ignore
   -------------
   -- There's no 'head' in Relude
   , rule.Ignore {ignore = {name = "Use head"}}
   -- We have 'whenJust' for this
   , rule.Ignore {ignore = {name = "Use Foldable.forM_"}}

   -------------
   -- Relude specific
   -------------
   -- Applicative
   , hintNote "pure ()" "pass" "Use 'pass'"
   , hintNote "return ()" "pass" "Use 'pass'"

   -- Deepseq
   , warnSimple "Control.Exception.evaluate" "evaluateWHNF"
   , warnSimple "Control.Exception.evaluate (force x)" "evaluateNF x"
   , warnSimple "Control.Exception.evaluate (x `deepseq` ())" "evaluateNF_ x"
   , warnSimple "void (evaluateWHNF x)" "evaluateWHNF_ x"
   , warnSimple "void (evaluateNF x)" "evaluateNF_ x"

   -- Exception
   , hintNote "Control.Exception.throw" "impureThrow" "Use 'impureThrow'"

   -- Foldable
   , hintNote "foldl' (flip f)" "flipfoldl' f" "Use 'flipfoldl''"

   , warnSimple "foldl' (+) 0" "sum"
   , warnSimple "foldl' (*) 1" "product"

   , hintNote "fmap and (sequence s)" "andM s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."
   , hintNote "and <$> sequence s" "andM s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."
   , hintNote "fmap or (sequence s)" "orM s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."
   , hintNote "or <$> sequence s" "orM s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."

   , hintNote "fmap and (mapM f s)" "allM f s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."
   , hintNote "and <$> mapM f s" "allM f s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."

   , hintNote "fmap or (mapM f s)" "anyM f s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."
   , hintNote "or <$> mapM f s" "anyM f s" "Applying this hint would mean that some actions that were being executed previously would no longer be executed."

   -- Functor
   , warnSimple "map fst &&& map snd" "unzip"
   , hintNote "fmap (fmap f) x" "f <<$>> x" "Use '(<<$>>)'"
   , hintNote "(\\f -> f x) <$> ff"  "ff ?? x" "Use flap operator"
   , hintNote "fmap (\\f -> f x) ff" "ff ?? x" "Use flap operator"
   , hintNote "fmap ($ x) ff"        "ff ?? x" "Use flap operator"
   , hintNote "($ x) <$> ff"         "ff ?? x" "Use flap operator"

   -- List
   , warnSimple "fmap f (nonEmpty x)" "viaNonEmpty f x"
   , warnSimple "fmap f . nonEmpty" "viaNonEmpty f"
   , warnSimple "f <$> nonEmpty x" "viaNonEmpty f x"

   -- Monad
   , warnSimple "f >>= guard" "guardM f"
   , warnSimple "guard =<< f" "guardM f"

   , warnSimple "whenM (not <$> x)" "unlessM x"
   , warnSimple "unlessM (not <$> x)" "whenM x"
   , warnSimple "either (const True) (const False)" "isLeft"
   , warnSimple "either (const False) (const True)" "isRight"
   , warnSimple "either id (const a)" "fromLeft a"
   , warnSimple "either (const b) id" "fromRight b"
   , warnSimple "either Just (const Nothing)" "leftToMaybe"
   , warnSimple "either (const Nothing) Just" "rightToMaybe"
   , warnSimple "maybe (Left l) Right" "maybeToRight l"
   , warnSimple "maybe (Right r) Left" "maybeToLeft r"

   -- case:whenJust
   , warnSimple "(case m of Just x -> f x; Nothing -> pure ()  )" "whenJust m f"
   , warnSimple "(case m of Just x -> f x; Nothing -> return ())" "whenJust m f"
   , warnSimple "(case m of Just x -> f x; Nothing -> pass     )" "whenJust m f"
   , warnSimple "(case m of Nothing -> pure ()  ; Just x -> f x)" "whenJust m f"
   , warnSimple "(case m of Nothing -> return (); Just x -> f x)" "whenJust m f"
   , warnSimple "(case m of Nothing -> pass     ; Just x -> f x)" "whenJust m f"
   , warnSimple "(maybe (pure ())   f m)"                         "whenJust m f"
   , warnSimple "(maybe (return ()) f m)"                         "whenJust m f"
   , warnSimple "(maybe pass        f m)"                         "whenJust m f"

   -- case:whenJustM
   , warnSimple "(m >>= \\a -> whenJust a f)"                        "whenJustM m f"
   , warnSimple "(m >>= \\case Just x -> f x; Nothing -> pure ()  )" "whenJustM m f"
   , warnSimple "(m >>= \\case Just x -> f x; Nothing -> return ())" "whenJustM m f"
   , warnSimple "(m >>= \\case Just x -> f x; Nothing -> pass     )" "whenJustM m f"
   , warnSimple "(m >>= \\case Nothing -> pure ()  ; Just x -> f x)" "whenJustM m f"
   , warnSimple "(m >>= \\case Nothing -> return (); Just x -> f x)" "whenJustM m f"
   , warnSimple "(m >>= \\case Nothing -> pass     ; Just x -> f x)" "whenJustM m f"
   , warnSimple "(maybe (pure ())   f =<< m)"                        "whenJustM m f"
   , warnSimple "(maybe (return ()) f =<< m)"                        "whenJustM m f"
   , warnSimple "(maybe pass        f =<< m)"                        "whenJustM m f"
   , warnSimple "(m >>= maybe (pure ())   f)"                        "whenJustM m f"
   , warnSimple "(m >>= maybe (return ()) f)"                        "whenJustM m f"
   , warnSimple "(m >>= maybe pass        f)"                        "whenJustM m f"

   -- case:whenNothing_
   , warnSimple "(case m of Just _ -> pure ()  ; Nothing -> x)" "whenNothing_ m x"
   , warnSimple "(case m of Just _ -> return (); Nothing -> x)" "whenNothing_ m x"
   , warnSimple "(case m of Just _ -> pass     ; Nothing -> x)" "whenNothing_ m x"
   , warnSimple "(case m of Nothing -> x; Just _ -> pure ()  )" "whenNothing_ m x"
   , warnSimple "(case m of Nothing -> x; Just _ -> return ())" "whenNothing_ m x"
   , warnSimple "(case m of Nothing -> x; Just _ -> pass     )" "whenNothing_ m x"
   , warnSimple "(maybe x (\\_ -> pure ()    ) m)"              "whenNothing_ m x"
   , warnSimple "(maybe x (\\_ -> return ()  ) m)"              "whenNothing_ m x"
   , warnSimple "(maybe x (\\_ -> pass       ) m)"              "whenNothing_ m x"
   , warnSimple "(maybe x (const (pure ()  )) m)"               "whenNothing_ m x"
   , warnSimple "(maybe x (const (return ())) m)"               "whenNothing_ m x"
   , warnSimple "(maybe x (const (pass     )) m)"               "whenNothing_ m x"

   -- case:whenNothingM_
   , warnSimple "(m >>= \\a -> whenNothing_ a x)"                  "whenNothingM_ m x"
   , warnSimple "(m >>= \\case Just _ -> pure ()  ; Nothing -> x)" "whenNothingM_ m x"
   , warnSimple "(m >>= \\case Just _ -> return (); Nothing -> x)" "whenNothingM_ m x"
   , warnSimple "(m >>= \\case Just _ -> pass     ; Nothing -> x)" "whenNothingM_ m x"
   , warnSimple "(m >>= \\case Nothing -> x; Just _ -> pure ()  )" "whenNothingM_ m x"
   , warnSimple "(m >>= \\case Nothing -> x; Just _ -> return ())" "whenNothingM_ m x"
   , warnSimple "(m >>= \\case Nothing -> x; Just _ -> pass     )" "whenNothingM_ m x"
   , warnSimple "(maybe x (\\_ -> pure ()    ) =<< m)"             "whenNothingM_ m x"
   , warnSimple "(maybe x (\\_ -> return ()  ) =<< m)"             "whenNothingM_ m x"
   , warnSimple "(maybe x (\\_ -> pass       ) =<< m)"             "whenNothingM_ m x"
   , warnSimple "(maybe x (const (pure ()  )) =<< m)"              "whenNothingM_ m x"
   , warnSimple "(maybe x (const (return ())) =<< m)"              "whenNothingM_ m x"
   , warnSimple "(maybe x (const (pass     )) =<< m)"              "whenNothingM_ m x"
   , warnSimple "(m >>= maybe x (\\_ -> pure ())    )"             "whenNothingM_ m x"
   , warnSimple "(m >>= maybe x (\\_ -> return ())  )"             "whenNothingM_ m x"
   , warnSimple "(m >>= maybe x (\\_ -> pass)       )"             "whenNothingM_ m x"
   , warnSimple "(m >>= maybe x (const (pure ())  ))"              "whenNothingM_ m x"
   , warnSimple "(m >>= maybe x (const (return ())))"              "whenNothingM_ m x"
   , warnSimple "(m >>= maybe x (const (pass)     ))"              "whenNothingM_ m x"

   -- case:whenLeft_
   , warnSimple "(whenLeft ())"                                   "whenLeft_"
   , warnSimple "(case m of Left x -> f x; Right _ -> pure ()  )" "whenLeft_ m f"
   , warnSimple "(case m of Left x -> f x; Right _ -> return ())" "whenLeft_ m f"
   , warnSimple "(case m of Left x -> f x; Right _ -> pass     )" "whenLeft_ m f"
   , warnSimple "(case m of Right _ -> pure ()  ; Left x -> f x)" "whenLeft_ m f"
   , warnSimple "(case m of Right _ -> return (); Left x -> f x)" "whenLeft_ m f"
   , warnSimple "(case m of Right _ -> pass     ; Left x -> f x)" "whenLeft_ m f"
   , warnSimple "(either f (\\_ -> pure ()    ) m)"               "whenLeft_ m f"
   , warnSimple "(either f (\\_ -> return ()  ) m)"               "whenLeft_ m f"
   , warnSimple "(either f (\\_ -> pass       ) m)"               "whenLeft_ m f"
   , warnSimple "(either f (const (pure ()  )) m)"                "whenLeft_ m f"
   , warnSimple "(either f (const (return ())) m)"                "whenLeft_ m f"
   , warnSimple "(either f (const (pass     )) m)"                "whenLeft_ m f"

   -- case:whenLeftM_
   , warnSimple "(m >>= \\a -> whenLeft_ a f)"                       "whenLeftM_ m f"
   , warnSimple "(m >>= \\case Left x -> f x; Right _ -> pure ()  )" "whenLeftM_ m f"
   , warnSimple "(m >>= \\case Left x -> f x; Right _ -> return ())" "whenLeftM_ m f"
   , warnSimple "(m >>= \\case Left x -> f x; Right _ -> pass     )" "whenLeftM_ m f"
   , warnSimple "(m >>= \\case Right _ -> pure ()  ; Left x -> f x)" "whenLeftM_ m f"
   , warnSimple "(m >>= \\case Right _ -> return (); Left x -> f x)" "whenLeftM_ m f"
   , warnSimple "(m >>= \\case Right _ -> pass     ; Left x -> f x)" "whenLeftM_ m f"
   , warnSimple "(either f (\\_ -> pure ()    ) =<< m)"              "whenLeftM_ m f"
   , warnSimple "(either f (\\_ -> return ()  ) =<< m)"              "whenLeftM_ m f"
   , warnSimple "(either f (\\_ -> pass       ) =<< m)"              "whenLeftM_ m f"
   , warnSimple "(either f (const (pure ()  )) =<< m)"               "whenLeftM_ m f"
   , warnSimple "(either f (const (return ())) =<< m)"               "whenLeftM_ m f"
   , warnSimple "(either f (const (pass     )) =<< m)"               "whenLeftM_ m f"
   , warnSimple "(m >>= either f (\\_ -> pure ())    )"              "whenLeftM_ m f"
   , warnSimple "(m >>= either f (\\_ -> return ())  )"              "whenLeftM_ m f"
   , warnSimple "(m >>= either f (\\_ -> pass)       )"              "whenLeftM_ m f"
   , warnSimple "(m >>= either f (const (pure ())  ))"               "whenLeftM_ m f"
   , warnSimple "(m >>= either f (const (return ())))"               "whenLeftM_ m f"
   , warnSimple "(m >>= either f (const (pass)     ))"               "whenLeftM_ m f"

   -- case:whenRight_
   , warnSimple "(whenRight ())"                                  "whenRight_"
   , warnSimple "(case m of Right x -> f x; Left _ -> pure ()  )" "whenRight_ m f"
   , warnSimple "(case m of Right x -> f x; Left _ -> return ())" "whenRight_ m f"
   , warnSimple "(case m of Right x -> f x; Left _ -> pass     )" "whenRight_ m f"
   , warnSimple "(case m of Left _ -> pure ()  ; Right x -> f x)" "whenRight_ m f"
   , warnSimple "(case m of Left _ -> return (); Right x -> f x)" "whenRight_ m f"
   , warnSimple "(case m of Left _ -> pass     ; Right x -> f x)" "whenRight_ m f"
   , warnSimple "(either (\\_ -> pure ()    ) f m)"               "whenRight_ m f"
   , warnSimple "(either (\\_ -> return ()  ) f m)"               "whenRight_ m f"
   , warnSimple "(either (\\_ -> pass       ) f m)"               "whenRight_ m f"
   , warnSimple "(either (const (pure ()  )) f m)"                "whenRight_ m f"
   , warnSimple "(either (const (return ())) f m)"                "whenRight_ m f"
   , warnSimple "(either (const (pass     )) f m)"                "whenRight_ m f"

   -- case:whenRightM_
   , warnSimple "(m >>= \\a -> whenRight_ a f)"                      "whenRightM_ m f"
   , warnSimple "(m >>= \\case Right x -> f x; Left _ -> pure ()  )" "whenRightM_ m f"
   , warnSimple "(m >>= \\case Right x -> f x; Left _ -> return ())" "whenRightM_ m f"
   , warnSimple "(m >>= \\case Right x -> f x; Left _ -> pass     )" "whenRightM_ m f"
   , warnSimple "(m >>= \\case Left _ -> pure ()  ; Right x -> f x)" "whenRightM_ m f"
   , warnSimple "(m >>= \\case Left _ -> return (); Right x -> f x)" "whenRightM_ m f"
   , warnSimple "(m >>= \\case Left _ -> pass     ; Right x -> f x)" "whenRightM_ m f"
   , warnSimple "(either (\\_ -> pure ()    ) f =<< m)"              "whenRightM_ m f"
   , warnSimple "(either (\\_ -> return ()  ) f =<< m)"              "whenRightM_ m f"
   , warnSimple "(either (\\_ -> pass       ) f =<< m)"              "whenRightM_ m f"
   , warnSimple "(either (const (pure ()  )) f =<< m)"               "whenRightM_ m f"
   , warnSimple "(either (const (return ())) f =<< m)"               "whenRightM_ m f"
   , warnSimple "(either (const (pass     )) f =<< m)"               "whenRightM_ m f"
   , warnSimple "(m >>= either (\\_ -> pure ())     f)"              "whenRightM_ m f"
   , warnSimple "(m >>= either (\\_ -> return ())   f)"              "whenRightM_ m f"
   , warnSimple "(m >>= either (\\_ -> pass)        f)"              "whenRightM_ m f"
   , warnSimple "(m >>= either (const (pure ())  ) f)"               "whenRightM_ m f"
   , warnSimple "(m >>= either (const (return ())) f)"               "whenRightM_ m f"
   , warnSimple "(m >>= either (const (pass)     ) f)"               "whenRightM_ m f"

   -- case:whenNotNull[M]
   , warnSimple "(case m of [] -> return (); (x:xs) -> f (x :| xs))" "whenNotNull m f"
   , warnSimple "(case m of [] -> pure ()  ; (x:xs) -> f (x :| xs))" "whenNotNull m f"
   , warnSimple "(case m of [] -> pass     ; (x:xs) -> f (x :| xs))" "whenNotNull m f"
   , warnSimple "(case m of (x:xs) -> f (x :| xs); [] -> return ())" "whenNotNull m f"
   , warnSimple "(case m of (x:xs) -> f (x :| xs); [] -> pure ()  )" "whenNotNull m f"
   , warnSimple "(case m of (x:xs) -> f (x :| xs); [] -> pass     )" "whenNotNull m f"
   , warnSimple "(m >>= \\case [] -> pass     ; (x:xs) -> f (x :| xs))" "whenNotNullM m f"
   , warnSimple "(m >>= \\case [] -> pure ()  ; (x:xs) -> f (x :| xs))" "whenNotNullM m f"
   , warnSimple "(m >>= \\case [] -> return (); (x:xs) -> f (x :| xs))" "whenNotNullM m f"
   , warnSimple "(m >>= \\case (x:xs) -> f (x :| xs); [] -> pass     )" "whenNotNullM m f"
   , warnSimple "(m >>= \\case (x:xs) -> f (x :| xs); [] -> pure ()  )" "whenNotNullM m f"
   , warnSimple "(m >>= \\case (x:xs) -> f (x :| xs); [] -> return ())" "whenNotNullM m f"

   , warnSimple "mapMaybe leftToMaybe" "lefts"
   , warnSimple "mapMaybe rightToMaybe" "rights"

   -- Monad.Trans
   , warnSimple "flip runReaderT" "usingReaderT"
   , warnSimple "flip runReader" "usingReader"
   , warnSimple "flip runStateT" "usingStateT"
   , warnSimple "flip runState" "usingState"
   , warnSimple "fst <$> usingStateT s st" "evaluatingStateT s st"
   , warnSimple "fst (usingState s st)" "evaluatingState s st"
   , warnSimple "snd <$> usingStateT s st" "executingStateT s st"
   , warnSimple "snd (usingState s st)" "executingState s st"

   -- Monoid
   , warnSimple "fromMaybe mempty" "maybeToMonoid"
   , warnSimple "m ?: mempty" "maybeToMonoid m"

   -- Sort and Nub Containers
   , warnSimple "Data.Map.toAscList (Data.Map.fromList x)" "sortWith fst x"
   , warnSimple "Data.Map.toDescList (Data.Map.fromList x)" "sortWith (Down . fst) x"
   , warnSimple "Data.Set.toList (Data.Set.fromList l)" "sortNub l"
   , warnSimple "Data.Set.assocs (Data.Set.fromList l)" "sortNub l"
   , warnSimple "Data.Set.toAscList (Data.Set.fromList l)" "sortNub l"
   , warnSimple "Data.HashSet.toList (Data.HashSet.fromList l)" "unstableNub l"
   -- Sort and Nub Base
   , warnNote "nub" "ordNub" "'nub' is O(n^2), 'ordNub' is O(n log n)"
   , warnNote "sortBy (comparing f)" "sortWith f"
         "If the function you are using for 'comparing' is slow, use 'sortOn' instead of 'sortWith', because 'sortOn' caches applications the function and 'sortWith' doesn't."
   , warnNote "sortOn fst" "sortWith fst" "'sortWith' will be faster here because it doesn't do caching"
   , warnNote "sortOn snd" "sortWith snd" "'sortWith' will be faster here because it doesn't do caching"
   , warnNote "sortOn (Down . fst)" "sortWith (Down . fst)" "'sortWith' will be faster here because it doesn't do caching"
   , warnNote "sortOn (Down . snd)" "sortWith (Down . snd)" "'sortWith' will be faster here because it doesn't do caching"

   -- Print
   , warnSimple "Data.Text.IO.putStr" "putText"
   , warnSimple "Data.Text.IO.putStrLn" "putTextLn"
   , warnSimple "Data.Text.Lazy.IO.putStr" "putLText"
   , warnSimple "Data.Text.Lazy.IO.putStrLn" "putLTextLn"
   , warnSimple "Data.ByteString.Char8.putStr" "putBS"
   , warnSimple "Data.ByteString.Char8.putStrLn" "putBSLn"
   , warnSimple "Data.ByteString.Lazy.Char8.putStr" "putLBS"
   , warnSimple "Data.ByteString.Lazy.Char8.putStrLn" "putLBSLn"

   -- String
   , warnSimple "Data.Text.Lazy.Text" "LText"
   , warnSimple "Data.ByteString.Lazy.ByteString" "LByteString"
   -- ConvertUtf8
   , warnSimple "Data.ByteString.UTF8.fromString" "encodeUtf8"
   , warnSimple "Data.ByteString.UTF8.toString" "decodeUtf8"
   , warnSimple "Data.Text.Encoding.encodeUtf8" "encodeUtf8"
   , warnSimple "Data.Text.Encoding.decodeUtf8" "decodeUtf8"
   , warnSimple "Data.ByteString.Lazy.toStrict (encodeUtf8 x)" "encodeUtf8 x"
   , warnSimple "toStrict (encodeUtf8 x)" "encodeUtf8 x"
   , warnSimple "decodeUtf8 (Data.ByteString.Lazy.fromStrict x)" "decodeUtf8 x"
   , warnSimple "decodeUtf8 (fromStrict x)" "decodeUtf8 x"
   , warnSimple "Data.ByteString.Lazy.UTF8.fromString" "encodeUtf8"
   , warnSimple "Data.ByteString.Lazy.UTF8.toString" "decodeUtf8"
   , warnSimple "Data.ByteString.Lazy.fromStrict (Data.Text.Encoding.encodeUtf8 x)" "encodeUtf8 x"
   , warnSimple "Data.ByteString.Lazy.fromStrict (encodeUtf8 x)" "encodeUtf8 x"
   , warnSimple "Data.Text.Encoding.decodeUtf8 (Data.ByteString.Lazy.toStrict x)" "decodeUtf8 x"
   , warnSimple "Data.Text.Encoding.decodeUtf8 (toStrict x)" "decodeUtf8 x"
   , warnSimple "decodeUtf8 (Data.ByteString.Lazy.toStrict x)" "decodeUtf8 x"
   , warnSimple "decodeUtf8 (toStrict x)" "decodeUtf8 x"
   -- ToText
   , warnSimple "Data.Text.pack" "toText"
   , warnSimple "Data.Text.unpack" "toString"
   , warnSimple "Data.Text.Lazy.pack" "toLText"
   , warnSimple "Data.Text.Lazy.unpack" "toString"
   , warnSimple "Data.Text.Lazy.toStrict" "toText"
   , warnSimple "Data.Text.Lazy.fromStrict" "toLText"
   -- Show
   , warnSimple "Data.Text.pack (show x)" "show x"
   , warnSimple "Data.Text.Lazy.pack (show x)" "show x"
   -- LazyStrict
   , warnSimple "Data.ByteString.Lazy.fromStrict" "fromStrict"
   , warnSimple "Data.ByteString.Lazy.toStrict" "toStrict"
   , warnSimple "Data.Text.Lazy.fromStrict" "fromStrict"
   , warnSimple "Data.Text.Lazy.toStrict" "toStrict"

   -------------
   -- Reexports
   -------------
     -- Applicative
   , warnReexport "Alternative" "Control.Applicative"
   , warnReexport "empty"       "Control.Applicative"
   , warnReexportOp "<|>"       "Control.Applicative"
   , warnReexport "some"        "Control.Applicative"
   , warnReexport "many"        "Control.Applicative"
   , warnReexport "Const"       "Control.Applicative"
   , warnReexport "getConst"    "Control.Applicative"
   , warnReexport "ZipList"     "Control.Applicative"
   , warnReexport "getZipList"  "Control.Applicative"
   , warnReexport "liftA2"      "Control.Applicative"
   , warnReexport "liftA3"      "Control.Applicative"
   , warnReexport "optional"    "Control.Applicative"
   , warnReexportOp "<**>"      "Control.Applicative"

     -- Base
   , warnReexport "xor" "Data.Bits"
   , warnReexport "chr" "Data.Char"
   , warnReexport "Int8"  "Data.Int"
   , warnReexport "Int16" "Data.Int"
   , warnReexport "Int32" "Data.Int"
   , warnReexport "Int64" "Data.Int"
   , warnReexport "Word8"      "Data.Word"
   , warnReexport "Word16"     "Data.Word"
   , warnReexport "Word32"     "Data.Word"
   , warnReexport "Word64"     "Data.Word"
   , warnReexport "byteSwap16" "Data.Word"
   , warnReexport "byteSwap32" "Data.Word"
   , warnReexport "byteSwap64" "Data.Word"
   , warnReexport "Natural" "Numeric.Natural"
   , warnReexport "Handle"        "System.IO"
   , warnReexport "IOMode"        "System.IO"
   , warnReexport "ReadMode"      "System.IO"
   , warnReexport "WriteMode"     "System.IO"
   , warnReexport "AppendMode"    "System.IO"
   , warnReexport "ReadWriteMode" "System.IO"
   , warnReexport "stderr"        "System.IO"
   , warnReexport "stdin"         "System.IO"
   , warnReexport "stdout"        "System.IO"
   , warnReexport "withFile"      "System.IO"
   , warnReexport "Down"      "Data.Ord"
   , warnReexport "comparing" "Data.Ord"
   , warnReexport "Coercible" "Data.Coerce"
   , warnReexport "coerce"    "Data.Coerce"
   , warnReexport "Constraint" "Data.Kind"
   , warnReexport "Type"       "Data.Kind"
   , warnReexport "Typeable"   "Data.Typeable"
   , warnReexport "Proxy" "Data.Proxy"
   , warnReexport "Typeable" "Data.Typeable"
   , warnReexport "Void"    "Data.Void"
   , warnReexport "absurd"  "Data.Void"
   , warnReexport "vacuous" "Data.Void"
   , warnReexport "maxInt" "Data.Base"
   , warnReexport "minInt" "Data.Base"
   , warnReexport "ord"    "Data.Base"
   , warnReexport "boundedEnumFrom"     "GHC.Enum"
   , warnReexport "boundedEnumFromThen" "GHC.Enum"
   , warnReexport "Generic" "GHC.Generics"
   , warnReexport "Ratio"       "GHC.Real"
   , warnReexport "Rational"    "GHC.Real"
   , warnReexport "denominator" "GHC.Real"
   , warnReexport "numerator"   "GHC.Real"
   , warnReexport "CmpNat"     "GHC.TypeNats"
   , warnReexport "KnownNat"   "GHC.TypeNats"
   , warnReexport "Nat"        "GHC.TypeNats"
   , warnReexport "SomeNat"    "GHC.TypeNats"
   , warnReexport "natVal"     "GHC.TypeNats"
   , warnReexport "someNatVal" "GHC.TypeNats"
   , warnReexport "CmpNat"     "GHC.TypeLits"
   , warnReexport "KnownNat"   "GHC.TypeLits"
   , warnReexport "Nat"        "GHC.TypeLits"
   , warnReexport "SomeNat"    "GHC.TypeLits"
   , warnReexport "natVal"     "GHC.TypeLits"
   , warnReexport "someNatVal" "GHC.TypeLits"
   , warnReexport "getStackTrace"  "GHC.ExecutionStack"
   , warnReexport "showStackTrace" "GHC.ExecutionStack"
   , warnReexport "IsLabel"   "GHC.OverloadedLabels"
   , warnReexport "fromLabel" "GHC.OverloadedLabels"
   , warnReexport "CallStack"           "GHC.Stack"
   , warnReexport "HasCallStack"        "GHC.Stack"
   , warnReexport "callStack"           "GHC.Stack"
   , warnReexport "currentCallStack"    "GHC.Stack"
   , warnReexport "getCallStack"        "GHC.Stack"
   , warnReexport "prettyCallStack"     "GHC.Stack"
   , warnReexport "prettySrcLoc"        "GHC.Stack"
   , warnReexport "withFrozenCallStack" "GHC.Stack"
   -- Bool
   , warnReexport "guard" "Control.Monad"
   , warnReexport "unless" "Control.Monad"
   , warnReexport "when" "Control.Monad"
   , warnReexport "bool" "Data.Bool"
   -- Container
   , warnReexport "Hashable"     "Data.Hashable"
   , warnReexport "hashWithSalt" "Data.Hashable"
   , warnReexport "HashMap"      "Data.HashMap.Strict"
   , warnReexport "HashSet"      "Data.HashSet"
   , warnReexport "IntMap"       "Data.IntMap.Strict"
   , warnReexport "IntSet"       "Data.IntSet"
   , warnReexport "Map"          "Data.Map.Strict"
   , warnReexport "Sequence"     "Data.Sequence"
   , warnReexport "Set"          "Data.Set"
   , warnReexport "swap"         "Data.Tuple"
   , warnReexport "Vector"       "Data.Vector"
   , warnReexport "IsList"    "GHC.Exts"
   , warnReexport "fromList"  "GHC.Exts"
   , warnReexport "fromListN" "GHC.Exts"
   -- Debug
   , warnReexport "trace"       "Debug.Trace"
   , warnReexport "traceShow"   "Debug.Trace"
   , warnReexport "traceShowId" "Debug.Trace"
   , warnReexport "traceShowM"  "Debug.Trace"
   , warnReexport "traceM"      "Debug.Trace"
   , warnReexport "traceId"     "Debug.Trace"
   -- Deepseq
   , warnReexport "NFData"  "Control.DeepSeq"
   , warnReexport "rnf"     "Control.DeepSeq"
   , warnReexport "deepseq" "Control.DeepSeq"
   , warnReexport "force"   "Control.DeepSeq"
   , warnReexportOp "$!!"   "Control.DeepSeq"
   -- Exception
   , warnReexport "Exception"        "Control.Exception"
   , warnReexport "SomeException"    "Control.Exception"
   , warnReexport "toException"      "Control.Exception"
   , warnReexport "fromException"    "Control.Exception"
   , warnReexport "displayException" "Control.Exception"
   -- Foldable
   , warnReexport "asum"       "Data.Foldable"
   , warnReexport "find"       "Data.Foldable"
   , warnReexport "find"       "Data.Foldable"
   , warnReexport "fold"       "Data.Foldable"
   , warnReexport "foldl'"     "Data.Foldable"
   , warnReexport "foldrM"     "Data.Foldable"
   , warnReexport "forM_"      "Data.Foldable"
   , warnReexport "for_"       "Data.Foldable"
   , warnReexport "sequenceA_" "Data.Foldable"
   , warnReexport "toList"     "Data.Foldable"
   , warnReexport "traverse_"  "Data.Foldable"
   , warnReexport "forM"      "Data.Traversable"
   , warnReexport "mapAccumL" "Data.Traversable"
   , warnReexport "mapAccumR" "Data.Traversable"
   -- Function
   , warnReexportOp ">>>" "Control.Category"
   , warnReexportOp "<<<" "Control.Category"
   , warnReexport "fix" "Data.Function"
   , warnReexport "on"  "Data.Function"
   -- Functor
   , warnReexportOp "&&&" "Control.Arrow"
   , warnReexport "Bifunctor" "Data.Bifunctor"
   , warnReexport "bimap"     "Data.Bifunctor"
   , warnReexport "first"     "Data.Bifunctor"
   , warnReexport "second"    "Data.Bifunctor"
   , warnReexport "void"  "Data.Functor"
   , warnReexportOp "$>"  "Data.Functor"
   , warnReexportOp "<&>" "Data.Functor"
   , warnReexport "Compose"    "Data.Functor.Compose"
   , warnReexport "getCompose" "Data.Functor.Compose"
   , warnReexport "Identity"    "Data.Functor.Identity"
   , warnReexport "runIdentity" "Data.Functor.Identity"
   -- Lifted Concurrent
   , warnReexport "MVar"         "Control.Concurrent.MVar"
   , warnReexport "newEmptyMVar" "Control.Concurrent.MVar"
   , warnReexport "newMVar"      "Control.Concurrent.MVar"
   , warnReexport "putMVar"      "Control.Concurrent.MVar"
   , warnReexport "readMVar"     "Control.Concurrent.MVar"
   , warnReexport "swapMVar"     "Control.Concurrent.MVar"
   , warnReexport "takeMVar"     "Control.Concurrent.MVar"
   , warnReexport "tryPutMVar"   "Control.Concurrent.MVar"
   , warnReexport "tryReadMVar"  "Control.Concurrent.MVar"
   , warnReexport "tryTakeMVar"  "Control.Concurrent.MVar"
   , warnReexport "STM"        "Control.Monad.STM"
   , warnReexport "atomically" "Control.Monad.STM"
   , warnReexport "TVar" "Control.Concurrent.STM.TVar"
   , warnReexport "newTVarIO" "Control.Concurrent.STM.TVar"
   , warnReexport "readTVarIO" "Control.Concurrent.STM.TVar"
   , warnReexport "modifyTVar'" "Control.Concurrent.STM.TVar"
   , warnReexport "newTVar" "Control.Concurrent.STM.TVar"
   , warnReexport "readTVar" "Control.Concurrent.STM.TVar"
   , warnReexport "writeTVar" "Control.Concurrent.STM.TVar"
   -- Lifted File
   , warnReexport "appendFile" "Data.Text.IO"
   , warnReexport "getLine"    "Data.Text.IO"
   , warnReexport "readFile"   "Data.Text.IO"
   , warnReexport "writeFile"  "Data.Text.IO"
   , warnReexport "openFile" "System.IO"
   -- Lifted IORef
   , warnReexport "IORef" "Data.IORef"
   , warnReexport "atomicModifyIORef" "Data.IORef"
   , warnReexport "atomicModifyIORef'" "Data.IORef"
   , warnReexport "atomicWriteIORef" "Data.IORef"
   , warnReexport "modifyIORef" "Data.IORef"
   , warnReexport "modifyIORef'" "Data.IORef"
   , warnReexport "newIORef" "Data.IORef"
   , warnReexport "readIORef" "Data.IORef"
   , warnReexport "writeIORef" "Data.IORef"
   -- List
   , warnReexport "genericDrop"      "Data.List"
   , warnReexport "genericLength"    "Data.List"
   , warnReexport "genericReplicate" "Data.List"
   , warnReexport "genericSplitAt"   "Data.List"
   , warnReexport "genericTake"      "Data.List"
   , warnReexport "group"            "Data.List"
   , warnReexport "inits"            "Data.List"
   , warnReexport "intercalate"      "Data.List"
   , warnReexport "intersperse"      "Data.List"
   , warnReexport "isPrefixOf"       "Data.List"
   , warnReexport "permutations"     "Data.List"
   , warnReexport "sort"             "Data.List"
   , warnReexport "sortBy"           "Data.List"
   , warnReexport "sortOn"           "Data.List"
   , warnReexport "subsequences"     "Data.List"
   , warnReexport "tails"            "Data.List"
   , warnReexport "transpose"        "Data.List"
   , warnReexport "uncons"          "Data.List"
   , warnReexport "unfoldr"          "Data.List"
   , warnReexport "NonEmpty" "Data.NonEmpty"
   , warnReexportOp ":|"     "Data.NonEmpty"
   , warnReexport "nonEmpty" "Data.NonEmpty"
   , warnReexport "head"     "Data.NonEmpty"
   , warnReexport "init"     "Data.NonEmpty"
   , warnReexport "last"     "Data.NonEmpty"
   , warnReexport "tail"     "Data.NonEmpty"
   , warnReexport "sortWith" "GHC.Exts"
   -- Monad
   , warnReexport "ExceptT"    "Control.Monad.Except"
   , warnReexport "runExceptT" "Control.Monad.Except"
   , warnReexport "MonadReader" "Control.Monad.Reader"
   , warnReexport "Reader"      "Control.Monad.Reader"
   , warnReexport "ReaderT"     "Control.Monad.Reader"
   , warnReexport "runReaderT"  "Control.Monad.Reader"
   , warnReexport "ask"         "Control.Monad.Reader"
   , warnReexport "asks"        "Control.Monad.Reader"
   , warnReexport "local"       "Control.Monad.Reader"
   , warnReexport "reader"      "Control.Monad.Reader"
   , warnReexport "runReader"   "Control.Monad.Reader"
   , warnReexport "withReader"  "Control.Monad.Reader"
   , warnReexport "withReaderT" "Control.Monad.Reader"
   , warnReexport "MonadState" "Control.Monad.State.Strict"
   , warnReexport "State"      "Control.Monad.State.Strict"
   , warnReexport "StateT"     "Control.Monad.State.Strict"
   , warnReexport "runStateT"  "Control.Monad.State.Strict"
   , warnReexport "evalState"  "Control.Monad.State.Strict"
   , warnReexport "evalStateT" "Control.Monad.State.Strict"
   , warnReexport "execState"  "Control.Monad.State.Strict"
   , warnReexport "execStateT" "Control.Monad.State.Strict"
   , warnReexport "get"        "Control.Monad.State.Strict"
   , warnReexport "gets"       "Control.Monad.State.Strict"
   , warnReexport "modify"     "Control.Monad.State.Strict"
   , warnReexport "modify'"    "Control.Monad.State.Strict"
   , warnReexport "put"        "Control.Monad.State.Strict"
   , warnReexport "runState"   "Control.Monad.State.Strict"
   , warnReexport "state"      "Control.Monad.State.Strict"
   , warnReexport "withState"  "Control.Monad.State.Strict"
   , warnReexport "MonadIO"    "Control.Monad.Trans"
   , warnReexport "MonadTrans" "Control.Monad.Trans"
   , warnReexport "lift"       "Control.Monad.Trans"
   , warnReexport "liftIO"     "Control.Monad.Trans"
   , warnReexport "IdentityT"    "Control.Monad.Trans.Identity"
   , warnReexport "runIdentityT" "Control.Monad.Trans.Identity"
   , warnReexport "MaybeT"         "Control.Monad.Trans.Maybe"
   , warnReexport "maybeToExceptT" "Control.Monad.Trans.Maybe"
   , warnReexport "exceptToMaybeT" "Control.Monad.Trans.Maybe"
   , warnReexport "MonadPlus"    "Control.Monad"
   , warnReexport "mzero"        "Control.Monad"
   , warnReexport "mplus"        "Control.Monad"
   , warnReexport "filterM"      "Control.Monad"
   , warnReexport "forever"      "Control.Monad"
   , warnReexport "join"         "Control.Monad"
   , warnReexport "mapAndUnzipM" "Control.Monad"
   , warnReexport "mfilter"      "Control.Monad"
   , warnReexport "replicateM"   "Control.Monad"
   , warnReexport "replicateM_"  "Control.Monad"
   , warnReexport "zipWithM"     "Control.Monad"
   , warnReexport "zipWithM_"    "Control.Monad"
   , warnReexportOp "<$!>"       "Control.Monad"
   , warnReexportOp "<=<"        "Control.Monad"
   , warnReexportOp "=<<"        "Control.Monad"
   , warnReexportOp ">=>"        "Control.Monad"
   , warnReexport "MonadFail" "Control.Monad.Fail"
   , warnReexport "catMaybes"   "Data.Maybe"
   , warnReexport "fromMaybe"   "Data.Maybe"
   , warnReexport "isJust"      "Data.Maybe"
   , warnReexport "isNothing"   "Data.Maybe"
   , warnReexport "listToMaybe" "Data.Maybe"
   , warnReexport "mapMaybe"    "Data.Maybe"
   , warnReexport "maybeToList" "Data.Maybe"
   , warnReexport "isLeft"           "Data.Either"
   , warnReexport "isRight"          "Data.Either"
   , warnReexport "lefts"            "Data.Either"
   , warnReexport "partitionEithers" "Data.Either"
   , warnReexport "rights"           "Data.Either"
   -- Monoid
   , warnReexport "All"        "Data.Monoid"
   , warnReexport "getAll"     "Data.Monoid"
   , warnReexport "Alt"        "Data.Monoid"
   , warnReexport "getAlt"     "Data.Monoid"
   , warnReexport "Any"        "Data.Monoid"
   , warnReexport "getAny"     "Data.Monoid"
   , warnReexport "Dual"       "Data.Monoid"
   , warnReexport "getDual"    "Data.Monoid"
   , warnReexport "Endo"       "Data.Monoid"
   , warnReexport "appEndo"    "Data.Monoid"
   , warnReexport "First"      "Data.Monoid"
   , warnReexport "getFirst"   "Data.Monoid"
   , warnReexport "Last"       "Data.Monoid"
   , warnReexport "getLast"    "Data.Monoid"
   , warnReexport "Product"    "Data.Monoid"
   , warnReexport "getProduct" "Data.Monoid"
   , warnReexport "Sum"        "Data.Monoid"
   , warnReexport "getSum"     "Data.Monoid"
   , warnReexport "Option"                 "Data.Semigroup"
   , warnReexport "getOption"              "Data.Semigroup"
   , warnReexport "Semigroup"              "Data.Semigroup"
   , warnReexport "sconcat"                "Data.Semigroup"
   , warnReexport "stimes"                 "Data.Semigroup"
   , warnReexportOp "<>"                   "Data.Semigroup"
   , warnReexport "WrappedMonoid"          "Data.Semigroup"
   , warnReexport "cycle1"                 "Data.Semigroup"
   , warnReexport "mtimesDefault"          "Data.Semigroup"
   , warnReexport "stimesIdempotent"       "Data.Semigroup"
   , warnReexport "stimesIdempotentMonoid" "Data.Semigroup"
   , warnReexport "stimesMonoid"           "Data.Semigroup"
   -- String
   , warnReexport "ByteString" "Data.ByteString"
   , warnReexport "IsString"   "Data.String"
   , warnReexport "fromString" "Data.String"
   , warnReexport "Text"    "Data.Text"
   , warnReexport "lines"   "Data.Text"
   , warnReexport "unlines" "Data.Text"
   , warnReexport "words"   "Data.Text"
   , warnReexport "unwords" "Data.Text"
   , warnReexport "decodeUtf8'"    "Data.Text.Encoding"
   , warnReexport "decodeUtf8With" "Data.Text.Encoding"
   , warnReexport "OnDecodeError"    "Data.Text.Encoding.Error"
   , warnReexport "OnError"          "Data.Text.Encoding.Error"
   , warnReexport "UnicodeException" "Data.Text.Encoding.Error"
   , warnReexport "lenientDecode"    "Data.Text.Encoding.Error"
   , warnReexport "strictDecode"     "Data.Text.Encoding.Error"
   , warnReexport "Read"      "Text.Read"
   , warnReexport "readMaybe" "Text.Read"


   ------------
   -- Lifted
   ------------
   -- Concurrency
   , warnLifted "newEmptyMVar" ""
   , warnLifted "newMVar" "x"
   , warnLifted "putMVar" "x y"
   , warnLifted "readMVar" "x"
   , warnLifted "swapMVar" "x y"
   , warnLifted "takeMVar" "x"
   , warnLifted "tryPutMVar" "x y"
   , warnLifted "tryReadMVar" "x"
   , warnLifted "tryTakeMVar" "x"
   , warnLifted "atomically" "x"
   , warnLifted "newTVarIO" "x"
   , warnLifted "readTVarIO" "x"
   -- Exit
   , warnLifted "exitWith" "x"
   , warnLifted "exitFailure" ""
   , warnLifted "exitSuccess" ""
   , warnLifted "die" "x"
   -- File
   , warnLifted "appendFile" "x y"
   , warnLifted "openFile" "x y"
   , warnLifted "readFile" "x"
   , warnLifted "writeFile" "x y"
   -- IORef
   , warnLifted "newIORef" "x"
   , warnLifted "readIORef" "x"
   , warnLifted "writeIORef" "x y"
   , warnLifted "modifyIORef" "x y"
   , warnLifted "modifyIORef'" "x y"
   , warnLifted "atomicModifyIORef" "x y"
   , warnLifted "atomicModifyIORef'" "x y"
   , warnLifted "atomicWriteIORef" "x y"
   -- Terminal
   , warnLifted "getLine" ""
   , warnLifted "print" "x"
   , warnLifted "putStr" "x"
   , warnLifted "putStrLn" "x"
   , warnLifted "putText" "x"
   , warnLifted "putTextLn" "x"
   , warnLifted "putLText" "x"
   , warnLifted "putLTextLn" "x"
   , warnLifted "putBS" "x"
   , warnLifted "putBSLn" "x"
   , warnLifted "putLBS" "x"
   , warnLifted "putLBSLn" "x"
   ]