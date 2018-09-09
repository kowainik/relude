let Rule = ./Rule.dhall
in let rule = constructors Rule
in let simple
    : Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) ->
        rule.Warn {warn =
            { name = [] : Optional Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = [] : Optional Text
            }
        }

in let noteWarn
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        rule.Warn {warn =
            { name = [] : Optional Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = ["${n}"] : Optional Text
            }
        }

in let reexportWarn
    : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        rule.Warn {warn =
            { name = ["Use '${f}' from Relude"] : Optional Text
            , lhs = "${mod}.${f}"
            , rhs = "${f}"
            , note = [] : Optional Text
            }
        }

in let reexportWarnOp : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        rule.Warn {warn =
            { name = ["Use '${f}' from Relude"] : Optional Text
            , lhs = "(${mod}.${f})"
            , rhs = "(${f})"
            , note = [] : Optional Text
            }
        }

in let liftWarn
    : Text -> Text -> Rule
    =  \(f : Text) -> \(args : Text) ->
        rule.Warn { warn =
            { name = ["'liftIO' is not needed"] : Optional Text
            , lhs = "(liftIO (${f} ${args}))"
            , rhs = "Relude.${f}"
            , note = ["If you import '${f}' from Relude, it's already lifted"] : Optional Text
            }
        }

in let noteHint
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        rule.Hint { hint =
            { lhs = "(${lhsTxt})"
            , rhs = "${rhsTxt}"
            , note = ["${n}"] : Optional Text
            }
        }

in { warnSimple     = simple
   , warnNote       = noteWarn
   , warnReexport   = reexportWarn
   , warnReexportOp = reexportWarnOp
   , warnLifted     = liftWarn
   , hintNote       = noteHint
   }