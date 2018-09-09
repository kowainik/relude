let Rule = ./Rule.dhall
in let rule = constructors Rule
in let warnSimple
    : Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) ->
        rule.Warn {warn =
            { name = [] : Optional Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = [] : Optional Text
            }
        }

in let warnNote
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        rule.Warn {warn =
            { name = [] : Optional Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = ["${n}"] : Optional Text
            }
        }

in let warnReexport
    : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        rule.Warn {warn =
            { name = ["Use '${f}' from Relude"] : Optional Text
            , lhs = "${mod}.${f}"
            , rhs = "${f}"
            , note = ["Function '${f}' is already exported from Relude"] : Optional Text
            }
        }

in let warnReexportOp : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        rule.Warn {warn =
            { name = ["Use '${f}' from Relude"] : Optional Text
            , lhs = "(${mod}.${f})"
            , rhs = "(${f})"
            , note = ["Operator '(${f})' is already exported from Relude"] : Optional Text
            }
        }

in let warnLifted
    : Text -> Text -> Rule
    =  \(f : Text) -> \(args : Text) ->
        rule.Warn { warn =
            { name = ["'liftIO' is not needed"] : Optional Text
            , lhs = "(liftIO (${f} ${args}))"
            , rhs = "${f}"
            , note = ["If you import '${f}' from Relude, it's already lifted"] : Optional Text
            }
        }

in let hintNote
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        rule.Hint { hint =
            { lhs = "(${lhsTxt})"
            , rhs = "${rhsTxt}"
            , note = ["${n}"] : Optional Text
            }
        }

in { warnSimple     = warnSimple
   , warnNote       = warnNote
   , warnReexport   = warnReexport
   , warnReexportOp = warnReexportOp
   , warnLifted     = warnLifted
   , hintNote       = hintNote
   }