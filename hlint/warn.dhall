let Rule = ./Rule.dhall
in let rule = constructors Rule
in let warnSimple
    : Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) ->
        rule.Warn {warn =
            { name = None Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = None Text
            }
        }

in let warnNote
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        rule.Warn {warn =
            { name = None Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = Some "${n}"
            }
        }

in let warnReexport
    : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        rule.Warn {warn =
            { name = Some "Use '${f}' from Relude"
            , lhs = "${mod}.${f}"
            , rhs = "${f}"
            , note = Some "'${f}' is already exported from Relude"
            }
        }

in let warnReexportOp : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        rule.Warn {warn =
            { name = Some "Use '${f}' from Relude"
            , lhs = "(${mod}.${f})"
            , rhs = "(${f})"
            , note = Some "Operator '(${f})' is already exported from Relude"
            }
        }

in let warnLifted
    : Text -> Text -> Rule
    =  \(f : Text) -> \(args : Text) ->
        rule.Warn { warn =
            { name = Some "'liftIO' is not needed"
            , lhs = "(liftIO (${f} ${args}))"
            , rhs = "${f}"
            , note = Some "If you import '${f}' from Relude, it's already lifted"
            }
        }

in let hintNote
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        rule.Hint { hint =
            { lhs = "(${lhsTxt})"
            , rhs = "${rhsTxt}"
            , note = Some "${n}"
            }
        }

in { warnSimple     = warnSimple
   , warnNote       = warnNote
   , warnReexport   = warnReexport
   , warnReexportOp = warnReexportOp
   , warnLifted     = warnLifted
   , hintNote       = hintNote
   }