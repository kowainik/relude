let Rule = ./Rule.dhall
let warnSimple
    : Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) ->
        Rule.Warn
        { warn =
            { name = None Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = None Text
            }
        }

let warnNote
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        Rule.Warn {warn =
            { name = None Text
            , lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = Some "${n}"
            }
        }

let warnReexport
    : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        Rule.Warn
        { warn =
            { name = Some "Use '${f}' from Relude"
            , lhs = "${mod}.${f}"
            , rhs = "${f}"
            , note = Some "'${f}' is already exported from Relude"
            }
        }

let warnReexportOp : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        Rule.Warn
        { warn =
            { name = Some "Use '${f}' from Relude"
            , lhs = "(${mod}.${f})"
            , rhs = "(${f})"
            , note = Some "Operator '(${f})' is already exported from Relude"
            }
        }

let warnLifted
    : Text -> Text -> Rule
    =  \(f : Text) -> \(args : Text) ->
        Rule.Warn
        { warn =
            { name = Some "'liftIO' is not needed"
            , lhs = "(liftIO (${f} ${args}))"
            , rhs = "${f}"
            , note = Some "If you import '${f}' from Relude, it's already lifted"
            }
        }

let hintNote
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        Rule.Hint
        { hint =
            { lhs = "${lhsTxt}"
            , rhs = "${rhsTxt}"
            , note = Some "${n}"
            }
        }

let hintNoteOp
    : Text -> Text -> Text -> Rule
    = \(lhsTxt : Text) -> \(rhsTxt : Text) -> \(n : Text) ->
        hintNote "(${lhsTxt})" rhsTxt n

in { warnSimple
   , warnNote
   , warnReexport
   , warnReexportOp
   , warnLifted
   , hintNote
   , hintNoteOp
   }