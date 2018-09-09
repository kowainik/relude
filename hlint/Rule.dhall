< Arguments :
    { arguments : List Text }
| Ignore :
    { ignore : {name : Text} }
| Warn :
    { warn :
        { name : Optional Text
        , lhs  : Text
        , rhs  : Text
        , note : Optional Text
        }
    }
| Hint :
    { hint :
        { lhs  : Text
        , rhs  : Text
        , note : Optional Text
        }
    }
>