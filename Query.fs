module Qry.Query

type BinaryExpressionKind =
    | Add
    | Subtract
    | Multiply
    | Divide
    | And
    | Or
    | Equals
    | NotEquals
    | GreaterThan
    | GreaterThanOrEquals
    | LesserThan
    | LesserThanOrEquals

type Expression =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | Identifier of string
    | Binary of (Expression * Expression * BinaryExpressionKind)
