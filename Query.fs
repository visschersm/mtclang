module Qry.Query

open FParsec

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

type OrderDirection =
    | Ascending
    | Descending

type Statement =
    | FilterBy of Expression
    | OrderBy of Expression * OrderDirection
    | Skip of int
    | Take of int

type Query = { Statements: Statement list }

let whitespace = skipMany (skipChar ' ')
let whitespace1 = skipMany1 (skipChar ' ')

let quote : Parser<_, unit> = skipChar '\''

let stringLiteral = quote >>. manyCharsTill anyChar quote |>> Expression.StringLiteral .>> whitespace

let intOrFloatLiteral : Parser<Expression, unit> = 
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
            if n.IsInteger then Expression.IntLiteral (int n.String)
            else Expression.FloatLiteral (float n.String)
    .>> whitespace

let identifier = many1Chars (letter <|> digit) |>> Expression.Identifier .>> whitespace

let opp = OperatorPrecedenceParser<Expression, _, _>()

opp.TermParser <- choice [
    intOrFloatLiteral
    stringLiteral
    identifier
]

opp.AddOperator <| InfixOperator("*", whitespace, 1, Associativity.Left, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.Multiply))
opp.AddOperator <| InfixOperator("/", whitespace, 2, Associativity.Left, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.Divide))
opp.AddOperator <| InfixOperator("-", whitespace, 3, Associativity.Left, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.Subtract))
opp.AddOperator <| InfixOperator("+", whitespace, 4, Associativity.Left, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.Add))
opp.AddOperator <| InfixOperator("&&", whitespace, 5, Associativity.Left, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.And))
opp.AddOperator <| InfixOperator("||", whitespace, 6, Associativity.Left, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.Or))
opp.AddOperator <| InfixOperator("=", whitespace, 7, Associativity.None, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.Equals))
opp.AddOperator <| InfixOperator("!=", whitespace, 8, Associativity.None, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.NotEquals))
opp.AddOperator <| InfixOperator(">", whitespace, 9, Associativity.None, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.GreaterThan))
opp.AddOperator <| InfixOperator(">=", whitespace, 10, Associativity.None, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.GreaterThanOrEquals))
opp.AddOperator <| InfixOperator("<", whitespace, 11, Associativity.None, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.LesserThan))
opp.AddOperator <| InfixOperator("<=", whitespace, 12, Associativity.None, fun x y -> Expression.Binary(x, y, BinaryExpressionKind.LesserThanOrEquals))

let expression = opp.ExpressionParser

let orderDirectionAscending = skipString "asc" >>% OrderDirection.Ascending .>> whitespace
let orderDirectionDescending = skipString "desc" >>% OrderDirection.Descending .>> whitespace
let orderDirection = orderDirectionAscending <|> orderDirectionDescending

let filterBy = skipString "filterby" >>. whitespace1 >>. expression .>> whitespace |>>  Statement.FilterBy
let orderBy = skipString "orderby" >>. whitespace1 >>. expression .>>. orderDirection .>> whitespace |>>  Statement.OrderBy
let skip = skipString "skip" >>. whitespace1 >>. pint32 .>> whitespace |>>  Statement.Skip
let take = skipString "take" >>. whitespace1 >>. pint32 .>> whitespace |>>  Statement.Take

let statement = choice [
    filterBy
    orderBy
    skip
    take
]

let query = sepEndBy statement skipNewline |>> function s -> { Statements = s }

let queryFull = spaces >>. query .>> spaces .>> eof

let parse input =
    match run queryFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err