open Qry.Query

let input = """
filterby Category = 'Fantasy'
orderby Rating desc
skip 5
take 10
"""

let result = parse input

match result with
| Result.Ok res -> printfn "%A" res
| Result.Error err -> printfn "%A" err

