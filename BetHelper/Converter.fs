module Converter

open System
open Structures

let toDecimal (str : string) = 
    (str.Replace ('.', ',')).Replace ("$", "")
    |> decimal

let toDate (str : string) = 
    str
    |> Convert.ToDateTime

let toMatchResult (str : string) = 
    if str = MatchResult.WonString then Win
    elif str = MatchResult.RefundString then Refund
    elif str = MatchResult.LostString then Lost
    elif str = MatchResult.PlacedString then Placed
    else invalidArg str <| sprintf "Unexpected parser input: %s" str