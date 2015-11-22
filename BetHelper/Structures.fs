module Structures

open System
open System.Text.RegularExpressions

open Constant

type MatchInfo(_match : string, event : string, selection : string, koefficient : double, result : string) = 

    member this.Match with get() = _match
    member this.Event with get() = event
    member this.Selection with get() = selection
    member this.Koefficient with get() = koefficient
    member this.Result with get() = result

type BetType = 
| Single of MatchInfo
| Express of ResizeArray<MatchInfo>

[<AbstractClass>]
type Bet(date : string, stake : double, returns : double, reference : string) =
    
    let mutable date = date
    let mutable stake = stake
    let mutable returns = returns
    let mutable reference = reference

    member this.Date with get() = date
    member this.Stake 
        with get() = stake
        and set value = stake <- value
    member this.Returns with get() = returns
    member this.Reference with get() = reference
    abstract BetType : BetType with get
    
    interface System.IComparable with
        member this.CompareTo(obj: obj): int = 
            
            let extractBetNumber str = 
                let m = Regex.Match (str, referenceRegExpr)
                if m.Success
                then Convert.ToInt32 (m.Groups.["NUM"].Value)
                else failwith "Trying extract stake number failed"

            match obj with 
            | :? Bet as bet -> 
                let xNum = extractBetNumber this.Reference
                let betNum = extractBetNumber bet.Reference
                
                if xNum < betNum then -1 else 1
            | _ -> failwith "Unexpected parameter"
        

type SingleBet(matchInfo, date, stake, returns, reference) =
    inherit Bet(date, stake, returns, reference)

    override this.BetType = Single matchInfo

    member this.Match = matchInfo

type ExpressBet(matches, date, stake, returns, reference) = 
    inherit Bet(date, stake, returns, reference)

    override this.BetType = Express matches

    member this.Matches = 
        let allMatches = 
            match this.BetType with 
            | Express m -> m
            | _ -> failwith "Expected Bet.Type = Express"
        allMatches

type ParseResult = 
| Success of Bet
| NotFinished of Bet
| Fail of string

type MatchResult = 
| Win
| Refund
| Lost

let matchResultToString = 
    function 
    | Win -> wonString
    | Refund -> refundString
    | Lost -> lostString