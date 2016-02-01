module Structures

open System
open System.Text.RegularExpressions

open Constant

type MatchInfo(matchStr : string, event : string, selection : string, koefficient : double, result : string) = 

    member this.Match with get() = matchStr
    member this.Event with get() = event
    member this.Selection with get() = selection
    member this.Koefficient with get() = koefficient
    member this.Result with get() = result

type BetType = 
| Single of MatchInfo
| Express of MatchInfo seq

[<AbstractClass>]
type Bet(date : string, stake : double, returns : double, reference : string) =
    
    let extractBetNumber str = 
        let m = Regex.Match(str, referenceRegExpr)
        if m.Success
        then Convert.ToInt32 (m.Groups.["NUM"].Value)
        else failwith "Trying extract stake number failed"

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
            match obj with 
            | :? Bet as bet -> 
                let xNum = extractBetNumber this.Reference
                let betNum = extractBetNumber bet.Reference
                
                if xNum < betNum 
                then -1 
                elif xNum = betNum
                then 0
                else 1
            | _ -> failwith "Unexpected parameter"
        
    override this.Equals obj = 
        let thisComparable = this :> IComparable
        (thisComparable.CompareTo obj) = 0

    override this.GetHashCode() = 
        let betNumber = extractBetNumber this.Reference
        betNumber * 13

type SingleBet(matchInfo, date, stake, returns, reference) =
    inherit Bet(date, stake, returns, reference)

    override this.BetType = Single matchInfo

    member this.Match = matchInfo

type ExpressBet(matches, date, stake, returns, reference) = 
    inherit Bet(date, stake, returns, reference)

    let mutable betType = Express matches

    override this.BetType = betType

    member this.Matches = 
        let allMatches = 
            match this.BetType with 
            | Express m -> m
            | _ -> failwith "Expected Bet.Type = Express"
        allMatches

    member this.AddMatch m = 
        let newMatches = 
            this.Matches
            |> Seq.append [m]
        betType <- Express newMatches

type ParseResult = 
| Success of Bet
| NotFinished of Bet
| Fail of string

type MatchResult = 
| Win
| Refund
| Lost

    static member ToString = 
        function 
        | Win -> wonString
        | Refund -> refundString
        | Lost -> lostString