module Structures

open System
open System.Collections.Generic
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

type Bet(date : string, stake : double, returns : double, reference : string, matches : BetType) =
    
    let extractBetNumber str = 
        let m = Regex.Match(str, referenceRegExpr)
        if m.Success
        then Convert.ToInt32 (m.Groups.["NUM"].Value)
        else failwith "Trying extract stake number failed"

    let mutable date = date
    let mutable stake = stake
    let mutable returns = returns
    let mutable reference = reference
    let mutable matches = matches

    let compare (bet : Bet) = 
        let xNum = extractBetNumber reference
        let betNum = extractBetNumber bet.Reference
                
        if xNum < betNum 
        then -1 
        elif xNum = betNum
        then 0
        else 1

    member this.Date with get() = date
    member this.Stake 
        with get() = stake
        and set value = stake <- value
    member this.Returns with get() = returns
    member this.Reference with get() = reference
    member this.Matches with get() = matches
        

    member this.AddMatch matchInfo = 
        let newMatches = 
            match matches with
            | Express games -> 
                games
                |> Seq.append [matchInfo]
            | Single _ -> invalidOp "Cann't add match to single bet"
        matches <- Express(newMatches)
    
    interface System.IComparable with
        member this.CompareTo other = 
            match other with
            | :? Bet as bet -> compare bet
            | _ -> failwith "other"

    interface System.IComparable<Bet> with
        member this.CompareTo(bet : Bet) = 
            compare bet
        
    override this.Equals obj = 
        match obj with
        | :? Bet as bet -> 
            let thisAsComparer = this :> IComparable<Bet>
            (thisAsComparer.CompareTo bet) = 0
        | _ -> false

    override this.GetHashCode() = 
        let betNumber = extractBetNumber this.Reference
        betNumber * 13

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