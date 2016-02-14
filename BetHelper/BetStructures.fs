module Structures

open System
open System.Text.RegularExpressions

open Constant

type MatchResult = 
| Win
| Refund
| Lost
| Placed
    
    static member WonString = "Won"
    static member LostString = "Lost"
    static member PlacedString = "Placed"
    static member RefundString = "Void"

    static member AreEqual one two = 
        match one, two with
        | Win, Win
        | Refund, Refund
        | Lost, Lost 
        | Placed, Placed -> true
        | _ -> false
    
    override this.ToString() = 
        match this with
        | Win -> MatchResult.WonString
        | Refund -> MatchResult.RefundString
        | Lost -> MatchResult.LostString
        | Placed -> MatchResult.PlacedString

type MatchInfo = 
    {
        Match : string
        Event : string
        Selection : string
        Koefficient : decimal
        Result : MatchResult
    }

type BetType = 
| Single of MatchInfo
| Express of MatchInfo seq

    static member SingleString = "Single"
    static member ExpressString = "Express"
    
    override this.ToString() = 
        match this with
        | Single _ -> BetType.SingleString
        | Express _ -> BetType.ExpressString

type Bet(date : string, stake : decimal, returns : decimal, reference : string, matches : BetType) =
    
    let extractBetNumber str = 
        let m = Regex.Match(str, referenceRegExpr)
        if m.Success
        then int m.Groups.["NUM"].Value
        else invalidArg "str" "Trying extract stake number failed"

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
    member this.Stake with get() = stake
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