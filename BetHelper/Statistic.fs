module Statistic

open Constant
open Structures
open System

let calculateProfit (bets : Bet seq) = 
    bets
    |> Seq.fold (fun sum bet -> sum + bet.Returns - bet.Stake) 0.0

let calculateStakes (bets : Bet seq) = 
    bets
    |> Seq.fold (fun sum bet -> sum + bet.Stake) 0.0

let calculateMatches (bets : Bet seq) = 
    bets
    |> Seq.fold 
        (
            fun sum bet -> 
                match bet.BetType with
                | Single _ -> sum + 1
                | Express matches -> sum + Seq.length matches
        ) 0

let chooseWinBets (bets: Bet seq) = 
    bets
    |> Seq.filter (fun bet -> bet.Returns > bet.Stake)

let chooseVoidBets (bets: Bet seq) = 
    bets
    |> Seq.filter (fun bet -> bet.Returns = bet.Stake)

let chooseLooseBets (bets: Bet seq) = 
    bets
    |> Seq.filter (fun bet -> bet.Returns < bet.Stake)

let printBets (bets : Bet seq) = 
    bets 
    |> Seq.iter 
        (fun bet -> 
            printfn "%s\t%.2f - %.2f = %.2f\t%s" bet.Date bet.Returns bet.Stake (bet.Returns - bet.Stake) bet.Reference
        )

let calculateWinProfit (bets : Bet seq) = 
    bets
    |> chooseWinBets
    |> calculateProfit

let calculateMatchesResult (bets : Bet seq) = 
    let won = ref 0
    let refund = ref 0
    let lost = ref 0

    let mainFunc (game : MatchInfo) = 
        if game.Result = wonString then incr won
        elif game.Result = refundString then incr refund
        elif game.Result = lostString then incr lost
        elif game.Result = placedString then incr lost

    bets
    |> Seq.iter 
        (
            fun bet -> 
                match bet with 
                | :? SingleBet as single -> mainFunc single.Match
                | :? ExpressBet as express -> 
                    express.Matches
                    |> Seq.iter mainFunc
                | x -> failwithf "Unexpected bet type: %A" x
        )
    !won, !refund, !lost

let printNeedMatches (bets : Bet seq) needResult = 
    
    let needString = MatchResult.ToString needResult
    let count = ref 0

    let isNeedMatch (matchInfo : MatchInfo) = 
        matchInfo.Result = needString

    let print date _match reference = 
        incr count
        printfn "%d %s %s %s" !count date _match reference

    let printBet (bet : Bet) = 
        match bet with 
        | :? SingleBet as single -> 
            if isNeedMatch single.Match 
            then print bet.Date single.Match.Match bet.Reference
        
        | :? ExpressBet as express -> 
            let winMatches = 
                express.Matches
                |> Seq.filter isNeedMatch
            
            winMatches
            |> Seq.iter(fun matchInfo -> print bet.Date matchInfo.Match bet.Reference)

        | _ -> failwithf "Expected singleBet or ExpressBet, but received %A" bet

    bets 
    |> Seq.iter printBet