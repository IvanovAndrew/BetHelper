module Statistic

open Structures

let calculateProfit (bets : Bet seq) = 
    bets
    |> Seq.sumBy (fun bet -> bet.Returns - bet.Stake)

let calculateStakes (bets : Bet seq) = 
    bets
    |> Seq.sumBy (fun bet -> bet.Stake)

let calculateMatches (bets : Bet seq) = 
    bets
    |> Seq.sumBy 
        (
            fun bet -> 
                match bet.Matches with
                | Single _ -> 1
                | Express matches -> Seq.length matches
        )

let chooseWinBets (bets : Bet seq) = 
    bets
    |> Seq.filter (fun bet -> bet.Returns > bet.Stake)

let chooseVoidBets (bets : Bet seq) = 
    bets
    |> Seq.filter (fun bet -> bet.Returns = bet.Stake)

let chooseLooseBets (bets : Bet seq) = 
    bets
    |> Seq.filter (fun bet -> bet.Returns < bet.Stake)

let printBets (bets : Bet seq) = 
    bets 
    |> Seq.iter 
        (
            fun bet -> 
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
        match game.Result with
        | Win -> incr won
        | Refund -> incr refund
        | Lost 
        | Placed -> incr lost

    bets
    |> Seq.iter 
        (
            fun bet -> 
                match bet.Matches with 
                | Single game -> mainFunc game
                | Express games -> 
                    games
                    |> Seq.iter mainFunc
        )
    !won, !refund, !lost

let printNeedMatches (bets : Bet seq) needResult = 
    
    let count = ref 0

    let isNeedMatch (matchInfo : MatchInfo) = 
        MatchResult.AreEqual matchInfo.Result needResult

    let print date _match reference = 
        incr count
        printfn "%d %s %s %s" !count date _match reference

    let printBet (bet : Bet) = 
        match bet.Matches with 
        | Single game -> 
            if isNeedMatch game 
            then print bet.Date game.Match bet.Reference
        
        | Express games -> 
            let winMatches = 
                games
                |> Seq.filter isNeedMatch
            
            winMatches
            |> Seq.iter(fun matchInfo -> print bet.Date matchInfo.Match bet.Reference)

    bets 
    |> Seq.iter printBet