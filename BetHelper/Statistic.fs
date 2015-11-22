module Statistic

open Constant
open Structures

let calculateProfit (bets : Bet array) = 
    bets
    |> Array.fold (fun sum bet -> sum + bet.Returns - bet.Stake) 0.0

let calculateStakes (bets : Bet array) = 
    bets
    |> Array.fold (fun sum bet -> sum + bet.Stake) 0.0

let calculateMatches (bets : Bet array) = 
    bets
    |> Array.fold 
        (
            fun sum bet -> 
                match bet.BetType with
                | Single _ -> sum + 1
                | Express matches -> sum + matches.Count
        ) 0

let chooseWinBets (bets: Bet array) = 
    bets
    |> Array.filter (fun bet -> bet.Returns > bet.Stake)

let chooseVoidBets (bets: Bet array) = 
    bets
    |> Array.filter (fun bet -> bet.Returns = bet.Stake)

let chooseLooseBets (bets: Bet array) = 
    bets
    |> Array.filter (fun bet -> bet.Returns < bet.Stake)

let printBets (bets : Bet array) = 
    bets 
    |> Array.iter 
        (fun bet -> 
            printfn "%s\t%.2f - %.2f = %.2f\t%s" bet.Date bet.Returns bet.Stake (bet.Returns - bet.Stake) bet.Reference
        )

let calculateWinProfit (bets : Bet array) = 
    bets
    |> chooseWinBets
    |> calculateProfit

let calculateMatchesResult (bets : Bet array) = 
    let won = ref 0
    let refund = ref 0
    let lost = ref 0

    let mainFunc (game : MatchInfo) = 
        if game.Result = wonString then incr won
        elif game.Result = refundString then incr refund
        elif game.Result = lostString then incr lost
        elif game.Result = placedString then incr lost

    bets
    |> Array.iter 
        (
            fun bet -> 
                match bet with 
                | :? SingleBet as single -> mainFunc single.Match
                | :? ExpressBet as express -> 
                    for game in express.Matches do
                        mainFunc game
        )

    !won, !refund, !lost

let printNeedMatches (bets : Bet array) needResult = 
    
    let needString = matchResultToString needResult
    let count = ref 0

    let isNeedMatch (matchInfo : MatchInfo) = 
        matchInfo.Result = needString

    let print date _match reference = 
        incr count
        printfn "%d %s %s %s" !count date _match reference


    bets 
    |> Array.iter 
        (
            fun bet -> 
                match bet with 
                | :? SingleBet as single -> 
                    if isNeedMatch single.Match 
                    then print bet.Date single.Match.Match bet.Reference
                | :? ExpressBet as express -> 
                    let winMatches = express.Matches.FindAll(fun matchInfo -> isNeedMatch matchInfo)
                    winMatches.ForEach(fun matchInfo -> print bet.Date matchInfo.Match bet.Reference)
                        
                | _ -> failwithf "Expected singleBet or ExpressBet, but received %s" <| bet.GetType().ToString()
        )