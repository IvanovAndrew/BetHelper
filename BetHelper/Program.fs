open System.Text.RegularExpressions
open System.IO
open System

open Structures
open XmlWriter
open HtmlToBet
open XmlToBet
open Statistic

//let folder = "../../../Database/"

let findFiles mask = 
    let allFiles = Directory.GetFiles <| Directory.GetCurrentDirectory()
    allFiles 
    |> Array.filter (fun s -> Regex.IsMatch(s, mask))
    

[<EntryPoint>]
let main argv = 
    let xmlName = "2015_11_Novermber.xml"
    
    //let regExp = @"2015_May.*\.html"
    let regExp = @"2015_November.*\.html"
    let files = findFiles regExp

    let bets = 
        files 
        |> Array.map 
            (
                fun name -> 
                    let fullPath = (*folder +*) name
                    if Path.GetExtension(name) = ".html"
                    then HtmlExtractor.ExtractData fullPath
                    elif Path.GetExtension(name) = ".xml"
                    then XmlExtractor.ExtractData fullPath
                    else failwithf "Unsupported file extension: %s" <| Path.GetExtension name
            
            )
        |> Array.concat
        |> Array.sort

    writeToXml xmlName bets

//    printfn "Failed %d" failed.Length

    let profit = calculateProfit bets
    let totalStakes = calculateStakes bets
    

    let winStakes = Array.length <| chooseWinBets bets
    let voidStakes = Array.length <| chooseVoidBets bets
    let looseStakes = Array.length <| chooseLooseBets bets

    printfn "Income is $%.2f. Investition is $%.2f" profit totalStakes
    printfn "Count is %d: +%d =%d -%d" bets.Length winStakes voidStakes looseStakes

    let totalMatches = calculateMatches bets
    let winMatches, refundMatches, lostMathces = calculateMatchesResult bets

    printfn "Total matches is %d: +%d =%d -%d" totalMatches winMatches refundMatches lostMathces

//    printfn "Win profit is %f" <| calculateWinProfit bets
    printBets bets
//    printfn ""
    //printNeedMatches bets Win
    0 // return an integer exit code
