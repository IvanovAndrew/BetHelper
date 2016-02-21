open System.IO
open System.Text.RegularExpressions

open Constant
open Structures
open XmlWriter
open HtmlToBet
open XmlToBet
open Statistic

let dbFolder = "../../../Database/"

let findFiles mask = 
    let allFiles = 
        let tempFiles = Directory.GetFiles <| Directory.GetCurrentDirectory()
        let dbFiles = Directory.GetFiles(dbFolder)
        
        Array.append tempFiles dbFiles

    allFiles 
    |> Array.filter (fun s -> Regex.IsMatch(s, mask))

let parseFile fileName = 
    
    let (|Html|Htm|Xml|Other|) extension = 
        if extension = ".html" then Html
        elif extension = ".htm" then Htm
        elif extension = ".xml" then Xml
        else Other(extension)

    let fullPath = fileName
    match Path.GetExtension(fileName) with
    | Html 
    | Htm -> HtmlExtractor.ExtractData fullPath
    | Xml -> XmlExtractor.ExtractData fullPath
    | Other x -> 
        let message = sprintf "Unsupported file extension: %s" x
        invalidArg fileName message
    
[<EntryPoint>]
let main argv = 
    //let xmlName = "2014.xml"
    
    //let regExp = @"2015_May.*\.html?"
    let regExp = @"Demo\.xml"
    let files = findFiles regExp
    printfn "%A files are found" files

    let bets = 
        files 
        |> Array.map parseFile
        |> Seq.concat
        |> Seq.sort

    //writeToXml xmlName bets

//    printfn "Failed %d" failed.Length

    let profit = calculateProfit bets
    let totalStakes = calculateStakes bets
    
    let winStakes = Seq.length <| chooseWinBets bets
    let voidStakes = Seq.length <| chooseVoidBets bets
    let looseStakes = Seq.length <| chooseLooseBets bets

    printfn "Income is $%.2f. Investition is $%.2f" profit totalStakes
    printfn "Count is %d: +%d =%d -%d" <| Seq.length bets <| winStakes <| voidStakes <| looseStakes

    let totalMatches = calculateMatches bets
    let winMatches, refundMatches, lostMathces = calculateMatchesResult bets

    printfn "Total matches is %d: +%d =%d -%d" totalMatches winMatches refundMatches lostMathces

//    printfn "Win profit is %f" <| calculateWinProfit bets
    //printBets bets
//    printfn ""
    //printNeedMatches bets Lost
    0 // return an integer exit code