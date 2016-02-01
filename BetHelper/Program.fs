open System.IO
open System.Text.RegularExpressions

open XmlWriter
open HtmlToBet
open XmlToBet
open Statistic

//let folder = "../../../Database/"

let findFiles mask = 
    let allFiles = Directory.GetFiles <| Directory.GetCurrentDirectory()
    allFiles 
    |> Array.filter (fun s -> Regex.IsMatch(s, mask))

let parseFile fileName = 
    
    let (|Html|Htm|Xml|Other|) extension = 
        if extension = ".html" then Html
        elif extension = ".htm" then Htm
        elif extension = ".xml" then Xml
        else Other(extension)

    let fullPath = (*folder +*) fileName
    match Path.GetExtension(fileName) with
    | Html 
    | Htm -> HtmlExtractor.ExtractData fullPath
    | Xml -> XmlExtractor.ExtractData fullPath
    | Other x -> failwithf "Unsupported file extension: %s" x
    
[<EntryPoint>]
let main argv = 
    let xmlName = "2015_11_Novermber.xml"
    
    //let regExp = @"2015_May.*\.html"
    let regExp = @"2015_November.*\.html"
    let files = findFiles regExp

    let bets = 
        files 
        |> Array.map parseFile
        |> Seq.concat
        |> Seq.sort

    writeToXml xmlName bets

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
    printBets bets
//    printfn ""
    //printNeedMatches bets Win
    0 // return an integer exit code