module HtmlToBet

open System.IO
open System.Text.RegularExpressions

open Constant
open Helper
open Structures

let parseMatchInfo(matchStr : string, selectStr : string, result : string) = 
    let splitRes = matchStr.Split(delimiter, System.StringSplitOptions.RemoveEmptyEntries)

    let _match = 
        let str = splitRes.[0].Trim()
        if str.Contains "</a>" 
        then 
            let regRes = Regex.Match(str, @".*>(?<Match>.*)</a>")
            if regRes.Success then regRes.Groups.["Match"].Value
            else str
        else 
            str

    let event = splitRes.[1].Trim()
        
    let selectSplit = selectStr.Split(koeffDelimiter, System.StringSplitOptions.RemoveEmptyEntries)
    let selection = selectSplit.[0].Trim()
    let koeff = 
        let src = selectSplit.[1].Trim()
        let m = Regex.Match(src, @"(?<Koef>\d+\.\d+)\s*")
        if m.Success
        then toDouble m.Groups.["Koef"].Value
        else failwith "Extracting koeff failed"

    let resultTrim = result.Trim()

    MatchInfo(_match, event, selection, koeff, resultTrim)

let parseDate (dateStr : string) = 
    
    let splitRes = (dateStr.Replace ("(MSK)", "")).Split([|'/'|])
    let date = toDate <| splitRes.[0] + splitRes.[1]
    date.ToString()

let parseSingle(matchArray : string array) = 
    let date = parseDate matchArray.[0]

    let matchInfo = parseMatchInfo (matchArray.[4], matchArray.[5], matchArray.[6])

    let stake = toDouble matchArray.[7]
    let returns = toDouble matchArray.[8]
    let reference = matchArray.[9]

    //new SingleBet(matchInfo, date, stake, returns, reference)
    new Bet(date, stake, returns, reference, Single(matchInfo))


let parseExpress(matchArray : string array) = 
    
    let date = parseDate matchArray.[0]

    let matchInfo = parseMatchInfo(matchArray.[4], matchArray.[5], matchArray.[6])
    let arr = new ResizeArray<_>()
    arr.Add (matchInfo)

    let stake = toDouble matchArray.[7]
    let returns = toDouble matchArray.[8]
    let reference = matchArray.[9]

    //new ExpressBet(arr, date, stake, returns, reference)
    Bet(date, stake, returns, reference, Express(arr))

type BetAllParser() = 

    static member ParseText text = 
        let res : Bet list ref = ref []
        let oldData = ref None
        
        let parse matchStr = 
        
            let parseString str = 
                let info = new ResizeArray<_>()
                let bets = Regex.Matches(str, fieldRegExp, RegexOptions.Singleline)
                
                for m in bets do
                    info.Add m.Groups.["HEAD"].Value

                info

            let info = parseString matchStr

            match info.Count with
            | 0 -> printfn "Unmatched %s\n" matchStr
            | 5 -> 
                let matchInfo = parseMatchInfo (info.[2], info.[3], info.[4])
                match !oldData with
                | Some (expressBet : Bet) -> expressBet.AddMatch matchInfo
                | None -> failwithf "There is express match, but express description is absent\n"
            | 10 ->
                match !oldData with
                | Some expressBet -> 
                    res := expressBet :: !res
                    oldData := None
                | None -> ()

                if info.Contains singleString 
                then
                    let singleBet = info.ToArray() |> parseSingle
                    res := singleBet :: !res
                else
                    let express = info.ToArray() |> parseExpress
                    oldData := Some <| express

            | _ -> failwithf "Unexpected info.Count value: %d" info.Count

        let totalReg = Regex.Matches(text, betRegExpr, RegexOptions.Singleline)
        let totalRegSeq = seq {for m in totalReg do yield m.Groups.["Info"].Value}

        totalRegSeq 
        |> Seq.iter parse
        
        match !oldData with
        | Some value -> res := value :: !res
        | None -> ()
        
        !res

type HtmlExtractor() = 

    static member ExtractData name = 
        let text = File.ReadAllText name
        let res = BetAllParser.ParseText text
        res
        |> Seq.ofList