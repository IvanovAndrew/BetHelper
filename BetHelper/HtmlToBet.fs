﻿module HtmlToBet

open System.IO
open System.Text.RegularExpressions

open Constant
open Converter
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
        then toDecimal m.Groups.["Koef"].Value
        else invalidOp "Extracting koeff failed"

    let resultType = 
        result.Trim() |> toMatchResult

    let res : MatchInfo = 
        {
            Match = _match; 
            Event = event; 
            Selection = selection; 
            Koefficient = koeff; 
            Result = resultType;
        }
    res

let parseDate (dateStr : string) = 
    
    let splitRes = (dateStr.Replace ("(MSK)", "")).Split([|'/'|])
    let date = toDate <| splitRes.[0] + splitRes.[1]
    date.ToString()

let parseSingle(matchArray : string array) = 
    let date = parseDate matchArray.[0]

    let matchInfo = parseMatchInfo (matchArray.[4], matchArray.[5], matchArray.[6])

    let stake = toDecimal matchArray.[7]
    let returns = toDecimal matchArray.[8]
    let reference = matchArray.[9]

    new Bet(date, stake, returns, reference, Single(matchInfo))


let parseExpress(matchArray : string array) = 
    
    let date = parseDate matchArray.[0]

    let matchInfo = parseMatchInfo(matchArray.[4], matchArray.[5], matchArray.[6])
    let arr = new ResizeArray<_>()
    arr.Add (matchInfo)

    let stake = toDecimal matchArray.[7]
    let returns = toDecimal matchArray.[8]
    let reference = matchArray.[9]

    Bet(date, stake, returns, reference, Express(arr))

type BetAllParser() = 

    static member ParseText text = 
        let res : Bet list ref = ref []
        let oldData = ref None
        
        let parse matchStr = 
        
            let parseString str = 
                let info = new ResizeArray<_>()
                let bets = Regex.Matches(str, fieldRegExp, RegexOptions.Singleline)
                
                let betsSeq = seq{for m in bets do yield m}
                
                betsSeq
                |> Seq.map (fun m -> m.Groups.["HEAD"].Value)
                |> Array.ofSeq

            let info = parseString matchStr

            match info.Length with
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

                if info |> Array.exists ((=) BetType.SingleString)
                then
                    let singleBet = parseSingle info
                    res := singleBet :: !res
                else
                    let express = parseExpress info
                    oldData := Some <| express

            | _ -> failwithf "Unexpected info.Count value: %d" info.Length

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