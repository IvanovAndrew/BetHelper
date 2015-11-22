module XmlWriter

open System.Text

open Constant
open Structures


let print (out : StringBuilder) (x : 'a) =
    Printf.kprintf (fun s -> out.Append s |> ignore) x

let printBr (out : StringBuilder) (x : 'a) =
    Printf.kprintf (fun s -> out.Append(s).Append(System.Environment.NewLine) |> ignore) x

let printBrInd (out : StringBuilder) num (x : 'a) =
    print out "%s" (String.replicate (num <<< 2) " ")
    printBr out x

type Printer(out : StringBuilder) = 

    member this.Out with get() = out
        
    member this.Print x = 
        Printf.kprintf (fun s -> this.Out.Append s |> ignore) x

    member this.PrintBr (x : 'a) =
        Printf.kprintf (fun s -> this.Out.Append(s).Append(System.Environment.NewLine) |> ignore) x

    member this.PrintBrInd num (x : 'a) =
        print out "%s" (String.replicate (num <<< 2) " ")
        printBr out x


let matchToXmlString (matchInfo : MatchInfo) tab = 
    
    let printer = new Printer(new StringBuilder())

    printer.PrintBrInd tab "<%s>" matchInfoTag

    printer.PrintBrInd (tab + 1) "<%s>%s</%s>" matchTag matchInfo.Match matchTag
    
    if matchInfo.Event <> "" 
    then printer.PrintBrInd (tab + 1) "<%s>%s</%s>" eventTag matchInfo.Event eventTag

    printer.PrintBrInd (tab + 1) "<%s>%s</%s>" selectionTag matchInfo.Selection selectionTag
    printer.PrintBrInd (tab + 1) "<%s>%.2f</%s>" koefficientTag matchInfo.Koefficient koefficientTag
    printer.PrintBrInd (tab + 1) "<%s>%s</%s>" resultTag matchInfo.Result resultTag

    printer.PrintBrInd tab "</%s>" matchInfoTag

    printer.Out.ToString()


let writeBet (printer : Printer) (bet : Bet) = 

    printer.PrintBrInd 1 "<%s>" betTag
    printer.PrintBrInd 2 "<%s>%s</%s>" dateTag bet.Date dateTag

    match bet with
    | :? SingleBet as stake -> 
        
        printer.PrintBrInd 2 "<%s>%s</%s>" betTypeTag singleString betTypeTag
        printer.PrintBrInd 2 "<%s>" matchesTag
        
        let matchString = matchToXmlString stake.Match 3
        printer.Print "%s" matchString
        
        printer.PrintBrInd 2 "</%s>" matchesTag

    | :? ExpressBet as express ->
        
        printer.PrintBrInd 2 "<%s>%s</%s>" betTypeTag expressString betTypeTag
        printer.PrintBrInd 2 "<%s>" matchesTag

        for m in express.Matches do
            let matchString = matchToXmlString m 3
            printer.Print "%s" matchString

        printer.PrintBrInd 2 "</%s>" matchesTag
        
    | _ -> failwith "Unexpected Bet. Expected SingleBet or ExpressBet"

    
    printer.PrintBrInd 2 "<%s>%.2f</%s>" stakeTag bet.Stake stakeTag
    printer.PrintBrInd 2 "<%s>%.2f</%s>" returnsTag bet.Returns returnsTag
    printer.PrintBrInd 2 "<%s>%s</%s>" referencesTag bet.Reference referencesTag
    printer.PrintBrInd 1 "</%s>" betTag

let writeToXml (name : string) (bets : Bet array) = 

    use out = new System.IO.StreamWriter(name)

    let printer = new Printer(new StringBuilder())
    printer.PrintBr "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    
    printer.PrintBr "<Data>"
    bets
    |> Array.iter (fun bet -> writeBet printer bet)
    printer.PrintBr "</Data>"

    out.WriteLine(printer.Out.ToString())

    out.Close()
