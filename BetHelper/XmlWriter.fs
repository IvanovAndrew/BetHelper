module XmlWriter

open System.IO
open System.Text

open Constant
open Structures

type Printer(out : StringBuilder) = 

    member this.Out with get() = out
        
    member this.Print x = 
        x 
        |> Printf.kprintf this.Out.Append >> ignore

    member this.PrintBr (x : 'a) =
        x
        |> Printf.kprintf (fun s -> this.Out.Append(s).Append(System.Environment.NewLine) |> ignore)

    member this.PrintBrInd num (x : 'a) =
        this.Print "%s" (String.replicate (num <<< 2) " ")
        this.PrintBr x


let matchToXmlString (matchInfo : MatchInfo) tab = 
    
    let printer = new Printer(new StringBuilder())

    printer.PrintBrInd tab "<%s>" matchInfoTag

    printer.PrintBrInd (tab + 1) "<%s>%s</%s>" matchTag matchInfo.Match matchTag
    
    if matchInfo.Event <> "" 
    then printer.PrintBrInd (tab + 1) "<%s>%s</%s>" eventTag matchInfo.Event eventTag

    printer.PrintBrInd (tab + 1) "<%s>%s</%s>" selectionTag matchInfo.Selection selectionTag
    printer.PrintBrInd (tab + 1) "<%s>%.2f</%s>" koefficientTag matchInfo.Koefficient koefficientTag
    printer.PrintBrInd (tab + 1) "<%s>%s</%s>" resultTag <| MatchResult.ToString matchInfo.Result <| resultTag

    printer.PrintBrInd tab "</%s>" matchInfoTag

    printer.Out.ToString()

let writeBet (printer : Printer) (bet : Bet) = 

    printer.PrintBrInd 1 "<%s>" betTag
    printer.PrintBrInd 2 "<%s>%s</%s>" dateTag bet.Date dateTag
    printer.PrintBrInd 2 "<%s>%s</%s>" 
        <| betTypeTag 
        <| BetType.BetTypeToString bet.Matches 
        <| betTypeTag

    match bet.Matches with
    | Single game -> 
        printer.PrintBrInd 2 "<%s>" matchesTag        
        printer.Print "%s" <| matchToXmlString game 3
        printer.PrintBrInd 2 "</%s>" matchesTag

    | Express games ->
        printer.PrintBrInd 2 "<%s>" matchesTag
        games
        |> Seq.iter (fun m -> printer.Print "%s" <| matchToXmlString m 3)
        printer.PrintBrInd 2 "</%s>" matchesTag
    
    printer.PrintBrInd 2 "<%s>%.2f</%s>" stakeTag bet.Stake stakeTag
    printer.PrintBrInd 2 "<%s>%.2f</%s>" returnsTag bet.Returns returnsTag
    printer.PrintBrInd 2 "<%s>%s</%s>" referencesTag bet.Reference referencesTag
    printer.PrintBrInd 1 "</%s>" betTag

let writeToXml (name : string) bets = 

    use out = new StreamWriter(name)

    let printer = new Printer(new StringBuilder())
    printer.PrintBr "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    
    printer.PrintBr "<Data>"
    bets
    |> Seq.iter (writeBet printer)
    printer.PrintBr "</Data>"

    out.WriteLine(printer.Out.ToString())

    out.Close()