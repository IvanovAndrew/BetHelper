module XmlToBet

open System.Xml
open System.Collections.Generic

open Constant
open Helper
open Structures

let isEmpty str = str = ""

type private DelayBetType = 
| SingleType
| ExpressType
    
    static member Create str =
        if str = singleString then SingleType
        elif str = expressString then ExpressType
        else failwithf "Expected %s or %s betType, but %s was" singleString expressString str

type private OneOrManyMatch = 
| One of MatchInfo
| Many of MatchInfo seq

type XmlExtractor() = 
    
    static let parseMatchInfo (xmlNode : XmlNode) = 
        let (|MatchTag|EventTag|SelectionTag|KoefficientTag|ResultTag|Other|) (input : XmlNode) =
            let text = input.InnerText
            let tag = input.Name

            if tag = matchTag then MatchTag (text)
            elif tag = eventTag then EventTag (text)
            elif tag = selectionTag then SelectionTag (text)
            elif tag = koefficientTag then KoefficientTag (text)
            elif tag = resultTag then ResultTag (text)
            else Other(tag)

        let mutable _match = ""
        let mutable event = ""
        let mutable selection = ""
        let mutable result = ""
        let mutable koefficient = 0.0

        for child2 in xmlNode.ChildNodes do
            match child2 with
            | MatchTag str -> _match <- str
            | EventTag str -> event <- str
            | SelectionTag str -> selection <- str
            | KoefficientTag str -> koefficient <- toDouble str
            | ResultTag str -> result <- str
            | Other tag -> failwithf "Uncovered matchInfo tag: %s" tag
        
        assert not (isEmpty _match || isEmpty event || isEmpty selection || isEmpty result)
        new MatchInfo(_match, event, selection, koefficient, result)

    static let parseMatches (xmlNode : XmlNode) = 
        
        if xmlNode.ChildNodes.Count = 1 
        then 
            let matchInfo = parseMatchInfo xmlNode.FirstChild
            One(matchInfo)
        else
            let childsSeq = seq {for child in xmlNode.ChildNodes do yield child}
            
            let matchesInfo = 
                childsSeq
                |> Seq.map parseMatchInfo
            Many(matchesInfo)

    static let parseBet (xmlNode : XmlNode)= 
        let mutable date = ""
        let mutable betType = Unchecked.defaultof<DelayBetType>
        let mutable matches = Unchecked.defaultof<OneOrManyMatch>
        let mutable stake = 0.0
        let mutable returns = 0.0
        let mutable reference = ""

        let (|Date|BetType|Matches|Stake|Returns|Reference|Other|) (input : XmlNode) = 
            let tag = input.Name
            let text = input.InnerText

            if tag = dateTag then Date(text)
            elif tag = betTypeTag then BetType(DelayBetType.Create text)
            elif tag = matchesTag then Matches(parseMatches input)
            elif tag = stakeTag then Stake(toDouble text)
            elif tag = returnsTag then Returns(toDouble text)
            elif tag = referencesTag then Reference text
            else Other text

        for child in xmlNode.ChildNodes do
            match child with
            | Date d -> date <- d
            | BetType b -> betType <- b
            | Matches m -> matches <- m
            | Stake s -> stake <- s
            | Returns r -> returns <- r
            | Reference r -> reference <- r
            | Other tag -> failwithf "Unsupported tag: %s" tag
        
        match betType, matches with
        | SingleType, One game -> new SingleBet(game, date, stake, returns, reference) :> Bet
        | ExpressType, Many games -> new ExpressBet(games, date, stake, returns, reference) :> Bet
        | _ -> failwith "Unexpected configuration in match parsing"

    static member ExtractData (name : string) = 
        let xmlDocument = new XmlDocument()
        xmlDocument.Load(name)

        let element = xmlDocument.DocumentElement.ChildNodes

        let elementsSeq = seq {for elem in element do yield elem} 

        elementsSeq
        |> Seq.map parseBet