module XmlToBet

open System.Xml
open System.Collections.Generic

open Constant
open Helper
open Structures

type private DelayBetType = 
| SingleType
| ExpressType
    
    static member Create str =
        if str = singleString then SingleType
        elif str = expressString then ExpressType
        else failwithf "Expected %s or %s betType, but %s was" singleString expressString str

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

        let matches = new ResizeArray<_>()
    
        for child2 in xmlNode.FirstChild.ChildNodes do
            let mutable _match = ""
            let mutable event = ""
            let mutable selection = ""
            let mutable result = ""
            let mutable koefficient = 0.0

            match child2 with
            | MatchTag str -> _match <- str
            | EventTag str -> event <- str
            | SelectionTag str -> selection <- str
            | KoefficientTag str -> koefficient <- toDouble str
            | ResultTag str -> result <- str
            | Other tag -> failwithf "Uncovered matchInfo tag: %s" tag

            let matchInfo = new MatchInfo(_match, event, selection, koefficient, result)
            matches.Add matchInfo

        matches

    static let parseBet (xmlNode : XmlNode)= 
        let mutable date = ""
        let mutable betType = Unchecked.defaultof<DelayBetType>
        let mutable matchArray = new ResizeArray<_>()
        let mutable stake = 0.0
        let mutable returns = 0.0
        let mutable reference = ""

        let (|Date|BetType|Matches|Stake|Returns|Reference|Other|) (input : XmlNode) = 
            let tag = input.Name
            let text = input.InnerText

            if tag = dateTag then Date(text)
            elif tag = betTypeTag then BetType(DelayBetType.Create text)
            elif tag = matchesTag then Matches(parseMatchInfo input)
            elif tag = stakeTag then Stake(toDouble text)
            elif tag = returnsTag then Returns(toDouble text)
            elif tag = referencesTag then Reference text
            else Other text


        for child in xmlNode.ChildNodes do
            match child with
            | Date d -> date <- d
            | BetType b -> betType <- b
            | Matches m -> matchArray <- m
            | Stake s -> stake <- s
            | Returns r -> returns <- r
            | Reference r -> reference <- r
            | Other tag -> failwithf "Unsupported tag: %s" tag
        
        match betType with
        | SingleType -> new SingleBet(matchArray.[0], date, stake, returns, reference) :> Bet
        | ExpressType -> new ExpressBet(matchArray, date, stake, returns, reference) :> Bet

    static member ExtractData (name : string) = 
        let xmlDocument = new XmlDocument()
        xmlDocument.Load(name)

        let element = xmlDocument.DocumentElement.ChildNodes
        let enumerator = element.GetEnumerator()

        let elementsSeq = seq {
                                while enumerator.MoveNext() do 
                                    yield enumerator.Current :?> XmlNode
                              }
        elementsSeq
        |> Seq.map parseBet