module XmlToBet

open System
open System.Xml
open System.Collections.Generic

open Constant
open Helper
open Structures


type XmlExtractor() = 

    static member ExtractData (name : string) : Bet[] = 

        let xmlDocument = new XmlDocument()
        xmlDocument.Load (name)

        let mutable element = xmlDocument.DocumentElement.ChildNodes
        let result = Queue<Bet>()

        for bet in element do
            let mutable date = ""
            let mutable betType = Unchecked.defaultof<BetType>
            let mutable matchArray = new ResizeArray<_>()
            let mutable matchInfo = Unchecked.defaultof<MatchInfo>
            let mutable stake = 0.0
            let mutable returns = 0.0
            let mutable reference = ""
            for child in bet.ChildNodes do
                
                if child.Name = dateTag 
                then 
                    date <- child.InnerText
                elif child.Name = betTypeTag 
                then
                    betType <-
                        if child.InnerText = singleString then Single(matchInfo)
                        elif child.InnerText = expressString then Express(matchArray)
                        else failwithf "Expected %s or %s betType" singleString expressString child.InnerText

                elif child.Name = matchesTag 
                then    
                    let matches = new ResizeArray<_>()
                    
                    for child2 in child.FirstChild.ChildNodes do
                                    
                        let mutable _match = ""
                        let mutable event = ""
                        let mutable selection = ""
                        let mutable result = ""
                        let mutable koefficient = 0.0

                        if child2.Name = matchTag 
                        then _match <- child2.InnerText
                        elif child2.Name =  eventTag 
                        then event <- child2.InnerText
                        elif child2.Name =  selectionTag 
                        then selection <- child2.InnerText
                        elif child2.Name = koefficientTag
                        then koefficient <- toDouble child2.InnerText
                        elif child2.Name =  resultTag 
                        then result <- child2.InnerText
                        else failwithf "Uncovered matchInfo tag: %s" child2.Name

                        matchInfo <- new MatchInfo(_match, event, selection, koefficient, result)
                        matches.Add matchInfo

                    matchArray <- matches
                        
                elif child.Name = stakeTag
                then 
                    stake <- toDouble child.InnerText
                elif child.Name = returnsTag 
                then
                    returns <- toDouble child.InnerText
                elif child.Name = referencesTag 
                then 
                    reference <- child.InnerText           
            
            match betType with
            | Single _ -> 
                matchInfo <- matchArray.[0]
                let single = new SingleBet(matchInfo, date, stake, returns, reference)
                result.Enqueue single
            | Express _ -> 
                let express = new ExpressBet(matchArray, date, stake, returns, reference)
                result.Enqueue express

        result.ToArray()