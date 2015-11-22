module Constant

let singleString = "Single"
let expressString = "Express"

let wonString = "Won"
let lostString = "Lost"
let placedString = "Placed"
let refundString = "Void"

let fieldRegExp = @"<td.*?>(?<HEAD>.*?)</td>"
let expressRegExp = @".*\((?<NUMBER>\d+)\).*"

let dollar = '$'
let delimiter = [|"&nbsp;–&nbsp;"|]
let koeffDelimiter = [|"@"|]

let bodyRegexp = @"<tbody>(?<DATA>.*)</tbody>"
let betRegExpr = @"<tr>\s+(?<Info>.*?)\s+</tr>"
let referenceRegExpr = @"O/6351351/(?<NUM>\d*)/F"

let betTag = "Bet"
let dateTag = "Date"
let betTypeTag = "BetType"
let matchesTag = "Matches"
let stakeTag = "Stake"
let returnsTag = "Returns"
let referencesTag = "Reference"

let matchInfoTag = "MatchInfo"
let matchTag = "Match"
let eventTag = "Event"
let selectionTag = "Selection"
let koefficientTag = "Koefficient"
let resultTag = "Result"