using System;
using System.Linq;
using System.Xml;

namespace CSBetHelper
{
    internal class XmlParser
    {
        internal Bet ParseBet(XmlNode node)
        {
            var betBuilder = Bet.With();
            foreach (XmlNode childNode in node.ChildNodes)
            {
                var text = childNode.InnerText;
                switch (childNode.Name)
                {
                    case Constant.DateTag:
                        betBuilder.DateTime(Convert.ToDateTime(text));
                        break;
                    case Constant.StakeTag:
                        betBuilder.Stake(Converter.ToDecimal(text));
                        break;
                    case Constant.ReturnsTag:
                        betBuilder.Returns(Converter.ToDecimal(text));
                        break;
                    case Constant.ReferencesTag:
                        betBuilder.Reference(text);
                        break;
                    case Constant.MatchesTag:
                        betBuilder.Matches(ParseMatches(childNode));
                        break;
                    default:
                        break;
                }
            }
            return betBuilder.Build();
        }

        private OneOrMany ParseMatches(XmlNode node)
        {
            var matchesList = (from XmlNode child in node.ChildNodes select ParseMatchInfo(child)).ToList();

            OneOrMany res;
            if (matchesList.Count == 1)
                res = new One(matchesList[0]);
            else
                res = new Many(matchesList);
            return res;
        }

        private MatchInfo ParseMatchInfo(XmlNode node)
        {
            var builder = MatchInfo.With();
            foreach (XmlNode child in node.ChildNodes)
            {
                string text = child.InnerText;
                switch (child.Name)
                {
                    case Constant.MatchTag:
                        builder.Match(text);
                        break;
                    case Constant.EventTag:
                        builder.Event(text);
                        break;
                    case Constant.KoefficientTag:
                        builder.Koefficient(Converter.ToDecimal(text));
                        break;
                    case Constant.SelectionTag:
                        builder.Selection(text);
                        break;
                    case Constant.ResultTag:
                        BetResult betRes;
                        if (BetResultEx.TryParse(text, out betRes))
                            builder.Result(betRes);
                        else
                            throw new InvalidOperationException(String.Format("Cann't parse betResult: {0}", text));
                        break;
                    default:
                        break;
                }
            }
            return builder.Build();
        }
    }
}
