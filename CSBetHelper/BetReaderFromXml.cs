using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml;

namespace CSBetHelper
{
    class BetReaderFromXml : BetReader
    {
        public override IEnumerable<Bet> ReadData(string file)
        {
            var xmlDocument = new XmlDocument();
            xmlDocument.Load(file);

            XmlNode root = xmlDocument.DocumentElement;
            if (root == null)
                throw new ArgumentException("file");

            return from XmlNode betElement in root.ChildNodes select ParseBet(betElement);
        }

        private Bet ParseBet(XmlNode node)
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
                        betBuilder.Stake(Helper.ToDouble(text));
                        break;
                    case Constant.ReturnsTag:
                        betBuilder.Returns(Helper.ToDouble(text));
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
            if (matchesList.Count() == 1)
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
                        builder.Koefficient(Helper.ToDouble(text));
                        break;
                    case Constant.SelectionTag:
                        builder.Selection(text);
                        break;
                    case Constant.ResultTag:
                        builder.Result(text);
                        break;
                    default:
                        break;
                }
            }
            return builder.Build();
        }
    }
}
