using System;
using System.Collections.Generic;
using System.IO;

namespace CSBetHelper
{
    class BetWriter
    {
        private void WriteMatchInfo(StreamWriter writer, MatchInfo matchInfo)
        {
            writer.WriteLine("\t\t\t<{0}>", Constant.MatchInfoTag); 
            
            writer.WriteLine("\t\t\t\t<{0}>{1}</{0}>", Constant.MatchTag, matchInfo.Match);

            if (!String.IsNullOrEmpty(matchInfo.Event))
                writer.WriteLine("\t\t\t\t<{0}>{1}</{0}>", Constant.EventTag, matchInfo.Event);

            writer.WriteLine("\t\t\t\t<{0}>{1}</{0}>", Constant.SelectionTag, matchInfo.Selection);
            writer.WriteLine("\t\t\t\t<{0}>{1:F}</{0}>", Constant.KoefficientTag, matchInfo.Koefficient);
            writer.WriteLine("\t\t\t\t<{0}>{1}</{0}>", Constant.ResultTag, BetResultEx.ToString(matchInfo.Result));

            writer.WriteLine("\t\t\t</{0}>", Constant.MatchInfoTag); 
        }
        
        private void WriteBet(StreamWriter writer, Bet bet)
        {
            writer.WriteLine("\t<{0}>", Constant.BetTag); 
            writer.WriteLine("\t\t<{0}>{1}</{0}>", Constant.DateTag, bet.Date);

            writer.WriteLine("\t\t<{0}>", Constant.MatchesTag);
            foreach (MatchInfo matchInfo in bet.Matches.GetAll())
            {
                WriteMatchInfo(writer, matchInfo);
            }
            writer.WriteLine("\t\t</{0}>", Constant.MatchesTag); 
            //about matchInfo

            writer.WriteLine("\t\t<{0}>{1:F}</{0}>", Constant.StakeTag, bet.Stake);
            writer.WriteLine("\t\t<{0}>{1:F}</{0}>", Constant.ReturnsTag, bet.Returns);
            writer.WriteLine("\t\t<{0}>{1}</{0}>", Constant.ReferencesTag, bet.Reference);
            writer.WriteLine("\t</{0}>", Constant.BetTag);
        }
        
        public void WriteBets(string name, IEnumerable<Bet> bets)
        {
            using (var writer = new StreamWriter(name))
            {
                writer.WriteLine("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
                writer.WriteLine("<Data>");
                foreach (var bet in bets)
                {
                    WriteBet(writer, bet);
                }
                writer.WriteLine("</Data>");
            }
        }
    }
}
