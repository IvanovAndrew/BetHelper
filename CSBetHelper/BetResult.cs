using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;

namespace CSBetHelper
{
    public enum BetResult
    {
        Win = 0, 
        Refund = 1, 
        Lost = 2, 
        Placed = 3 
    }

    public static class BetResultEx
    {
        private static readonly Dictionary<string, BetResult> strToBetRes = new Dictionary<string, BetResult>();

        static BetResultEx()
        {
            strToBetRes["Won"] = BetResult.Win;
            strToBetRes["Void"] = BetResult.Refund;
            strToBetRes["Lost"] = BetResult.Lost;
            strToBetRes["Placed"] = BetResult.Placed;
        }

        public static bool TryParse(string input, out BetResult res)
        {
            Contract.Requires(!String.IsNullOrEmpty(input));

            return strToBetRes.TryGetValue(input, out res);
        }

        public static string ToString(BetResult betRes)
        {
            return strToBetRes.First(keyValue => keyValue.Value == betRes).Key;
        }
    }
}
