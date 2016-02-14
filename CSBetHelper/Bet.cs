using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text.RegularExpressions;
using CSBetHelper;

namespace CSBetHelper
{
    public class Bet : IComparable<Bet>
    {
        private DateTime _date;
        private decimal _stake;
        private decimal _returns;
        private string _reference;
        private OneOrMany _matches;
        
        public DateTime Date { get { return _date; } }
        public decimal Stake { get { return _stake; } }
        public decimal Returns { get { return _returns; } }
        public string Reference { get { return _reference; } }
        public OneOrMany Matches { get { return _matches; } }

        private Bet()
        {
        }

        private int GetBetNumber(string strId)
        {
            Match m = Regex.Match(strId, Constant.ReferenceRegExp);
            
            if (m.Success)
                return Convert.ToInt32(m.Groups["NUM"].Value);
            return -1;
        }

        public int CompareTo(Bet other)
        {
            var thisNum = GetBetNumber(Reference);
            var otherNum = GetBetNumber(other.Reference);

            if (thisNum < otherNum)
                return -1;
            return thisNum == otherNum ? 0 : 1;
        }

        public override bool Equals(object obj)
        {
            var bet = obj as Bet;
            if (bet == null)
                return false;
            return CompareTo(bet) == 0;
        }

        public override int GetHashCode()
        {
            return Reference.GetHashCode();
        }

        public static BetBuilder With()
        {
            return new BetBuilder(new Bet());
        }


        public class BetBuilder
        {
            private readonly Bet _bet;
            public BetBuilder(Bet bet)
            {
                _bet = bet;
            }

            public BetBuilder DateTime(DateTime dateTime)
            {
                _bet._date = dateTime;
                return this;
            }

            public BetBuilder Stake(decimal stake)
            {
                Contract.Requires(stake > 0);

                _bet._stake = stake;
                return this;
            }

            public BetBuilder Returns(decimal returns)
            {
                Contract.Requires(returns >= 0);

                _bet._returns = returns;
                return this;
            }

            public BetBuilder Reference(string reference)
            {
                Contract.Requires(!String.IsNullOrEmpty(reference));
                
                _bet._reference = reference;
                return this;
            }

            public BetBuilder Matches(OneOrMany matches)
            {
                Contract.Requires(matches != null);

                _bet._matches = matches;
                return this;
            }

            public Bet Build()
            {
                Contract.Ensures(_bet.Stake > 0);
                Contract.Ensures(_bet.Returns >= 0);
                Contract.Ensures(!String.IsNullOrEmpty(_bet.Reference));
                Contract.Ensures(_bet.Matches != null);

                return _bet;
            }
        }
    }
}

public abstract class OneOrMany
{
    public static OneOrMany Create(IEnumerable<MatchInfo> matches)
    {
        Contract.Requires(matches != null);
        Contract.Requires(matches.Any());

        OneOrMany res;
        if (matches.Count() == 1)
            res = new One(matches.First());
        else
            res = new Many(matches);
        return res;
    }

    public abstract IEnumerable<MatchInfo> GetAll();
}

public class One : OneOrMany
{
    public MatchInfo MatchInfo { get; private set; }

    public One(MatchInfo matchInfo)
    {
        Contract.Requires(matchInfo != null);
        
        MatchInfo = matchInfo;
    }

    public override IEnumerable<MatchInfo> GetAll()
    {
        Contract.Ensures(Contract.Result<IEnumerable<MatchInfo>>() != null);

        return new List<MatchInfo> {MatchInfo};
    }
}

public class Many : OneOrMany
{
    public List<MatchInfo> MatchInfos { get; set; }

    public Many(IEnumerable<MatchInfo> matchesList)
    {
        Contract.Requires(matchesList != null);
        Contract.Requires(matchesList.Count() > 1);
        
        MatchInfos = matchesList.ToList();
    }

    public override IEnumerable<MatchInfo> GetAll()
    {
        Contract.Ensures(MatchInfos != null);

        return MatchInfos;
    }
}
