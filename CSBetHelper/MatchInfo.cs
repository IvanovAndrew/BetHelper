using System;
using System.Diagnostics.Contracts;

namespace CSBetHelper
{
    public class MatchInfo
    {
        private string _match;
        private string _event;
        private string _selection;
        private decimal _koefficient;
        private BetResult _result;
        
        public string Match { get { return _match; } }
        public string Event { get { return _event; } }
        public string Selection {get { return _selection; }}
        public decimal Koefficient {get { return _koefficient; }}
        public BetResult Result { get { return _result; } }

        private MatchInfo()
        {
        }

        public static MatchInfoBuilder With()
        {
            return new MatchInfoBuilder(new MatchInfo());
        }

        public class MatchInfoBuilder
        {
            private readonly MatchInfo _matchInfo;
            internal MatchInfoBuilder(MatchInfo matchInfo)
            {
                _matchInfo = matchInfo;
            }

            public MatchInfoBuilder Match(string match)
            {
                Contract.Requires(!String.IsNullOrEmpty(match));

                _matchInfo._match = match;
                return this;
            }

            public MatchInfoBuilder Event(string _event)
            {
                Contract.Requires(!String.IsNullOrEmpty(_event));

                _matchInfo._event = _event;
                return this;
            }

            public MatchInfoBuilder Selection(string selection)
            {
                Contract.Requires(!String.IsNullOrEmpty(selection));

                _matchInfo._selection = selection;
                return this;
            }

            public MatchInfoBuilder Koefficient(decimal koefficient)
            {
                Contract.Requires(koefficient >= 1);
                
                _matchInfo._koefficient = koefficient;
                return this;
            }

            public MatchInfoBuilder Result(BetResult result)
            {
                _matchInfo._result = result;
                return this;
            }

            public MatchInfo Build()
            {
                Contract.Ensures(!String.IsNullOrEmpty(_matchInfo.Match));
                Contract.Ensures(!String.IsNullOrEmpty(_matchInfo.Event));
                Contract.Ensures(!String.IsNullOrEmpty(_matchInfo.Selection));
                Contract.Ensures(_matchInfo.Koefficient >= 1);

                return _matchInfo;
            }
        }
    }
}
