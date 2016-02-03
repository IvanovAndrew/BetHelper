namespace CSBetHelper
{
    public class MatchInfo
    {
        private string _match;
        private string _event;
        private string _selection;
        private double _koefficient;
        private string _result;
        
        public string Match { get { return _match; } }
        public string Event { get { return _event; } }
        public string Selection {get { return _selection; }}
        public double Koefficient {get { return _koefficient; }}
        public string Result { get { return _result; } }

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
                _matchInfo._match = match;
                return this;
            }

            public MatchInfoBuilder Event(string _event)
            {
                _matchInfo._event = _event;
                return this;
            }

            public MatchInfoBuilder Selection(string selection)
            {
                _matchInfo._selection = selection;
                return this;
            }

            public MatchInfoBuilder Koefficient(double koefficient)
            {
                _matchInfo._koefficient = koefficient;
                return this;
            }

            public MatchInfoBuilder Result(string result)
            {
                _matchInfo._result = result;
                return this;
            }

            public MatchInfo Build()
            {
                return _matchInfo;
            }
        }
    }
}
