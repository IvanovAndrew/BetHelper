using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;

namespace CSBetHelper
{
    public class BetStatistic
    {
        public int Won { get; private set; }
        public int Void { get; private set; }
        public int Lost { get; private set; }
        public int Total { get; private set; }
        public decimal Profit { get; private set; }
        public decimal Stake { get; private set; }

        public BetStatistic(IEnumerable<Bet> bets)
        {
            Contract.Requires(bets != null);

            Total = bets.Count();
            Won = bets.Count(bet => bet.Returns > bet.Stake);
            Void = bets.Count(bet => bet.Returns == bet.Stake);
            Lost = bets.Count(bet => bet.Returns < bet.Stake);
            Profit = CalculateProfit(bets);
            Stake = CalculateStakes(bets);
        }

        public override string ToString()
        {
            return String.Format("Profit: {0} Investition: {1:F}\nBets count {2}: +{3} ={4} -{5}",
                                this.Profit,
                                this.Stake,
                                this.Total,
                                this.Won,
                                this.Void,
                                this.Lost);
        }

        private decimal CalculateProfit(IEnumerable<Bet> bets)
        {
            return bets.Sum(bet => bet.Returns - bet.Stake);
        }

        private decimal CalculateStakes(IEnumerable<Bet> bets)
        {
            Contract.Ensures(Contract.Result<decimal>() >= 0);

            return bets.Sum(bet => bet.Stake);
        }
    }

    public class MatchStatistic
    {
        public int Won { get; private set; }
        public int Void { get; private set; }
        public int Lost { get; private set; }
        public int Total { get; private set; }

        public MatchStatistic(IEnumerable<Bet> bets)
        {
            Total = GetMatchesCount(bets, info => true);
            Won = GetMatchesCount(bets, info => info.Result == BetResult.Win);
            Void = GetMatchesCount(bets, info => info.Result == BetResult.Lost || info.Result == BetResult.Placed);
            Lost = GetMatchesCount(bets, info => info.Result == BetResult.Lost);
        }

        private int GetMatchesCount(IEnumerable<Bet> bets, Func<MatchInfo, bool> predicate)
        {
            Contract.Ensures(Contract.Result<int>() >= 0);

            return bets.Sum(bet => bet.Matches.GetAll().Count(predicate));
        }

        public override string ToString()
        {
            return String.Format("Matches count {0}: +{1} ={2} -{3}",
                                this.Total,
                                this.Won,
                                this.Void,
                                this.Lost);
        }
    }
}
