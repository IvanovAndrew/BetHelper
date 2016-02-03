using System.Collections.Generic;

namespace CSBetHelper
{
    abstract class BetReader
    {
        public abstract IEnumerable<Bet> ReadData(string file);
    }
}
