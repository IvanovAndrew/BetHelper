using System;
using System.IO;
using System.Linq;

namespace CSBetHelper
{
    class Program
    {
        private static string dbFolder = "../../../Database/";
        private static string fileName = "2016_01_January.xml";

        static void Main(string[] args)
        {
            var reader = new BetReaderFromXml();
            var bets = reader.ReadData(dbFolder + fileName);

            Console.WriteLine(bets.Count());
            string newFile = "temp.xml";
            var writer = new BetWriter();
            writer.WriteBets(newFile, bets);
        }
    }
}
