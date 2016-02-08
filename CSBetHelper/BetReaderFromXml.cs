using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml;

namespace CSBetHelper
{
    class BetReaderFromXml : BetReader
    {
        private readonly XmlParser _betParser = new XmlParser(); 

        public override IEnumerable<Bet> ReadData(string file)
        {
            var xmlDocument = new XmlDocument();
            xmlDocument.Load(file);

            XmlNode root = xmlDocument.DocumentElement;
            if (root == null)
                throw new ArgumentException("file");

            return from XmlNode betElement in root.ChildNodes select _betParser.ParseBet(betElement);
        }
    }

    
}
