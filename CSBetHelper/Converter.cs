using System;

namespace CSBetHelper
{
    static class Converter
    {
        public static decimal ToDecimal(string str)
        {
            var replacedString = (str.Replace('.', ',')).Replace("$", "");
            return Convert.ToDecimal(replacedString);
        }
    }
}
