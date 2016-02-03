using System;

namespace CSBetHelper
{
    static class Helper
    {
        public static double ToDouble(string str)
        {
            var replacedString = (str.Replace('.', ',')).Replace("$", "");
            return Convert.ToDouble(replacedString);
        }
    }
}
