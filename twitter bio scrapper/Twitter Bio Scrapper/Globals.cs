using System;
using System.Web;
using System.Text;

namespace Twitter_Bio_Scrapper
{
    public static class Globals
    {
        private static readonly string _API_KEY = "RrzWbHrgOZHV78EwqXg7b6KYV";
        private static readonly string _API_SECRET = "xDMjFKp6CUiLSqH9tvfc5pggC2o3Q2t6a7jdcHW6J5FITAMJJO";

        public static string BearerToken { get; set; } 

        private static readonly string[] UriRfc3986CharsToEscape = new[] { "!", "*", "'", "(", ")" };
        public static string EscapeUriDataStringRfc3986(string value)
        {
            StringBuilder escaped = new StringBuilder(Uri.EscapeDataString(value));

            for (int i = 0; i < UriRfc3986CharsToEscape.Length; i++)
            {
                escaped.Replace(UriRfc3986CharsToEscape[i], Uri.HexEscape(UriRfc3986CharsToEscape[i][0]));
            }

            return escaped.ToString();
        }

        public static string GetBearerAuthorizationKey() 
        {
            string encoded_api_key    = EscapeUriDataStringRfc3986(_API_KEY);
            string encoded_api_secret = EscapeUriDataStringRfc3986(_API_SECRET);

            string almost_final_key = String.Format("{0}:{1}", encoded_api_key, encoded_api_secret);

            byte[] chars = Encoding.UTF8.GetBytes(almost_final_key);

            return Convert.ToBase64String (chars);
        }

        public static T[] ArraySlice<T>(T[] arr, long start, long end)
        {
            T[] res = new T[end - start];

            long i2 = 0;

            for (long i = start; i < end; i++)
            {
                res[i2] = arr[i];

                i2++;
            }

            return res;
        }
    }
}
