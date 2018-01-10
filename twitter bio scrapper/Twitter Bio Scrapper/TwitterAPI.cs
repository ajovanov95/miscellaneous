using System;
using System.IO;
using System.Net;
using System.Text;
using System.Linq;
using System.Collections.Generic;

using System.Threading;
using System.Threading.Tasks;

using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Twitter_Bio_Scrapper
{
    public class RateLimitHitException : Exception
    {
        public RateLimitHitException() : base ("Rate limit hit")
        {
            
        }
    }

    public class UsernameNotFoundException : Exception 
    {
        public UsernameNotFoundException() : base("Username not found exception") 
        {
            
        }
    }

    public class NotAuthorizedException : Exception 
    {
        public NotAuthorizedException() : base("Lacking proper authorization") 
        {
            
        }
    }

    public class ServiceNotAvailableException : Exception
    {
        public ServiceNotAvailableException()
            : base("No twitter API :(")
        {

        }
    }

    public class NoResponseException : Exception 
    {
        public Uri Uri { get; set; }

        public NoResponseException(Uri uri) : base("No response from twitter") 
        {
            Uri = uri;
        }
    }

    public class TwitterAPI
    {
        public TwitterAPI() 
        {
        
        }

        public string GetBearerToken() 
        {
            string key = Globals.GetBearerAuthorizationKey();

            Uri address = new Uri("https://api.twitter.com/oauth2/token");

            HttpWebRequest request = WebRequest.Create(address) as HttpWebRequest;

            byte[] data = Encoding.UTF8.GetBytes("grant_type=client_credentials");

            request.Method = "POST";

            request.Headers["Authorization"] = "Basic " + key;
            request.ContentType   = "application/x-www-form-urlencoded;charset=UTF-8";
            request.ContentLength = data.Length;

            // Send data
            using (Stream postStream = request.GetRequestStream())
            {
                postStream.Write(data, 0, data.Length);
            }

            try
            {
                // Get and parse the response
                using (HttpWebResponse response = request.GetResponse() as HttpWebResponse)
                {
                    // Get the response stream  
                    StreamReader reader = new StreamReader(response.GetResponseStream());

                    string result = reader.ReadToEnd();

                    JObject json = JObject.Parse(result);

                    string token_type = json["token_type"].ToString();
                    string access_token = json["access_token"].ToString();

                    Log.WriteLine("Received bearer token");

                    return access_token;
                }
            }
            catch (WebException wex)
            {
                HttpWebResponse resp = wex.Response as HttpWebResponse;

                if (resp != null)
                    LogError(resp);
                else
                    Log.WriteLine("Failed to get error data from twitter. .NET exception message is : " + wex.Message);

                if (resp.StatusCode == HttpStatusCode.ServiceUnavailable) 
                {
                    throw new ServiceNotAvailableException();
                }

                return "";
            }
        }

        // Take a dictionary and make args in form of arg1=value&arg2=value2
        private string MakeStrArgs(Dictionary<string, object> args) 
        {
            List<string> results = new List<string>();

            foreach (var entry in args) 
            {
                results.Add(entry.Key + "=" + entry.Value);
            }

            return String.Join("&", results.ToArray());
        }

        private string GetHeadersAsString(HttpWebResponse resp) 
        {
            WebHeaderCollection headers = resp.Headers;

            var keys   = headers.AllKeys;

            string result = "";

            foreach (string key in keys) 
            {
                var values = headers.GetValues(key);

                result += key + " >> " + String.Join(", ", values) + " | ";
            }

            return result;
        }

        private void LogError(HttpWebResponse resp) 
        {
            lock (resp)
            {
                string resp_data = "";

                using (StreamReader reader = new StreamReader(resp.GetResponseStream()))
                {
                    resp_data = reader.ReadToEnd();
                }

                Log.WriteLine("Start of error report");
                Log.WriteLine("Status code is : " + resp.StatusCode);
                Log.WriteLine("Twitter api says the following about the error:");
                Log.WriteLine(resp_data);
                Log.WriteLine("The headers returned are :");
                Log.WriteLine(GetHeadersAsString(resp));
                Log.WriteLine("End of error report");
            }
        }

        private string MakeAPICall(string api_name, Dictionary<string, object> args, int timesCalled = 0)
        {
            lock (this)
            {
                string str_addr = String.Format("https://api.twitter.com/1.1/{0}.json?{1}", api_name, MakeStrArgs(args));

                Uri address = new Uri(str_addr);

                HttpWebRequest request = WebRequest.Create(address) as HttpWebRequest;

                if (request == null) 
                {
                    throw new Exception ("MakeAPICall. request is null.");
                }

                DateTime startTime = DateTime.Now;

                Log.WriteLine("Sending an api request to " + str_addr);

                request.Method = "GET";

                request.Headers["Authorization"] = "Bearer " + Globals.BearerToken;

                try
                {
                    using (HttpWebResponse response = request.GetResponse() as HttpWebResponse)
                    {
                        if (response == null)
                        {
                            throw new NoResponseException(address);
                        }

                        using (StreamReader reader = new StreamReader(response.GetResponseStream()))
                        {
                            string result = reader.ReadToEnd();

                            TimeSpan delta = DateTime.Now - startTime;

                            Log.WriteLine("Received an API response for uri = " + str_addr + " after " + delta);

                            return result;
                        }
                    }
                }
                catch (NoResponseException nre) 
                {
                    Log.WriteLine("Received no response from twitter for uri = " + str_addr);

                    if (timesCalled > 5)
                    {
                        Log.WriteLine("Failed to get data from twitter five times a row.");

                        throw nre;
                    }
                    else 
                    {
                        Log.WriteLine("Trying again to get data from twitter with the same parameters.");

                        return MakeAPICall(api_name, args, timesCalled + 1);
                    }
                }
                catch (WebException wex)
                {
                    HttpWebResponse resp = wex.Response as HttpWebResponse;

                    if (resp != null)
                        LogError(resp);
                    else
                        Log.WriteLine("Failed to get error data from twitter. .NET exception message is : " + wex.Message);

                    if (resp.StatusCode == HttpStatusCode.NotFound)
                    {
                        Log.WriteLine("Twitter says there is no such user.");

                        throw new UsernameNotFoundException();
                    }
                    else if (resp.StatusCode == HttpStatusCode.Unauthorized)
                    {
                        Log.WriteLine("Lack of authorization, profile probably protected.");

                        throw new NotAuthorizedException();
                    }
                    else
                    {
                        throw new RateLimitHitException();
                    }
                }
                catch (Exception ex)
                {
                    Log.WriteLine("Caught a generic error in MakeAPICall. Rethrowing it.");

                    Log.WriteLine("Exception info is : " + ex.Message);

                    throw ex;
                } 
            }
        }

        public JObject GetFriends(string screen_name, long cursor = -1) 
        {
            Log.WriteLine(String.Format ("Made a call to GetFriends with args : {0}, {1}", screen_name, cursor));

            Dictionary<string, object> args = new Dictionary<string, object>();

            args["screen_name"] = screen_name;
            args["cursor"] = cursor.ToString();

            string response = MakeAPICall("friends/ids", args);

            return JObject.Parse(response);
        }

        public JObject GetFollowers(string screen_name, long cursor = -1)
        {
            Log.WriteLine(String.Format("Made a call to GetFollowers with args : {0}, {1}", screen_name, cursor));

            Dictionary<string, object> args = new Dictionary<string, object>();

            args["screen_name"] = screen_name;
            args["cursor"] = cursor.ToString();

            string response = MakeAPICall("followers/ids", args);

            return JObject.Parse(response);
        }

        public JArray GetBatchUserData(long[] ids) 
        {
            if (ids.Length > 100)
            {
                throw new Exception("Twitter accepts only a max of 100 user IDs per batch.");
            }

            Log.WriteLine(String.Format("Made a call to GetBatchUserData"));

            Dictionary<string, object> args = new Dictionary<string, object>();

            args["user_id"] = String.Join(",", ids);

            string response = MakeAPICall("users/lookup", args);

            JArray result = JArray.Parse(response);

            return result;
        }

        public RateLimits GetRateLimits() 
        {
            Dictionary<string, object> args = new Dictionary<string, object>();

            args["resources"] = "followers,friends,users";

            string response = MakeAPICall("application/rate_limit_status", args); ;

            JObject result = JObject.Parse(response);

            JObject resources = result["resources"].Value<JObject>();

            return new RateLimits(resources);
        }
    }
}
