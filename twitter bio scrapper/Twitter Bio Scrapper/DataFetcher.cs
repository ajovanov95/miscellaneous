using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;

using System.Threading;
using System.Threading.Tasks;

using Newtonsoft.Json.Linq;

namespace Twitter_Bio_Scrapper
{
    public class DataFetcher
    {
        private MainWindow mainWin;

        public TwitterAPI api;

        private Dictionary<string, Func<string, long, JObject>> actions;

        public StreamWriter outputFileStream;

        public DataFetcher(MainWindow mainWin) 
        {
            this.mainWin = mainWin;

            actions = new Dictionary<string, Func<string, long, JObject>>();

            api = new TwitterAPI();

            actions["friends"]   = (sn, c) => api.GetFriends(sn, c);
            actions["followers"] = (sn, c) => api.GetFollowers(sn, c);
        }

        public void Fetch(string[] user_ids) 
        {
            Task fetchTask = Task.Factory.StartNew(() =>
            {
                string screen_name = "";

                MakeOutputFile(String.Join (",", user_ids));

                outputFileStream.WriteLine("namescreen_nameprofile_url,description,url,followers_count,following_count");

                foreach (var screen_name_ in user_ids) 
                {
                    screen_name = screen_name_;

                    try
                    {
                        ActualFetch(screen_name);
                    }
                    catch (UsernameNotFoundException uex)
                    {
                        outputFileStream.WriteLine("There is no user named " + screen_name + ".");
                    }
                    catch (NotAuthorizedException nex)
                    {
                        outputFileStream.WriteLine("Twitter denied access to profile data for " + screen_name + ".");
                    }
                    catch (Exception ex)
                    {
                        Log.WriteLine("DataFetcher::Fetch error." );
                        Log.WriteLine("Message is : " + ex.Message);
                        Log.WriteLine("Method that caused the exception is : " + ex.TargetSite.Name);

                        ResetGui();

                        RunOnUiThread(() => mainWin.GoButton.IsEnabled = true);

                        return;
                    }
                }

                Log.WriteLine("Finished scrapping.");

                outputFileStream.Close();

                RunOnUiThread(() => mainWin.GoButton.IsEnabled = true);

                ResetGui();
            });
        }

        private void ActualFetch(string screen_name) 
        {
            Log.WriteLine("Started getting data for " + screen_name);

            List<long> friends_ids = GetIDs("friends", screen_name);
            List<long> followers_ids = GetIDs("followers", screen_name);

            if (friends_ids.Count == 0 && followers_ids.Count == 0) 
            {
                return;
            }

            if (friends_ids.Count > 0)
            {
                outputFileStream.WriteLine(screen_name +  " --Friends--");
                ProcessIds(friends_ids.ToArray(), screen_name, "friends");
            }

            if (followers_ids.Count > 0)
            {
                outputFileStream.WriteLine(screen_name + " --Followers--");
                ProcessIds(followers_ids.ToArray(), screen_name, "followers");
            }

            ResetGui();
        }

        private void Finish() 
        {
            mainWin.AppStatusLabel.Content = "Idle";

            mainWin.GoButton.IsEnabled = true;
        }

        private List<long> GetIDs(string action_name, string screen_name) 
        {
            Log.WriteLine("Started getting IDs - " + action_name);

            long cursor = -1;
            List<long> ids = new List<long>();

            Func<string, long, JObject> action = actions[action_name];

            bool done = false;

            RunOnUiThread(() =>
            {
                mainWin.AppStatusLabel.Content = String.Format ("{0} - Getting IDs ({1})", screen_name, action_name);
            });

            while (!done)
            {
                try
                {
                    Log.WriteLine("Still getting IDs. Current cursor is : " + cursor.ToString() + " and we have a total of " + ids.Count + " ids thus far.");

                    JObject result = action(screen_name, cursor);

                    JArray json_ids = result["ids"].Value<JArray>();
                    long next_cursor = result["next_cursor"].Value<long>();

                    foreach (long id in json_ids)
                    {
                        ids.Add(id);
                    }

                    if (cursor == 0)
                    {
                        done = true;
                    }

                    cursor = next_cursor;
                }
                catch (RateLimitHitException rex)
                {
                    RateLimits limits = api.GetRateLimits();

                    RunOnUiThread(() => mainWin.rlWin.SetRateLimits(limits));

                    Log.WriteLine("Waiting for rate limits to reset.");

                    if (action_name == "friends" && limits.friendsIds.remaining == 0)
                    {
                        Wait(limits.friendsIds.resetTime, "Waiting for friends IDs rate to reset.");
                    }
                    else if (action_name == "friends" && limits.friendsIds.remaining == 0)
                    {
                        Wait(limits.friendsIds.resetTime, "Waiting for followers IDs rate to reset.");
                    }
                }
            }

            Log.WriteLine("Finished getting IDs");

            RunOnUiThread(() =>
            {
                mainWin.AppStatusLabel.Content = "Finished getting IDs for " + screen_name;
            });

            return ids;
        }

        private void Wait(DateTime end, string info) 
        {
            RunOnUiThread(() =>
            {
                mainWin.AppStatusLabel.Content = info;
            });

            // Add 5 second to fix possible timing issues
            TimeSpan timeToSleep = end - DateTime.Now.ToLocalTime();

            timeToSleep = timeToSleep.Add(new TimeSpan(0, 0, 5));

            Log.WriteLine("The program will wait for " + timeToSleep.ToString());

            Thread.Sleep(timeToSleep);

            Log.WriteLine("The program will now continue execution");

            mainWin.rlWin.GetRates();

            RunOnUiThread(() =>
            {
                mainWin.AppStatusLabel.Content = "Acquiring twitter data";
            });
        }

        private void MakeOutputFile(string screen_name) 
        {
            string desktop_path = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
            string dir_path = desktop_path + "/Twitter Bio Scrapper/";
            string str_date_time = DateTime.Now.ToString().Replace("/", ".").Replace(":", "-");
            string file_path = dir_path + screen_name + " at " + str_date_time + ".csv";

            Directory.CreateDirectory(dir_path);

            outputFileStream = new StreamWriter(new FileStream(file_path, FileMode.OpenOrCreate));

            Log.WriteLine("Created output file at " + file_path);
        }

        private void ProcessIds(long[] ids, string screen_name, string action) 
        {
            for (Int64 i = 0; i < ids.Length; i += 100)
            {
                Int64 end = i + 100 < ids.Length ? i + 100 : ids.Length;

                Int64[] seek_ids = Globals.ArraySlice<Int64>(ids, i, end);

                Log.WriteLine(String.Format ("Getting data for ids in the range {0} - {1}", i, end));

                try
                {
                    GetData(seek_ids, i, ids.Length, screen_name, action);

                    mainWin.rlWin.GetRates();
                }
                catch (RateLimitHitException rex)
                {
                    RateLimits limits = api.GetRateLimits();

                    RunOnUiThread(() => mainWin.rlWin.SetRateLimits(limits));

                    Log.WriteLine("Waiting for rate limits to reset");

                    Wait(limits.usersLookup.resetTime, "Waiting for users lookup rates to reset");

                    GetData(seek_ids, i, ids.Length, screen_name, action);
                }
            }

            mainWin.rlWin.GetRates();
        }

        private void GetData(long[] seek_ids, long progress, long total, string screen_name, string action_name) 
        {
           RunOnUiThread(() =>
            {
                mainWin.AppStatusLabel.Content = String.Format("Fetching data ({0} / {1}) : {2} / {3}", screen_name, action_name, progress, total);

                mainWin.ProgressBar.Value = progress;
                mainWin.ProgressBar.Maximum = total;
            });

            JArray result = api.GetBatchUserData(seek_ids);

            Log.WriteLine("Received data from twitter");

            string output = ProccessUserDataArray(result);

            Log.WriteLine("Generated the ouput");

            outputFileStream.WriteLine(output);

            Log.WriteLine("Written the output to a file");
        }

        private string ProccessUserDataArray(JArray arr)
        {
            List<string> results = new List<string>();

            foreach (JObject elem in arr)
            {
                results.Add(ProccessUserData(elem));
            }

            return String.Join("\n", results);
        }

        private string GetOrDefault(JObject data, string key, string default_) 
        {
            string value = data[key].Value<String>();

            if (String.IsNullOrEmpty(value))
                return default_;
            else
                return value;
        }

        private string ProccessUserData(JObject data)
        {
            string[] fields = new string[5];

            fields[0] = "https://twitter.com/" + data["screen_name"].Value<string>();
            fields[1] = "\"" + GetOrDefault(data, "description", "") + "\"";

            string url = data["url"].Value<string>();

            fields[2] = String.IsNullOrEmpty(url) ? "" : url;
            fields[3] = data["followers_count"].ToString();
            fields[4] = data["friends_count"].ToString();

            return String.Join(",", fields);
        }

        private void RunOnUiThread(Action action) 
        {
            mainWin.Dispatcher.Invoke ((Action)(() => action ()));
        }

        private void ShowMessage(string message) 
        {
            RunOnUiThread(() =>
            {
                mainWin.AppStatusLabel.Content = message;
            });
        }

        private void ResetGui() 
        {
            RunOnUiThread(() =>
            {
                mainWin.AppStatusLabel.Content = "Idle";

                mainWin.ProgressBar.Value = 0;
            });
        }
    }
}
