using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Globalization;

using Newtonsoft.Json.Linq;

namespace Twitter_Bio_Scrapper
{
    public class RateLimit 
    {
        public int remaining;
        public int limit;

        public DateTime resetTime;

        public RateLimit(JObject data) 
        {
            remaining = data["remaining"].Value<int>();
            limit = data["limit"].Value<int>();

            int dateAsInt = data["reset"].Value<int>();

            resetTime = Epoch.AddSeconds(Convert.ToDouble(dateAsInt));

            resetTime = resetTime.ToLocalTime();
        }

        public override string ToString()
        {
            return remaining + " / " + limit + "\t" + resetTime;
        }

        public static readonly DateTime Epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
    }

    public class RateLimits
    {
        public RateLimit friendsIds;
        public RateLimit followersIds;
        public RateLimit usersLookup;

        public RateLimits(JObject resources) 
        {
            JObject friends = resources["friends"].Value<JObject>();
            JObject _friendsIds = friends["/friends/ids"].Value<JObject>();

            JObject followers = resources["followers"].Value<JObject>();
            JObject _followersIds = followers["/followers/ids"].Value<JObject>();

            JObject users = resources["users"].Value<JObject>();
            JObject _usersLookup = users["/users/lookup"].Value<JObject>();

            friendsIds   = new RateLimit(_friendsIds);
            followersIds = new RateLimit(_followersIds);
            usersLookup = new RateLimit(_usersLookup);
        }

        public override string ToString()
        {
            return "Friends IDs :\t" + friendsIds.ToString() + "\n" +
                   "Followers IDs:\t" + followersIds.ToString() + "\n" +
                   "Users lookup :\t" + usersLookup.ToString();
        }
    }
}
