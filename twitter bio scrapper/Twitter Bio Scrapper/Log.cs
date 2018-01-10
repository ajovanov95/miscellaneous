using System;
using System.IO;

namespace Twitter_Bio_Scrapper
{
    public class Log
    {
        private static StreamWriter writer;

        public static void Init() 
        {
            string desktop_path = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
            string dir_path = desktop_path + "/Twitter Bio Scrapper/runtime-logs/";
            string str_date_time = DateTime.Now.ToString().Replace("/", ".").Replace(":", "-");
            string file_path = dir_path + "runtime-log-" + str_date_time + ".txt";

            Directory.CreateDirectory(dir_path);

            writer = new StreamWriter(new FileStream (file_path, FileMode.OpenOrCreate));
        }

        public static bool IsNull() 
        {
            return writer == null;
        }

        public static void WriteLine(string message) 
        {
            string timestamp = "[" + DateTime.Now.ToString() + "]";

            //lock (writer)
            //{
                writer.WriteLine(timestamp + " " + message);
            //}
        }

        public static void Close() 
        {
            writer.Close();
        }
    }
}
