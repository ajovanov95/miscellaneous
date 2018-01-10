using System;
using System.Windows;

using System.Threading.Tasks;
using Newtonsoft.Json.Linq;

namespace Twitter_Bio_Scrapper
{
    public partial class RateLimitsWindow : Window
    {
        private MainWindow mainWindow;

        private TwitterAPI api;

        public RateLimitsWindow(MainWindow mainWin)
        {
            InitializeComponent();

            mainWindow = mainWin;

            api = new TwitterAPI();
        }

        public void GetRates()
        {
            Task.Factory.StartNew(() =>
            {
                try
                {
                    RateLimits limits = api.GetRateLimits();

                    Log.WriteLine("Finished getting new rate limits");

                    Dispatcher.Invoke((Action)(() =>
                    {
                        SetRateLimits(limits);
                    }));
                }
                catch (RateLimitHitException rex) 
                {
                    Log.WriteLine("Failed to get rate limits because of the rate limits on rate limits checking.");

                    // Glorious invocation
                    Dispatcher.Invoke((Action)(() =>
                    {
                        TemporaryTextBox.SelectAll();

                        TemporaryTextBox.Selection.Text = "Rate limits are currently unavailable.";
                    }));
                }
            });
        }

        public void SetRateLimits(RateLimits limits) 
        {
            TemporaryTextBox.SelectAll();

            TemporaryTextBox.Selection.Text = limits.ToString();
        }

        private void Window_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            e.Cancel = true;

            Visibility = Visibility.Hidden;

            mainWindow.ShowRatesButton.IsEnabled = true;
        }
    }
}
