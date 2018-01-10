using System;
using System.Windows;
using System.Windows.Documents;
using System.Windows.Input;

using Newtonsoft.Json.Linq;

namespace Twitter_Bio_Scrapper
{
    public partial class MainWindow : Window
    {
        public RateLimitsWindow rlWin;

        private DataFetcher dataFetcher;

        public MainWindow()
        {
            InitializeComponent();

            this.ResizeMode = System.Windows.ResizeMode.NoResize;

            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(CurrentDomain_UnhandledException);

            Log.Init();

            rlWin = new RateLimitsWindow(this);

            dataFetcher = new DataFetcher(this);

            try
            {
                Globals.BearerToken = dataFetcher.api.GetBearerToken();
            }
            catch (ServiceNotAvailableException snae) 
            {
                MessageBox.Show("Twitter servers are down.\nTry later.", "Twitter servers are down");

                Close();
            }

            rlWin.GetRates();
       }

        void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            //if (Log.IsNull()) 
            //{
             //   return;
            //}

            Exception ex = (Exception) e.ExceptionObject;

            Log.WriteLine("Uncaught exception start.");
            Log.WriteLine("Message: " + ex.Message);
            Log.WriteLine("Runtime terminating: " + e.IsTerminating);
            Log.WriteLine("Sender is : " + sender.ToString());
            Log.WriteLine("Uncaught exception end.");
        }

        private void Go()
        {
            string input = InputTextBox.Text;

            if (input == "")
            {
                MessageBox.Show("Please enter a scren name for a twitter user", "Warning");

                return;
            }

            if (input.Contains(" "))
            {
                MessageBox.Show("Spaces are not allowed in twitter screen names.", "Warning");

                InputTextBox.Text = InputTextBox.Text.Replace(" ", "");

                return;
            }

            AppStatusLabel.Content = "Acquiring twitter data";

            string[] user_ids = input.Split(',');

            GoButton.IsEnabled = false;
 
            dataFetcher.Fetch(user_ids);
        }

        private void GoButton_Click(object sender, RoutedEventArgs e)
        {
            Go();
        }

        private void ShowRatesButton_Click(object sender, RoutedEventArgs e)
        {
            rlWin.Show();

            ShowRatesButton.IsEnabled = false;
        }

        private void Window_Closed(object sender, System.EventArgs e)
        {
            rlWin.Close();

            try
            {
                Log.Close();

                dataFetcher.outputFileStream.Close();
            }
            catch (System.Exception ex) 
            {
            
            }

            App.Current.Shutdown(0);
        }

        private void InputTextBox_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                Go();
            }
        }

        private void Window_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (!GoButton.IsEnabled)
            {
                if (MessageBox.Show("There is a scrap in progress!\nDo you really want to close?",
                                    "Are you sure",
                                    MessageBoxButton.YesNo,
                                    MessageBoxImage.Question) == MessageBoxResult.No)
                {
                    e.Cancel = true;
                }
            }
        }
    }
}
