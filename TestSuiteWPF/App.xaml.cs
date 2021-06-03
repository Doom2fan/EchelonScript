using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using System.Windows;

namespace TestSuiteWPF {
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application {
        [DllImport ("kernel32.dll")]
        public static extern bool AllocConsole ();

        [DllImport ("kernel32.dll")]
        public static extern bool FreeConsole ();

        protected override void OnStartup (StartupEventArgs e) {
            base.OnStartup (e);

            AllocConsole ();
        }

        protected override void OnExit (ExitEventArgs e) {
            base.OnExit (e);

            FreeConsole ();
        }
    }
}
