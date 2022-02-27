using System.Runtime.InteropServices;
using System.Windows;

namespace TestSuiteWPF;

/// <summary>
/// Interaction logic for App.xaml
/// </summary>
public partial class App : Application {
    [DllImport ("kernel32.dll")]
    internal static extern bool AllocConsole ();

    [DllImport ("kernel32.dll")]
    internal static extern bool FreeConsole ();

    protected override void OnStartup (StartupEventArgs e) {
        base.OnStartup (e);

        AllocConsole ();
    }

    protected override void OnExit (ExitEventArgs e) {
        base.OnExit (e);

        FreeConsole ();
    }
}
