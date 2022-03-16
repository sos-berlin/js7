package js7.launcher.forwindows.configuration

import com.sun.jna.platform.win32.Kernel32

object WindowsConf
{
  val codepage: Int =
    Kernel32.INSTANCE.GetConsoleCP()
}
