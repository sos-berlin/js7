package js7.launcher.forwindows.configuration

import com.sun.jna.platform.win32.Kernel32.INSTANCE.GetConsoleCP
import js7.base.problem.Checked
import js7.launcher.forwindows.WindowsApi

object WindowsConf
{
  val codepage: Checked[Int] =
    GetConsoleCP() match {
      case 0 => Left(WindowsApi.getLastErrorAsProblem("GetConsoleCP"))
      case n => Right(n)
    }
}
