package js7.launcher.forwindows

import com.sun.jna.platform.win32.Kernel32Util.closeHandle
import com.sun.jna.platform.win32.WinNT.TOKEN_ALL_ACCESS
import java.nio.file.Paths
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.launcher.forwindows.WindowsApi.{kernel32, messageIdToString, openProcessToken}

final class WindowsApiTest extends OurTestSuite
{
  if (isWindows) {
    "processHandleCount" in {
      assert(WindowsApi.processHandleCount > 0)
    }

    "tempPath" in {
      assert(WindowsApi.tempPath == Paths.get(sys.env("TMP")))
    }

    "windowsDirectory" in {
      assert(WindowsApi.windowsDirectory == Paths.get("C:\\Windows"))  // Expected for test environment
    }

    "openProcessToken" in {
      val handle = WindowsApi.openProcessToken(kernel32.GetCurrentProcess, TOKEN_ALL_ACCESS)
      closeHandle(handle)
    }

    "usersEnvironment" in {
      val userToken = openProcessToken(kernel32.GetCurrentProcess, TOKEN_ALL_ACCESS)
      assert(WindowsApi.usersEnvironment(userToken) contains "Path")
      closeHandle(userToken)
    }
  }

  "messageIdToString" in {
    assert(messageIdToString(0) == "0")
    assert(messageIdToString(1) == "1")
    assert(messageIdToString(1000) == "1000")
    assert(messageIdToString(-1) == "-1")
    assert(messageIdToString(-1000) == "-1000")
    assert(messageIdToString(0x7fffffff) == "2147483647")
    assert(messageIdToString(0x80000000) == "0x80000000")
    assert(messageIdToString(0x80000001) == "0x80000001")
    assert(messageIdToString(0xc0ffffff) == "0xc0ffffff")
  }
}
