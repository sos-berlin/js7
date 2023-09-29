package js7.launcher.forwindows

import com.sun.jna.platform.win32.Kernel32Util.formatMessageFromLastErrorCode
import com.sun.jna.platform.win32.WinBase.{WAIT_FAILED, WAIT_OBJECT_0}
import com.sun.jna.platform.win32.WinDef.DWORD
import com.sun.jna.platform.win32.WinError.WAIT_TIMEOUT
import com.sun.jna.platform.win32.WinNT.{HANDLE, HANDLEByReference}
import com.sun.jna.platform.win32.{Advapi32, Kernel32}
import com.sun.jna.ptr.{IntByReference, PointerByReference}
import com.sun.jna.win32.StdCallLibrary
import com.sun.jna.win32.W32APIOptions.UNICODE_OPTIONS
import com.sun.jna.{Native, Pointer}
import java.nio.file.{Path, Paths}
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import scala.annotation.tailrec
import scala.concurrent.blocking

private object WindowsApi:
  private val logger = Logger[this.type]
  val MAX_PATH = 260

  lazy val kernel32 = Native.load("kernel32", classOf[Kernel32], UNICODE_OPTIONS)
  lazy val advapi32 = Native.load("advapi32", classOf[Advapi32], UNICODE_OPTIONS)
  lazy val myUserenv = Native.load("userenv", classOf[MyUserenv], UNICODE_OPTIONS)
  lazy val myKernel32 = Native.load("kernel32", classOf[MyKernel32], UNICODE_OPTIONS)
  lazy val myAdvapi32 = Native.load("advapi32", classOf[MyAdvapi32], UNICODE_OPTIONS)

  def waitForSingleObject(handle: HANDLE, timeout: Int): Boolean =
    if timeout == 0 then
      waitForSingleObject_(handle, 0)
    else
      blocking:
        waitForSingleObject_(handle, timeout)

  def dontWaitForSingleObject(handle: HANDLE): Boolean =
    waitForSingleObject_(handle, 0)

  private def waitForSingleObject_(handle: HANDLE, timeout: Int): Boolean =
    requireWindows("WaitForSingleObject")
    kernel32.WaitForSingleObject(handle, timeout) match
      case WAIT_OBJECT_0 => true
      case WAIT_TIMEOUT => false
      case WAIT_FAILED => throwLastError("WaitForSingleObject")
      case o => throw new WindowsException(s"WaitForSingleObject returned $o")

  def processHandleCount: Int =
    val ref = new IntByReference
    call("GetProcessHandleCount"):
      myKernel32.GetProcessHandleCount(kernel32.GetCurrentProcess(), ref)
    ref.getValue

  def tempPath: Path =
    requireWindows("GetTempPath")
    val a = new Array[Char](MAX_PATH + 1)
    val length = kernel32.GetTempPath(new DWORD(a.length), a).intValue
    if length <= 0 || length > a.length then throw new WindowsException("GetTempPath failed")
    Paths.get(new String(a, 0, length))

  /**
    * Returns something like """C:\Windows""".
    */
  def windowsDirectory: Path =
    val a = new Array[Char](MAX_PATH + 1)
    val length = myKernel32.GetSystemWindowsDirectory(a, a.length)
    if length <= 0 || length > a.length then throw new WindowsException("GetSystemWindowsDirectory failed")
    Paths.get(new String(a, 0, length))

  def openProcessToken(process: HANDLE, desiredAccess: Int): HANDLE =
    handleCall("OpenProcessToken")(myAdvapi32.OpenProcessToken(process, desiredAccess, _))

  /**
    * Calls `CreateEnvironmentBlock`, returns the environment defined for the given user.
    */
  def usersEnvironment(userToken: HANDLE): Map[String, String] =
    val strings =
      val handle =
        val ref = new PointerByReference
        call("CreateEnvironmentBlock"):
          myUserenv.CreateEnvironmentBlock(ref, userToken, false)
        ref.getValue
      try
        val builder = Vector.newBuilder[String]

        @tailrec def continue(offset: Int): Unit =
          handle.getWideString(offset) match
            case "" =>
            case string =>
              builder += string
              continue(offset + 2 * (string.length + 1))

        continue(0)
        builder.result()
      finally
        call("DestroyEnvironmentBlock"):
          myUserenv.DestroyEnvironmentBlock(handle)

    (strings map { o =>
      o indexOf '=' match {
        case -1 => o -> ""
        case i => o.substring(0, i) -> o.substring(i + 1)
      }
    })
      .toMap

  //def stringToMemory(string: String): Memory = {
  //  val m = new Memory(2 * (string.length + 1))
  //  m.setWideString(0, string)
  //  m
  //}

  def handleCall(name: String)(apiFunction: HANDLEByReference => Boolean): HANDLE =
    val ref = new HANDLEByReference
    call(name)(apiFunction(ref))
    ref.getValue

  def call(functionName: String, args: String*)(apiFunction: => Boolean): Unit =
    requireWindows(functionName)
    logger.trace(s"Calling Windows API: $functionName ${args.mkString(", ")}")
    val ok = apiFunction
    if !ok then
      throwLastError(functionName)

  def throwLastError(function: String): Nothing =
    throw new WindowsException(getLastErrorAsProblem(function).toString)

  def getLastErrorAsProblem(function: String): Problem =
    requireWindows("GetLastError")
    val err = kernel32.GetLastError
    Problem(f"WINDOWS-${messageIdToString(err)} ($function) ${formatMessageFromLastErrorCode(err)}")

  def messageIdToString(id: Int) =
    if id <= 0xc0ffffff /*negative*/  then f"0x$id%08x" else id.toString

  private def requireWindows(functionName: String) =
    if !isWindows then sys.error(s"Windows API '$functionName' is only available under Microsoft Windows")

  final class WindowsException(message: String) extends RuntimeException(message)

  trait MyKernel32 extends StdCallLibrary:
    def GetProcessHandleCount(hProcess: HANDLE, handleCount: IntByReference): Boolean
    def GetSystemWindowsDirectory(buffer: Array[Char], nBufferLength: Int): Int

  trait MyAdvapi32 extends StdCallLibrary:
    def CredRead(targentName: String, typ: Int, flags: Int, credentialRef: PointerByReference): Boolean
    def CredFree(credentials: Pointer): Unit
    def OpenProcessToken(process: HANDLE, desiredAccess: Int, result: HANDLEByReference): Boolean

  object MyAdvapi32:
    val CRED_TYPE_GENERIC = 1

  trait MyUserenv extends StdCallLibrary:
    def LoadUserProfile(userToken: HANDLE, profileInfo: PROFILEINFO): Boolean
    def UnloadUserProfile(userToken: HANDLE, profileHandle: HANDLE): Boolean
    def CreateEnvironmentBlock(environment: PointerByReference, userToken: HANDLE, inherit: Boolean): Boolean
    def DestroyEnvironmentBlock(environment: Pointer): Boolean
