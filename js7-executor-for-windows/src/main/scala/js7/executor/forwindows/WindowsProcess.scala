package js7.executor.forwindows

import cats.syntax.traverse._
import com.sun.jna.platform.win32.Advapi32Util.getEnvironmentBlock
import com.sun.jna.platform.win32.Kernel32Util.closeHandle
import com.sun.jna.platform.win32.WinBase._
import com.sun.jna.platform.win32.WinError.ERROR_ACCESS_DENIED
import com.sun.jna.platform.win32.WinNT._
import com.sun.jna.platform.win32.Wincon.{STD_ERROR_HANDLE, STD_INPUT_HANDLE, STD_OUTPUT_HANDLE}
import com.sun.jna.ptr.IntByReference
import com.sun.jna.{Structure, WString}
import java.io.{InputStream, OutputStream}
import java.lang.Math.{max, min}
import java.lang.ProcessBuilder.Redirect
import java.lang.ProcessBuilder.Redirect.Type.{INHERIT, PIPE, WRITE}
import java.nio.charset.Charset
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.Pid
import js7.base.io.process.{Js7Process, KeyLogin, ReturnCode}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.SetOnce
import js7.executor.forwindows.WindowsApi.{advapi32, call, handleCall, kernel32, myUserenv, openProcessToken, waitForSingleObject, windowsDirectory}
import js7.executor.forwindows.WindowsProcess._
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration
import scala.io.Codec
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

/**
  * A Windows process, started with CreateProcessW via JNA.
  * `waitFor` must be called to release the process resource and the Windows handle.
  */
private final class WindowsProcess private(
  processInformation: PROCESS_INFORMATION,
  inRedirection: Redirection,
  outRedirection: Redirection,
  errRedirection: Redirection,
  loggedOn: LoggedOn)
extends Js7Process
{
  val pid = Some(Pid(processInformation.dwProcessId.intValue))
  private val returnCodeOnce = SetOnce[ReturnCode]

  private val handleGuard = new ResourceGuard(processInformation.hProcess) {
    def release(hProcess: HANDLE) = {
      closeHandle(hProcess)
      inRedirection.closePipe()
      outRedirection.closePipe()
      errRedirection.closePipe()
      loggedOn.close()
    }
  }

  def isAlive =
    returnCodeOnce.isEmpty && !waitForProcess(timeout = 0)

  lazy val stdin: OutputStream = {
    if (inRedirection.pipeHandle == INVALID_HANDLE_VALUE)
      throw new IllegalStateException("WindowsProcess has no handle for stdin attached")
    new PipeOutputStream(inRedirection.pipeHandle) {
      override def close() = inRedirection.closePipe()
    }
  }

  lazy val stdout: InputStream = {
    if (outRedirection.pipeHandle == INVALID_HANDLE_VALUE)
      throw new IllegalStateException("WindowsProcess has no handle for stdout attached")
    new PipeInputStream(outRedirection.pipeHandle) {
      override def close() = inRedirection.closePipe()
    }
  }

  lazy val stderr: InputStream = {
    if (errRedirection.pipeHandle == INVALID_HANDLE_VALUE)
      throw new IllegalStateException("WindowsProcess has no handle for stderr attached")
    new PipeInputStream(errRedirection.pipeHandle) {
      override def close() = inRedirection.closePipe()
    }
  }

  def destroy() = destroyForcibly()

  def destroyForcibly() =
    handleGuard {
      case None =>
      case Some(hProcess) =>
        call("TerminateProcess") {
          kernel32.TerminateProcess(hProcess, TerminateProcessReturnCode.number) ||
            kernel32.GetLastError == ERROR_ACCESS_DENIED && {
              Try(waitForProcess(0)).getOrElse(false) || {
                kernel32.SetLastError(ERROR_ACCESS_DENIED)
                false
              }
            }
        }
    }

  @TestOnly
  private[forwindows] def waitFor(timeout: FiniteDuration): Boolean =
    returnCodeOnce.isDefined ||
      waitForProcess(max(0, min(Int.MaxValue, timeout.toMillis)).toInt)

  def waitFor(): ReturnCode =
    returnCodeOnce.getOrElse {
      waitForProcess(INFINITE)
      returnCodeOnce.orThrow
    }

  def returnCode =
    returnCodeOnce.toOption

  private def waitForProcess(timeout: Int): Boolean =
    handleGuard {
      case None =>
        if (returnCodeOnce.isEmpty)
          throw new IllegalStateException("WindowsProcess has been closed before started")
        true

      case Some(hProcess) =>
        val terminated = waitForSingleObject(hProcess, timeout)
        if (terminated) {
          returnCodeOnce.trySet(ReturnCode(getExitCodeProcess(hProcess)))
          handleGuard.releaseAfterUse()
        }
        terminated
    }

  override def toString = s"WindowsProcess($pid)"
}

object WindowsProcess
{
  private[forwindows] val TerminateProcessReturnCode = ReturnCode(999_999_999)
  val TargetSystemProperty = "js7.WindowsProcess.target"
  private val AuthenticatedUsersSid = "S-1-5-11"
  private val logger = Logger(getClass)

  final case class StartWindowsProcess(
    args: Seq[String],
    stdinRedirect: Redirect,
    stdoutRedirect: Redirect,
    stderrRedirect: Redirect,
    additionalEnv: Map[String, String] = Map.empty)

  def startWithKeyLogin(
    startWindowsProcess: StartWindowsProcess,
    keyLogin: Option[KeyLogin])
  : Checked[Js7Process] =
    keyLogin
      .traverse(WindowsLogon.fromKeyLogin)
      .flatMap(
        startWithWindowsLogon(startWindowsProcess, _))

  private[forwindows] def startWithWindowsLogon(
    startWindowsProcess: StartWindowsProcess,
    maybeLogon: Option[WindowsLogon] = None)
  : Checked[WindowsProcess] = {
    import startWindowsProcess.{additionalEnv, args, stderrRedirect, stdinRedirect, stdoutRedirect}

    for (commandLine <- argsToCommandLine(args.toIndexedSeq)) yield {
      val inRedirection = redirectToHandle(STD_INPUT_HANDLE, stdinRedirect)
      val outRedirection = redirectToHandle(STD_OUTPUT_HANDLE, stdoutRedirect)
      val errRedirection = redirectToHandle(STD_ERROR_HANDLE, stderrRedirect)
      val startupInfo = new STARTUPINFO
      startupInfo.dwFlags |= STARTF_USESTDHANDLES
      startupInfo.hStdInput = inRedirection.startupInfoHandle
      startupInfo.hStdOutput = outRedirection.startupInfoHandle
      startupInfo.hStdError = errRedirection.startupInfoHandle

      val application = args.head
      val creationFlags = CREATE_UNICODE_ENVIRONMENT

      val loggedOn = LoggedOn.logon(maybeLogon)
      val env = maybeLogon.fold(sys.env)(logon =>
        if (logon.withUserProfile)
          WindowsApi.usersEnvironment(loggedOn.userToken)  // Only reliable if user profile has been loaded (see JS-1725)
        else
          WindowsApi.usersEnvironment(null) ++  // Default system environment
            Some("USERNAME" -> logon.user.withoutDomain) ++ // Default system environment contains default USERNAME and USERDOMAIN. We change this..
            logon.user.domain
              .orElse(sys.env.get("USERDOMAIN"))
              .map("USERDOMAIN" -> _))
      val directory = windowsDirectory.getRoot.toString  // Need a readable directory, ignoring a given working directory
      val processInformation = new PROCESS_INFORMATION
      call("CreateProcessAsUser", application, commandLine, s"directory=$directory") {
        advapi32.CreateProcessAsUser(loggedOn.userToken, application, commandLine,
          null: SECURITY_ATTRIBUTES, null: SECURITY_ATTRIBUTES, /*inheritHandles=*/true, creationFlags,
          getEnvironmentBlock((env ++ additionalEnv).asJava),
          directory, startupInfo, processInformation)
      }
      inRedirection.releaseStartupInfoHandle()
      outRedirection.releaseStartupInfoHandle()
      errRedirection.releaseStartupInfoHandle()
      closeHandle(processInformation.hThread)
      processInformation.hThread = INVALID_HANDLE_VALUE
      new WindowsProcess(processInformation, inRedirection, outRedirection, errRedirection, loggedOn)
    }
  }

  private class LoggedOn(val userToken: HANDLE, val profileHandle: HANDLE = INVALID_HANDLE_VALUE)
  extends AutoCloseable {
    private val closed = new AtomicBoolean

    def close() = {
      if (!closed.getAndSet(true)) {
        if (profileHandle != INVALID_HANDLE_VALUE) {
          call("UnloadUserProfile") {
            myUserenv.UnloadUserProfile(userToken, profileHandle)
          }
        }
        closeHandle(userToken)
      }
    }
  }

  private object LoggedOn {
    def logon(logonOption: Option[WindowsLogon]): LoggedOn =
      logonOption match {
        case Some(o) => logon(o)
        case None => new LoggedOn(openProcessToken(kernel32.GetCurrentProcess, TOKEN_ALL_ACCESS))
      }

    private def logon(logon: WindowsLogon): LoggedOn = {
      import logon.{credentials, user, withUserProfile}
      logger.debug(s"LogonUser '$user'")
      val userToken = handleCall("LogonUser")(
        advapi32.LogonUser(user.string, null, credentials.password.string, LOGON32_LOGON_BATCH, LOGON32_PROVIDER_DEFAULT, _))
      new LoggedOn(
        userToken,
        profileHandle =
          if (withUserProfile)
            loadUserProfile(userToken, user)
          else
            INVALID_HANDLE_VALUE)
    }
  }

  private def loadUserProfile(userToken: HANDLE, user: WindowsUserName): HANDLE = {
    val profileInfo = Structure.newInstance(classOf[PROFILEINFO])
    profileInfo.dwSize = profileInfo.size
    profileInfo.userName = new WString(user.string)
    profileInfo.write()
    call("LoadUserProfile") {
      myUserenv.LoadUserProfile(userToken, profileInfo)
    }
    profileInfo.read()
    profileInfo.hProfile
  }

  private def redirectToHandle(stdFile: Int, redirect: Redirect): Redirection =
    redirect.`type` match {
      case INHERIT =>
        new Redirection(kernel32.GetStdHandle(stdFile), false, INVALID_HANDLE_VALUE)

      case PIPE =>
        stdFile match {
          case STD_INPUT_HANDLE =>
            Redirection.newStdinPipeRedirection()

          case STD_OUTPUT_HANDLE | STD_ERROR_HANDLE =>
            Redirection.newStdouterrPipeRedirection()
        }

      case WRITE =>
        Redirection.forDirectFile(redirect.file)

      case t => throw new IllegalArgumentException(s"Unsupported Redirect $t")
  }

  private def getExitCodeProcess(hProcess: HANDLE): Int = {
    val ref = new IntByReference
    call("GetExitCodeProcess") {
      kernel32.GetExitCodeProcess(hProcess, ref)
    }
    logger.trace(s"GetExitCodeProcess => ${ref.getValue}")
    ref.getValue
  }

  /**
    * Adds the needed ACL despite any existing ACL.
    */
  def makeFileExecutableForUser(file: Path, user: WindowsUserName): file.type =
    grantFileAccess(file, s"${injectableUserName(user)}:RX")

  /**
    * Adds the needed ACL despite any existing ACL.
    */
  def makeFileExecutableForEverybody(file: Path): file.type =
    grantFileAccess(file, s"*$AuthenticatedUsersSid:RX")

  /**
    * Adds the needed ACL despite any existing ACL.
    */
  def makeFileAppendableForUser(file: Path, user: WindowsUserName): file.type = {
    grantFileAccess(file, s"${injectableUserName(user)}:M")
  }

  private val AllowedUserNameCharacters = Set('_', '.', '-', ',', ' ', '@')  // Only for icacls syntactically irrelevant characters and domain separators

  private[forwindows] def injectableUserName(user: WindowsUserName): String = {
    val name = user.string
    def isValid(c: Char) = c.isLetterOrDigit || AllowedUserNameCharacters(c)
    require(name.nonEmpty && name.forall(isValid), s"Unsupported character in Windows user name: '$name'")  // Avoid code injection
    name
  }

  def makeDirectoryAccessibleForEverybody(directory: Path): directory.type =
    grantFileAccess(directory, s"*$AuthenticatedUsersSid:(OI)RX")

  /**
    * @param grant syntax is weakly checked!
    */
  private def grantFileAccess(file: Path, grant: String): file.type = {
    execute(windowsDirectory / "System32\\icacls.exe", "\"" + file.toString + '"', "/q", "/grant", grant)
    file
  }

  def execute(executable: Path, args: String*): Vector[String] = {
    logger.debug(executable.toString + args.mkString(" [", ", ", "]"))
    val process = new ProcessBuilder(executable.toString +: args: _*).redirectErrorStream(true).start()
    process.getOutputStream.close()  // stdin
    val lines = {
      val commandCodec = new Codec(Charset.forName("cp850"))  // 850 contains all characters of ISO-8859-1
      try autoClosing(process.getInputStream) { in => scala.io.Source.fromInputStream(in)(commandCodec).getLines().toVector }
      catch { case NonFatal(t) =>
        Vector(s"error message not readable: $t")
      }
    }
    val returnCode = process.waitFor()
    if (returnCode != 0) throw new RuntimeException(s"Windows command failed: $executable => ${lines mkString " / "}")
    lines
  }

  def argsToCommandLine(args: Seq[String]): Checked[String] =
    WindowsCommandLineConversion.argsToCommandLine(args)
}