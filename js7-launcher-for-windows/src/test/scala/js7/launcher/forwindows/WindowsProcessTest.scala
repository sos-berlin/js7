package js7.launcher.forwindows

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.Files.{createTempFile, delete, deleteIfExists}
import java.nio.file.{Files, Path}
import java.util.Locale
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ReturnCode
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.launcher.forwindows.WindowsApi.windowsDirectory
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import js7.launcher.forwindows.WindowsProcessTest.*
import org.scalatest.matchers.should.Matchers.*
import scala.util.{Failure, Try}

final class WindowsProcessTest extends OurTestSuite
{
  if (isWindows) {
    // TODO Test fails
    val commandCharset = Charset forName "cp850"
    for (withUserProfile <- Seq(false, true))
      s"withUserProfile=$withUserProfile" - {
        lazy val logon = sys.props.get(TargetSystemProperty)
          .filter(_.nonEmpty)
          .map(o => WindowsLogon(
            WindowsProcessCredential.byKey(o).orThrow,
            withUserProfile = withUserProfile))

        "CreateProcess" in {
          val batchFile = /*credential match {
            case Some(WindowsProcessCredential(user, _)) =>
              val directory = createTempFile(WindowsApi.windowsDirectory / "Temp", "WindowsProcessTest-", ".cmd")
              WindowsProcess.makeFileExecutableForUser(directory, user)
            case None =>*/
              createTempFile("WindowsProcessTest-", ".cmd")
          //}
          batchFile :=
           s"""rem @echo off
              |set
              |dir
              |echo test-username=self-test
              |echo TEST-STDERR 1>&2
              |echo env-userdomain=%USERDOMAIN%
              |echo env-username=%USERNAME%
              |for /f "usebackq tokens=*" %%a in (`echo self-test`) do echo TEST-USERNAME=%%a
              |for /f "usebackq tokens=*" %%a in (`$windowsDirectory\\system32\\whoami.exe`) do echo TEST-USERNAME=%%a
              |rem $windowsDirectory\\system32\\whoami.exe >$windowsDirectory\\Temp\\WindowsProcessTest.log
              |rem Cygwin bricht das Skript ab: whoami.exe
              |ping -n 3 %1""".stripMargin
          val stdoutFile = createTempFile("test-stdout-", ".log")
          val stderrFile = createTempFile("test-stderr-", ".log")
          val process = WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              Seq(batchFile.toString, "127.0.0.1"),
              stdinRedirect = Redirect.INHERIT,
              stdoutRedirect = Redirect.to(stdoutFile.toFile),
              stderrRedirect = Redirect.to(stderrFile.toFile)),
            logon
          ).orThrow
          assert(process.isAlive && process.returnCode.isEmpty)
          process.waitFor(10.ms) shouldBe false
          val processResult = process.waitFor(5.s)
          assert(processResult)
          assert(!process.isAlive)
          assert(process.returnCode == Some(ReturnCode(0)))
          assert(Files.size(stdoutFile) > 0)
          val stdout = stdoutFile.contentString(commandCharset)
          println(stdout)
          assert(stdout contains "TEST-STDERR")
          assert(stdout contains "127.0.0.1")
          val stdoutLines = stdout.split("\n").map(_.trim.toLowerCase(Locale.ROOT))
          assert(stdoutLines contains s"env-userdomain=${sys.env("USERDOMAIN").toLowerCase(Locale.ROOT)}")
          val userNameLines = stdoutLines filter { _ contains "test-username=" }
          assert(userNameLines exists { _ endsWith "self-test" })
          val me = sys.env("USERNAME").toLowerCase(Locale.ROOT)
          logon match {
            case Some(WindowsLogon(WindowsProcessCredential(WindowsUserName(user), _), _)) =>
              assert(stdout.toLowerCase(Locale.ROOT) contains s"env-username=$user".toLowerCase(Locale.ROOT))
              assert(userNameLines.forall(o => !o.endsWith("\\" ++ me)))
              // whoami outputs nothing, but quits whole command ???
              //assert(userNameLines forall { _ endsWith ("\\" + user) })  // whoami outputs domain backslash username
            case None =>
              // "USERNAME=Lokaler Dienst"? assert(stdout contains s"env-username=$me")
              assert(userNameLines.exists(_.endsWith("\\" ++ me)))
          }
          delete(stdoutFile)
          delete(stderrFile)
          delete(batchFile)
        }

        "TerminateProcess" in {
          val process = WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              Seq(s"$windowsDirectory\\system32\\ping.exe", "-n", "10", "127.0.0.1"),
              Redirect.INHERIT,
              Redirect.INHERIT,
              Redirect.INHERIT),
            logon
          ).orThrow
          process.destroy()
          process.waitFor(1.s)
          assert(!process.isAlive)
          assert(process.returnCode == Some(WindowsProcess.TerminateProcessReturnCode))
        }

        "Inherit stdout (manual test)" in {
          val process = WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              Seq(s"$windowsDirectory\\system32\\ping.exe", "-n", "1", "127.0.0.1"),
              Redirect.PIPE,
              Redirect.INHERIT,  // Please check stdout yourself !!!
              Redirect.PIPE),
            logon
          ).orThrow
          process.waitFor(5.s) shouldBe true
          assert(process.returnCode == Some(ReturnCode(0)))
        }

        "Environment" in {
          val testVariableName = "TEST_VARIABLE"
          val testVariableValue = "TEST-VALUE"
          val scriptFile = createTempFile("test-", ".cmd")
          scriptFile.contentString = s"""
            |@echo off
            |echo $testVariableName=%$testVariableName%
            |exit 0
            |""".stripMargin
          val stdoutFile = createTempFile("test-", ".log")
          val process = WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              Seq(s"$windowsDirectory\\system32\\cmd.exe", "/C", scriptFile.toString),
              Redirect.PIPE,
              Redirect.to(stdoutFile.toFile),
              Redirect.PIPE,
              Map(testVariableName -> testVariableValue)),
            logon
          ).orThrow
          process.waitFor(5.s) shouldBe true
          assert(process.returnCode == Some(ReturnCode(0)))

          waitForCondition(10.s, 10.ms)(Try(stdoutFile.byteArray).isSuccess)  // For Windows
          assert(stdoutFile.contentString(commandCharset) contains s"$testVariableName=$testVariableValue")

          waitForCondition(10.s, 10.ms)(Try(delete(stdoutFile)).isSuccess)  // For Windows
          deleteIfExists(stdoutFile)
        }

        "Write to stdin" in {
          val scriptFile = createTempFile("test-", ".cmd")
          scriptFile.contentString = """
            |@echo off
            |set /p input=
            |echo input=%input%
            |""".stripMargin
          val stdoutFile = createTempFile("test-", ".log")
          val testString = "HELLO, THIS IS A TEST\r\n"
          val testBytes = testString.getBytes(US_ASCII)
          val process = WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              Seq(scriptFile.toString),
              Redirect.PIPE,
              Redirect.to(stdoutFile.toFile),
              Redirect.PIPE),
            logon
          ).orThrow
          process.stdin.write(testBytes, 0, testBytes.length)
          process.stdin.flush()
          process.waitFor(5.s) shouldBe true
          stdoutFile.contentString(commandCharset) shouldEqual s"input=$testString"
          waitForCondition(10.s, 10.ms) { Try(delete(stdoutFile)).isSuccess }
          deleteIfExists(stdoutFile)
          delete(scriptFile)
        }

        for (user <- logon.map(_.userName)) {
          "makeFileExecutableForUser" in {
            check("(RX)")(WindowsProcess.makeFileExecutableForUser(_, user))
          }

          "makeFileAppendableForUser" in {
            check("(M)")(WindowsProcess.makeFileAppendableForUser(_, user))
          }

          def check(expected: String)(body: Path => Unit): Unit = {
            val file = createTempFile("test-", ".tmp")
            body(file)
            val icaclsOut = icacls(file)
            println(icaclsOut)
            assert(icaclsOut.replace("\r\n", "\n").toLowerCase(Locale.ROOT) contains s"$user:$expected\n".toLowerCase(Locale.ROOT))
            delete(file)
          }

          "makeFileAppendableForUser, script appends" in {
            val appendableFile = createTempFile("test-", ".tmp")
            appendableFile.contentString = ""
            WindowsProcess.makeFileAppendableForUser(appendableFile, user)
            val stdoutFile = createTempFile("test-", ".tmp")
            println(icacls(appendableFile))
            val scriptFile = createTempFile("test-", ".cmd")
            scriptFile.contentString = s"echo TEST>>$appendableFile\n"
            WindowsProcess.startWithWindowsLogon(
              StartWindowsProcess(
                Seq(scriptFile.toString),
                Redirect.PIPE,
                Redirect.to(stdoutFile.toFile),
                Redirect.PIPE),
              logon
            ).orThrow.waitFor()
            println(stdoutFile.contentString(commandCharset))
            delete(stdoutFile)
            delete(scriptFile)
            assert(appendableFile.contentString == "TEST\r\n")
            delete(appendableFile)
          }
        }

        if (false) {  // Own user: stdout is empty. Other user: DLL initialization error, user32.dll
          "Java via script" in {
            val scriptFile = createTempFile("test-", ".cmd")
            scriptFile.contentString = """
              |@echo off
              |java -version
              |""".stripMargin
            val stdoutFile = createTempFile("test-", ".log")
            val process = WindowsProcess.startWithWindowsLogon(
              StartWindowsProcess(
                Seq(scriptFile.toString),
                Redirect.PIPE,
                Redirect.to(stdoutFile.toFile),
                Redirect.PIPE),
              logon
            ).orThrow
            process.waitFor()
            println(f"exitValue=${process.returnCode.get.number}%08x")
            assert(process.returnCode == Some(ReturnCode(0)))
            assert(stdoutFile.contentString(commandCharset) contains "java version 1.8.0_131")
            delete(stdoutFile)
            delete(scriptFile)
          }

          "Java direct" in {
            val stdoutFile = createTempFile("test-", ".log")
            val stderrFile = createTempFile("test-", ".log")
            val process = WindowsProcess.startWithWindowsLogon(
              StartWindowsProcess(
                Seq("""C:\Program Files\Java\jdk1.8.0_131\bin\java.exe""", "-version"),
                Redirect.PIPE,
                Redirect.to(stdoutFile.toFile),
                Redirect.PIPE),
              logon.map(_.copy(withUserProfile = true))
            ).orThrow
            process.waitFor()
            println(stdoutFile.contentString(commandCharset))
            println(stderrFile.contentString(commandCharset))
            println(f"exitValue=${process.returnCode.get.number}%08x")
            assert(process.returnCode == Some(ReturnCode(0)))
            assert(stdoutFile.contentString(commandCharset) contains "java version 1.8.0_131")
            delete(stdoutFile)
            delete(stderrFile)
          }
        }
      }

      def icacls(file: Path): String = {
        val stdoutFile = createTempFile("test-", ".log")
        new ProcessBuilder(s"$windowsDirectory\\system32\\icacls.exe", file.toString)
          .redirectOutput(Redirect.to(stdoutFile.toFile))
          .start()
          .waitFor()
        try stdoutFile.contentString(commandCharset)
        finally delete(stdoutFile)
      }

      "execute" in {
        val file = "NON-EXISTANT-FILE-WITH-ÜMLÅÙTS"
        val user = "NON-EXISTENT-ÜSËR"
        val Failure(t) = Try {
          WindowsProcess.execute(windowsDirectory / "System32\\icacls.exe", file, "/q", "/grant", s"$user:R")
        }
        val Array(a, b) = t.getMessage split "=>"
        println(t.getMessage)
        assert(a contains "icacls.exe")
        assert(b contains user)
      }
  }

  "injectableUserName" in {
    check("a")
    check("å")
    check("a.b")
    check(".a_b")
    check("_a.b")
    check("-a _ . , b")
    check("a b")
    check("user@domain")
    check("漢字")
    check("片仮名")
    def check(name: String) = assert(WindowsProcess.injectableUserName(WindowsUserName(name)) == name)
    intercept[IllegalArgumentException] { WindowsProcess.injectableUserName(WindowsUserName("")) }
    intercept[IllegalArgumentException] { WindowsProcess.injectableUserName(WindowsUserName("a%")) }
  }
}

object WindowsProcessTest {
  val TargetSystemProperty = "js7.WindowsProcess.target"
}
