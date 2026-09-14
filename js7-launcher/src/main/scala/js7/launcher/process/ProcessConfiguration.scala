package js7.launcher.process

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import js7.base.time.ScalaTime.*
import js7.launcher.forwindows.WindowsLogon
import scala.concurrent.duration.FiniteDuration

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  workingDirectory: Option[Path] = None,
  encoding: Charset,
  waitForStdouterrAfterSigkill: FiniteDuration,
  worryAboutStdoutAfterTermination: FiniteDuration,
  additionalEnvironment: Map[String, Option[String]] = Map.empty,
  windowsLogon: Option[WindowsLogon] = None)


object ProcessConfiguration:

  def forTest: ProcessConfiguration = ProcessConfiguration(
    encoding = UTF_8/*Windows ???*/,
    waitForStdouterrAfterSigkill = 500.ms,
    worryAboutStdoutAfterTermination = 100.ms)
