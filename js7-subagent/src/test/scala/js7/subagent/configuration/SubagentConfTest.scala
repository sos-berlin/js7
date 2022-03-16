package js7.subagent.configuration

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths
import js7.base.configutils.Configs._
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

final class SubagentConfTest extends AnyFreeSpec
{
  private lazy val logger = Logger[this.type]

  private lazy val subagentConf = SubagentConf.of(
    configDirectory = Paths.get("/tmp/CONFIG"),
    dataDirectory = Paths.get("/tmp/DATA"),
    logDirectory = Paths.get("/tmp/LOGS"),
    jobWorkingDirectory = Paths.get("/tmp/WORKING"),
    webServerPorts = Nil,
    killScript = None,
    config"""js7.windows.codepages.88888 = "UNKNOWN" """)

  "windowsCodepageToEncoding" in {
    assert(subagentConf.windowsCodepageToEncoding(99999) == Left(Problem(
      "Unknown Windows code page 99999")))

    assert(subagentConf.windowsCodepageToEncoding(88888) == Left(Problem(
      "Unknown encoding for Windows code page 88888: java.nio.charset.UnsupportedCharsetException: UNKNOWN")))

    assert(subagentConf.windowsCodepageToEncoding(65001) == Right(UTF_8))
    assert(subagentConf.windowsCodepageToEncoding(1252).map(_.name) == Right("windows-1252"))
  }

  "js7.windows.codepages are known" in {
    for ((cp, nameObj) <- SubagentConf.defaultConfig.getObject("js7.windows.codepages").asScala) {
      try {
        val name = nameObj.unwrapped.asInstanceOf[String]
        val encoding = Charset.forName(name)
        logger.info(s"Windows code page $cp -> $encoding${(encoding.name != name) ?? s" ($name) ❗️"}")
      }
      catch {
        case t: IllegalArgumentException => fail(s"Windows code page $cp: $t")
      }
    }
  }
}
