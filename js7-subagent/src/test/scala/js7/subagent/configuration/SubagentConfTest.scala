package js7.subagent.configuration

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.collection.View
import scala.jdk.CollectionConverters.*

final class SubagentConfTest extends OurTestSuite
{
  private lazy val logger = Logger[this.type]

  private lazy val subagentConf = SubagentConf.of(
    configDirectory = Paths.get("/tmp/CONFIG"),
    dataDirectory = Paths.get("/tmp/DATA"),
    logDirectory = Paths.get("/tmp/LOGS"),
    jobWorkingDirectory = Paths.get(if (isWindows) """c:\tmp\WORKING""" else "/tmp/WORKING"),
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
    val cpToEncoding = SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
      .asScala
      .to(View)
      .map { case (cp, v) => cp.toInt -> v.unwrapped.asInstanceOf[String] }
      .toVector
      .sortBy(_._1)
    for ((cp, name) <- cpToEncoding) {
      try {
        val encoding = Charset.forName(name)
        val hasCpName = encoding.name == s"cp$cp" || encoding.aliases.contains(s"cp$cp")
        logger.info(s"Windows code page $cp -> $encoding " +
          encoding.aliases.asScala.mkString(", ") + " " +
          ((encoding.name != name) ?? s" ($name) ❗️") +
          (hasCpName ?? " (derived too)"))
      } catch {
        case t: IllegalArgumentException => fail(s"Windows code page $cp: $t")
      }
    }
  }

  "Configured codepages that can also be derived" in {
    for ((cp, nameObj) <- SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
      .asScala.toVector.sortBy(_._1.toInt)) {
      try {
        val name = nameObj.unwrapped.asInstanceOf[String]
        val encoding = Charset.forName(name)
        val cpName = s"cp$cp"
        if (encoding.name == cpName || encoding.aliases.asScala(cpName))  {
          logger.info(s"Superfluous configured Windows code page $cp -> $encoding " +
            encoding.aliases.asScala.mkString(", "))
        }
      } catch {
        case t: IllegalArgumentException => fail(s"Windows code page $cp: $t")
      }
    }
  }

  "Known codepages" in {
    // Slow due to many Charset.forName
    val configuredCodepages  = SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
      .asScala.keySet
    // Parallelize for shorter test duration (4s instead of 17s)
    val cpToEnc = Observable.fromIterable(1 to 32767)
      .bufferTumbling(512)
      .mapParallelOrdered(sys.runtime.availableProcessors)(chunk => Task(
        chunk.map(cp => cp -> subagentConf.windowsCodepageToEncoding(cp))))
      .flatMap(Observable.fromIterable)
      .collect { case (codepage, Right(encoding)) => codepage -> encoding }
      .toListL
      .await(99.s)
    for ((codepage, encoding) <- cpToEnc) {
      logger.info(s"Known Windows code page $codepage -> $encoding " +
        encoding.aliases.asScala.toVector.sorted.mkString(", ") +
        ((configuredCodepages(codepage.toString) ?? " (configured)")))
    }
  }

  "Encodings without codepage (informative)" in {
    val supportedEncodings = SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
      .asScala.values.view.map(_.unwrapped.asInstanceOf[String]).toSet
    val javaEncodings = Charset.availableCharsets().asScala.values.toSet
    val unsupported = javaEncodings
      .filterNot(o => supportedEncodings(o.name))
      .filterNot(o => (o.aliases.asScala.toSet + o.name).exists(o => o.matches("""(cp|CP)\d+""")))
    for (encoding <- unsupported.toVector.sorted) {
      logger.info(s"Java encoding without Windows codepage: ${encoding.name} " +
        s"${encoding.aliases.asScala.toVector.mkString(", ")}")
    }
  }
}
