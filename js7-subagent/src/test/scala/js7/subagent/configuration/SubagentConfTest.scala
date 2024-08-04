package js7.subagent.configuration

import java.net.InetSocketAddress
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.createTempDirectory
import java.nio.file.{Path, Paths}
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.{WorkingDirectory, deleteDirectoryRecursively}
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.commandline.CommandLineArguments
import js7.common.pekkohttp.web.data.WebServerPort
import js7.subagent.configuration.SubagentConfTest.*
import scala.collection.View
import scala.jdk.CollectionConverters.*

final class SubagentConfTest extends OurTestSuite:

  private lazy val logger = Logger[this.type]

  "Shortest argument list" in:
    provideConfigAndData { (configDir, dataDir) =>
      val subagentConf = SubagentConf.fromCommandLine(CommandLineArguments(Seq(
        s"--config-directory=$configDir",
        s"--data-directory=$dataDir")))
      subagentConf.finishAndProvideFiles()
      assert(subagentConf == SubagentConf.of(
        configDirectory = configDir,
        dataDirectory = dataDir,
        logDirectory = dataDir / "logs",
        jobWorkingDirectory = WorkingDirectory,
        webServerPorts = Nil,
        name = "js7"))
    }

  "--http-port=" in:
    // For more tests see CommonConfigurationTest
    intercept[IllegalArgumentException] { dummyDirectoriesConf("--http-port=65536") }
    assert(dummyDirectoriesConf("--http-port=1234").webServerPorts == WebServerPort.Http(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)

  "--https-port=" in:
    // For more tests see CommonConfigurationTest
    assert(dummyDirectoriesConf("--https-port=1234").webServerPorts == WebServerPort.Https(new InetSocketAddress("0.0.0.0", 1234)) :: Nil)

  "--log-directory=" in:
    assert(dummyDirectoriesConf().logDirectory == Paths.get("DATA/logs").toAbsolutePath)
    assert(dummyDirectoriesConf("--log-directory=LOGS").logDirectory == Paths.get("LOGS").toAbsolutePath)
    assert(dummyDirectoriesConf("--log-directory=test").logDirectory == Paths.get("test").toAbsolutePath)

  "--job-working-directory=" in:
    assert(dummyDirectoriesConf("--job-working-directory=DIR").jobWorkingDirectory == Paths.get("DIR").toAbsolutePath)

  "Unknown argument" in:
    provideConfigAndData { (config, data) =>
      intercept[IllegalArgumentException]:
        conf(s"--config-directory=$config", s"--data-directory=$data", "--unknown=UNKNOWN")
    }

  "System property" in:
    assert(dummyDirectoriesConf().config.getString("user.name") == sys.props("user.name"))

  private def dummyDirectoriesConf(args: String*) =
    conf((List("--config-directory=CONFIG", "--data-directory=DATA") ++ args) *)

  private def conf(args: String*) =
    SubagentConf.fromCommandLine(
      CommandLineArguments(args.toVector),
      extraConfig = config"user.name = SubagentConfTest"/*Will be overridden*/)

  "Codepages" - {
    lazy val subagentConf = SubagentConf.of(
      configDirectory = Paths.get("/tmp/CONFIG"),
      dataDirectory = Paths.get("/tmp/DATA"),
      logDirectory = Paths.get("/tmp/LOGS"),
      jobWorkingDirectory = Paths.get(if isWindows then """c:\tmp\WORKING""" else "/tmp/WORKING"),
      webServerPorts = Nil,
      config"""js7.windows.codepages.88888 = "UNKNOWN" """,
      name = "JS7")

    "windowsCodepageToEncoding" in:
      assert(subagentConf.windowsCodepageToEncoding(99999) == Left(Problem(
        "Unknown Windows code page 99999")))

      assert(subagentConf.windowsCodepageToEncoding(88888) == Left(Problem(
        "Unknown encoding for Windows code page 88888: java.nio.charset.UnsupportedCharsetException: UNKNOWN")))

      assert(subagentConf.windowsCodepageToEncoding(65001) == Right(UTF_8))
      assert(subagentConf.windowsCodepageToEncoding(1252).map(_.name) == Right("windows-1252"))

    "js7.windows.codepages are known" in:
      val cpToEncoding = SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
        .asScala
        .to(View)
        .map { case (cp, v) => cp.toInt -> v.unwrapped.asInstanceOf[String] }
        .toVector
        .sortBy(_._1)
      for (cp, name) <- cpToEncoding do
        try
          val encoding = Charset.forName(name)
          val hasCpName = encoding.name == s"cp$cp" || encoding.aliases.contains(s"cp$cp")
          logger.info(s"Windows code page $cp -> $encoding " +
            encoding.aliases.asScala.mkString(", ") + " " +
            ((encoding.name != name) ?? s" ($name) ❗️") +
            (hasCpName ?? " (derived too)"))
        catch
          case t: IllegalArgumentException => fail(s"Windows code page $cp: $t")

    "Configured codepages that can also be derived" in:
      for ((cp, nameObj) <- SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
        .asScala.toVector.sortBy(_._1.toInt))
        try
          val name = nameObj.unwrapped.asInstanceOf[String]
          val encoding = Charset.forName(name)
          val cpName = s"cp$cp"
          if encoding.name == cpName || encoding.aliases.asScala(cpName) then
            logger.info(s"Superfluous configured Windows code page $cp -> $encoding " +
              encoding.aliases.asScala.mkString(", "))
        catch
          case t: IllegalArgumentException => fail(s"Windows code page $cp: $t")

    // "Known codepages": see ListWindowsCodepages program

    "Encodings without codepage (informative)" in:
      val supportedEncodings = SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
        .asScala.values.view.map(_.unwrapped.asInstanceOf[String]).toSet
      val javaEncodings = Charset.availableCharsets().asScala.values.toSet
      val unsupported = javaEncodings
        .filterNot(o => supportedEncodings(o.name))
        .filterNot(o => (o.aliases.asScala.toSet + o.name).exists(o => o.matches("""(cp|CP)\d+""")))
      for encoding <- unsupported.toVector.sorted do
        logger.info(s"Java encoding without Windows codepage: ${encoding.name} " +
          s"${encoding.aliases.asScala.toVector.mkString(", ")}")
  }


object SubagentConfTest:
  private val shellExt = if isWindows then "cmd" else "sh"

  private def provideConfigAndData(body: (Path, Path) => Unit): Unit =
    val config = createTempDirectory("SubagentConfTest-config")
    val data = createTempDirectory("SubagentConfTest-data")
    try body(config, data)
    finally
      deleteDirectoryRecursively(config)
      deleteDirectoryRecursively(data)
