package js7.subagent.configuration

import cats.syntax.semigroup.*
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import js7.base.configutils.Configs
import js7.base.configutils.Configs.{ConvertibleConfig, RichConfig, parseConfigIfExists}
import js7.base.convert.AsJava.asAbsolutePath
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.WorkingDirectory
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.catchExpected
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkohttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{CommonConfiguration, Js7Configuration}
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.launcher.configuration.{JobLauncherConf, ProcessKillScript}
import js7.launcher.forwindows.configuration.WindowsConf
import js7.launcher.process.ProcessKillScriptProvider
import js7.subagent.configuration.SubagentConf.*
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

final case class SubagentConf(
  configDirectory: Path,
  dataDirectory: Path,
  workDirectory: Path,
  logDirectory: Path,
  jobWorkingDirectory: Path = WorkingDirectory,
  webServerPorts: Seq[WebServerPort],
  defaultJobSigkillDelay: FiniteDuration,
  killScript: Option[ProcessKillScript],
  stdouterr: StdouterrConf,
  outerrCharBufferSize: Int,
  stdoutCommitDelay: FiniteDuration,
  name: String,
  config: Config)
extends CommonConfiguration:
  require(jobWorkingDirectory.isAbsolute)

  lazy val scriptInjectionAllowed =
    config.getBoolean("js7.job.execution.signed-script-injection-allowed")

  def executablesDirectory: Path =
    configDirectory / "executables"

  private lazy val shellScriptTmpDirectory: Path =
    workDirectory / "scripts"

  private lazy val workTmpDirectory: Path =
    workDirectory / "tmp"

  /** Directory for temporary value files. */
  lazy val valueDirectory: Path =
    workDirectory / "values"

  def finishAndProvideFiles(): SubagentConf =
    provideDataSubdirectories()
    if killScript.contains(ProcessKillScriptProvider.directoryToProcessKillScript(workDirectory)) then
      // After Subagent termination, leave behind the kill script,
      // in case of regular termination after error.
      new ProcessKillScriptProvider() //.closeWithCloser
        .provideTo(workDirectory)
    this

  private def provideDataSubdirectories(): this.type =
    if logDirectory == defaultLogDirectory(dataDirectory) && !exists(logDirectory) then
      createDirectory(logDirectory)
    autoCreateDirectory(workDirectory)
    autoCreateDirectory(shellScriptTmpDirectory)
    autoCreateDirectory(workTmpDirectory)
    autoCreateDirectory(valueDirectory)
    this

  def toJobLauncherConf(iox: IOExecutor, blockingJobScheduler: Scheduler, clock: AlarmClock)
  : Checked[JobLauncherConf] =
    JobLauncherConf.checked(
      executablesDirectory = executablesDirectory,
      shellScriptTmpDirectory = shellScriptTmpDirectory,
      workTmpDirectory = workTmpDirectory,
      jobWorkingDirectory = jobWorkingDirectory,
      systemEncoding = config.optionAs[String]("js7.job.execution.encoding")
        .map(Charset.forName /*throws*/)
        .getOrElse(systemEncoding.orThrow),
      killScript = killScript,
      scriptInjectionAllowed = scriptInjectionAllowed,
      iox,
      blockingJobScheduler = blockingJobScheduler,
      clock,
      config)

  private val systemEncoding: Checked[Charset] =
    if isWindows then
      WindowsConf.codepage.flatMap(windowsCodepageToEncoding)
    else
      Right(UTF_8)

  private[configuration] def windowsCodepageToEncoding(codepage: Int): Checked[Charset] =
    val key = s"js7.windows.codepages.$codepage"
    config.optionAs[String](key) match
      case None =>
        catchExpected[Exception](Charset.forName("cp" + codepage))
          .orElse(catchExpected[Exception](Charset.forName("CP" + codepage)))
          .left.map(_ => Problem(s"Unknown Windows code page $codepage"))

      case Some(encodingName) =>
        catchExpected[Exception](Charset.forName(encodingName))
          .left.map(Problem(s"Unknown encoding for Windows code page $codepage:") |+| _)

object SubagentConf:
  private def defaultLogDirectory(data: Path) = data / "logs"

  val DefaultConfig = Configs
    .loadResource(JavaResource("js7/subagent/configuration/subagent.conf"))
    .withFallback(Js7Configuration.defaultConfig)

  def fromCommandLine(
    args: CommandLineArguments,
    name: String = "JS7",
    extraConfig: Config = ConfigFactory.empty,
    internalConfig: Config = DefaultConfig)
  : SubagentConf =
    val common = CommonConfiguration.Common.fromCommandLineArguments(args)
    import common.configDirectory as configDir

    def toKillScriptSetting(path: String): Option[ProcessKillScript] =
      path.nonEmpty ? ProcessKillScript(Paths.get(path).toAbsolutePath)

    val config = resolvedConfig(configDir, extra = extraConfig, internal = internalConfig)
    val conf = SubagentConf.fromResolvedConfig(
      configDirectory = common.configDirectory,
      dataDirectory = common.dataDirectory,
      workDirectory = common.workDirectory,
      logDirectory = args.optionAs("--log-directory=")(asAbsolutePath)
        .orElse(config.optionAs("js7.job.execution.log.directory")(asAbsolutePath))
        .getOrElse(common.logDirectory),
      jobWorkingDirectory =
        args.as("--job-working-directory=", WorkingDirectory)(asAbsolutePath),
      common.webServerPorts,
      killScript =
        args.optionAs[String]("--kill-script=") match {
          case Some(path) => toKillScriptSetting(path)
          case None =>
            config.optionAs[String]("js7.job.execution.kill.script") match {
              case Some(path) => toKillScriptSetting(path)
              case None => Some(
                ProcessKillScriptProvider.directoryToProcessKillScript(common.workDirectory))
            }
        },
      name = name,
      config)

    args.requireNoMoreArguments()
    conf

  def forTest(
    configAndData: Path,
    name: String,
    extraConfig: Config = ConfigFactory.empty,
    internalConfig: Config = DefaultConfig,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None)
  : SubagentConf =
    SubagentConf.of(
      configDirectory = configAndData / "config",
      dataDirectory = configAndData / "data",
      logDirectory = configAndData / "data" / "logs",
      jobWorkingDirectory = configAndData / "data" / "work",
      webServerPorts =
        httpPort.map(port => WebServerPort.localhost(port)) ++:
          httpsPort.map(port => WebServerPort.localHttps(port)).toList,
      killScript = None,
      extraConfig = extraConfig,
      internalConfig = internalConfig,
      name = name)

  def of(
    configDirectory: Path,
    dataDirectory: Path,
    logDirectory: Path,
    jobWorkingDirectory: Path,
    webServerPorts: Seq[WebServerPort],
    killScript: Option[ProcessKillScript],
    extraConfig: Config = ConfigFactory.empty,
    internalConfig: Config = DefaultConfig,
    name: String = "JS7")
  : SubagentConf =
    fromResolvedConfig(
      configDirectory, dataDirectory,
      workDirectory = dataDirectory / "work",
      logDirectory, jobWorkingDirectory,
      webServerPorts, killScript, name,
      resolvedConfig(configDirectory, extraConfig, internalConfig))

  private def fromResolvedConfig(
    configDirectory: Path,
    dataDirectory: Path,
    workDirectory: Path,
    logDirectory: Path,
    jobWorkingDirectory: Path,
    webServerPorts: Seq[WebServerPort],
    killScript: Option[ProcessKillScript],
    name: String = "JS7",
    config: Config)
  : SubagentConf =
    val outErrConf = StdouterrConf.fromConfig(config)
    SubagentConf(
      configDirectory = configDirectory,
      dataDirectory = dataDirectory,
      workDirectory = workDirectory,
      logDirectory = logDirectory,
      jobWorkingDirectory = jobWorkingDirectory,
      webServerPorts,
      defaultJobSigkillDelay = config.finiteDuration("js7.job.execution.sigkill-delay").orThrow,
      killScript,
      outErrConf,
      outerrCharBufferSize = config.memorySizeAsInt("js7.order.stdout-stderr.char-buffer-size")
        .orThrow.min(outErrConf.chunkSize),
      stdoutCommitDelay = config.finiteDuration("js7.order.stdout-stderr.commit-delay").orThrow,
      name = name,
      config)

  private def resolvedConfig(configDir: Path, extra: Config, internal: Config): Config =
    ConfigFactory
      .systemProperties
      .withFallback(extra)
      .withFallback(ConfigFactory
        .parseMap(
          Map("js7.config-directory" -> configDir.toString).asJava))
      .withFallback(parseConfigIfExists(configDir / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDir / "agent.conf", secret = false))
      .withFallback(internal)
      .resolve

  private def autoCreateDirectory(directory: Path): Path =
    if !exists(directory) then createDirectory(directory)
    directory
