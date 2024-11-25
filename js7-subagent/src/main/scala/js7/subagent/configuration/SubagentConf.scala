package js7.subagent.configuration

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Path
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
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{CommonConfiguration, Js7Configuration}
import js7.common.pekkohttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.crashpidfile.CrashPidFile
import js7.launcher.forwindows.configuration.WindowsConf
import js7.subagent.configuration.SubagentConf.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.ExecutionContext
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
  stdouterr: StdouterrConf,
  outerrByteBufferSize: Int,
  outerrQueueSize: Int,
  eventBufferDelay: FiniteDuration,
  stdoutCommitDelay: FiniteDuration,
  name: String,
  config: Config)
extends CommonConfiguration:
  require(jobWorkingDirectory.isAbsolute)

  lazy val scriptInjectionAllowed: Boolean =
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
    this

  private def provideDataSubdirectories(): this.type =
    if logDirectory == defaultLogDirectory(dataDirectory) && !exists(logDirectory) then
      createDirectory(logDirectory)
    autoCreateDirectory(workDirectory)
    autoCreateDirectory(shellScriptTmpDirectory)
    autoCreateDirectory(workTmpDirectory)
    autoCreateDirectory(valueDirectory)
    this

  def toJobLauncherConf(
    iox: IOExecutor,
    blockingJobEC: ExecutionContext,
    clock: AlarmClock,
    crashPidFile: CrashPidFile)
  : Checked[JobLauncherConf] =
    JobLauncherConf.checked(
      executablesDirectory = executablesDirectory,
      shellScriptTmpDirectory = shellScriptTmpDirectory,
      workTmpDirectory = workTmpDirectory,
      jobWorkingDirectory = jobWorkingDirectory,
      systemEncoding = config.optionAs[String]("js7.job.execution.encoding")
        .map(Charset.forName /*throws*/)
        .getOrElse(systemEncoding.orThrow),
      scriptInjectionAllowed = scriptInjectionAllowed,
      iox,
      blockingJobEC = blockingJobEC,
      clock,
      crashPidFile,
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

  val DefaultConfig: Config = Configs
    .loadResource(JavaResource("js7/subagent/configuration/subagent.conf"))
    .withFallback(Js7Configuration.defaultConfig)

  def fromCommandLine(
    args: CommandLineArguments,
    name: String = "js7",
    extraConfig: Config = ConfigFactory.empty,
    internalConfig: Config = DefaultConfig)
  : SubagentConf =
    val common = CommonConfiguration.CommonWithData.fromCommandLineArguments(args)
    import common.{configDirectory, dataDirectory}

    val config = resolvedConfig(configDirectory, dataDirectory,
      extra = extraConfig, internal = internalConfig)
    val conf = SubagentConf.fromResolvedConfig(
      configDirectory = configDirectory,
      dataDirectory = common.dataDirectory,
      workDirectory = common.workDirectory,
      logDirectory = args.optionAs("--log-directory=")(using asAbsolutePath)
        .orElse(config.optionAs("js7.job.execution.log.directory")(using asAbsolutePath))
        .getOrElse(common.logDirectory),
      jobWorkingDirectory = args.as("--job-working-directory=", WorkingDirectory)(using asAbsolutePath),
      common.webServerPorts,
      name = name,
      config)

    args.requireNoMoreArguments()
    conf
  end fromCommandLine

  @TestOnly
  def forTest(
    configAndData: Path,
    name: String,
    extraConfig: Config = ConfigFactory.empty,
    internalConfig: Config = DefaultConfig,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None)
  : SubagentConf =
    val workDirectory = configAndData / "data" / "work"
    of(
      configDirectory = configAndData / "config",
      dataDirectory = configAndData / "data",
      logDirectory = configAndData / "data" / "logs",
      jobWorkingDirectory = workDirectory,
      webServerPorts =
        httpPort.map(port => WebServerPort.localhost(port)) ++:
          httpsPort.map(port => WebServerPort.localHttps(port)).toList,
      extraConfig = extraConfig,
      internalConfig = internalConfig,
      name = name)

  @TestOnly
  def of(
    configDirectory: Path,
    dataDirectory: Path,
    logDirectory: Path,
    jobWorkingDirectory: Path,
    webServerPorts: Seq[WebServerPort],
    extraConfig: Config = ConfigFactory.empty,
    internalConfig: Config = DefaultConfig,
    name: String)
  : SubagentConf =
    fromResolvedConfig(
      configDirectory, dataDirectory,
      workDirectory = dataDirectory / "work",
      logDirectory, jobWorkingDirectory,
      webServerPorts, name,
      resolvedConfig(configDirectory, dataDirectory, extraConfig, internalConfig))

  private def fromResolvedConfig(
    configDirectory: Path,
    dataDirectory: Path,
    workDirectory: Path,
    logDirectory: Path,
    jobWorkingDirectory: Path,
    webServerPorts: Seq[WebServerPort],
    name: String,
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
      outErrConf,
      outerrByteBufferSize = config.memorySizeAsInt("js7.order.stdout-stderr.byte-buffer-size")
        .orThrow.min(outErrConf.chunkSize),
      outerrQueueSize = config.memorySizeAsInt("js7.order.stdout-stderr.queue-size")
        .orThrow.max(1),
      eventBufferDelay = config.finiteDuration("js7.subagent-driver.event-buffer-delay").orThrow,
      stdoutCommitDelay = config.finiteDuration("js7.order.stdout-stderr.commit-delay").orThrow,
      name = name,
      config)

  private def resolvedConfig(configDir: Path, dataDir: Path, extra: Config, internal: Config)
  : Config =
    ConfigFactory
      .systemProperties
      .withFallback(extra)
      .withFallback(ConfigFactory.parseMap(
        Map(
          "js7.config-directory" -> configDir.toString,
          "js7.data-directory" -> dataDir.toString
        ).asJava))
      .withFallback(parseConfigIfExists(configDir / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDir / "agent.conf", secret = false))
      .withFallback(internal)
      .resolve

  private def autoCreateDirectory(directory: Path): Path =
    if !exists(directory) then createDirectory(directory)
    directory
