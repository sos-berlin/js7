package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.configuration.AgentConfiguration._
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.configutils.Configs._
import com.sos.scheduler.engine.common.convert.As
import com.sos.scheduler.engine.common.convert.As.asAbsolutePath
import com.sos.scheduler.engine.common.process.Processes.ShellFileExtension
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.{EmptyPath, WorkingDirectory}
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicitClass
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.tcp.TcpUtils.{parseTcpPort, requireTcpPortNumber}
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.data.DotnetConfiguration
import com.sos.scheduler.engine.taskserver.task.process.ProcessKillScriptProvider
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.{Path, Paths}
import java.time.Duration
import org.scalactic.Requirements._
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  dataDirectory: Option[Path],
  httpPort: Option[Int],
  https: Option[Https],
  /**
   * The IP address of the only network interface, the Agent should listen to.
   * If empty, the Agent listens to all network interfaces.
   */
  httpInterfaceRestriction: Option[String],
  uriPathPrefix: String,
  externalWebServiceClasses: immutable.Seq[Class[_ <: ExternalWebService]],
  workingDirectory: Path = WorkingDirectory,
  logDirectory: Path,
  environment: Map[String, String],
  jobJavaOptions: immutable.Seq[String],
  dotnet: DotnetConfiguration,
  rpcKeepaliveDuration: Option[Duration],
  killScript: Option[ProcessKillScript],
  config: Config)  // Should not be the first argument to avoid the misleading call AgentConfiguration(config)
{
  for (o ← httpPort) requireTcpPortNumber(o)
  for (o ← https) requireTcpPortNumber(o.port)
  require(!(uriPathPrefix.startsWith("/") || uriPathPrefix.endsWith("/")))
  require(workingDirectory.isAbsolute)

  private def withCommandLineArguments(a: CommandLineArguments): AgentConfiguration = {
    var v = copy(
      httpPort = a.optionAs("-http-port=", httpPort)(As(parseTcpPort)),
      https = a.optionAs("-https-port=")(As(parseTcpPort)) map portToHttps orElse https,
      httpInterfaceRestriction = a.optionAs[String]("-ip-address=", httpInterfaceRestriction),
      uriPathPrefix = a.as[String]("-uri-prefix=", uriPathPrefix) stripPrefix "/" stripSuffix "/",
      logDirectory = a.optionAs("-log-directory=")(asAbsolutePath) getOrElse logDirectory,
      jobJavaOptions = a.optionAs[String]("-job-java-options=") map { o ⇒ List(o) } getOrElse jobJavaOptions,
      rpcKeepaliveDuration = a.optionAs[Duration]("-rpc-keepalive=", rpcKeepaliveDuration))
    v = v withKillScript a.optionAs[String]("-kill-script=")
    for (o ← a.optionAs("-dotnet-class-directory=")(asAbsolutePath)) {
      v = v.copy(dotnet = DotnetConfiguration(classDllDirectory = Some(o)))
    }
    v
  }

  private def withKillScript(killScriptPath: Option[String]) = killScriptPath match {
    case None ⇒ this  // -kill-script= not given: Agent uses the internally provided kill script
    case Some("") ⇒ copy(killScript = None)      // -kill-script= (empty argument) means: don't use any kill script
    case Some(o) ⇒ copy(killScript = Some(ProcessKillScript(Paths.get(o).toAbsolutePath)))
  }

  def withHttpsPort(port: Int) = copy(https = Some(portToHttps(port)))

  private def portToHttps(port: Int) = Https(
    port,
    https map { _.keystoreReference } getOrElse
      KeystoreReference(
        (privateDirectory / "private-https.jks").toUri.toURL,
        storePassword = config.optionAs[SecretString]("jobscheduler.agent.https.keystore.password"),
        keyPassword = Some(config.as[SecretString]("jobscheduler.agent.https.keystore.password"))))

  def withWebService[A <: ExternalWebService : ClassTag] = withWebServices(List(implicitClass[A]))

  def withWebServices(classes: Iterable[Class[_ <: ExternalWebService]]) =
    copy(externalWebServiceClasses = externalWebServiceClasses ++ classes)

  def withDotnetAdapterDirectory(directory: Option[Path]) = copy(dotnet = dotnet.copy(adapterDllDirectory = directory))

  private def privateDirectory = dataDirectory map { _ / "config/private" } getOrElse {
    throw new IllegalArgumentException("Missing dataDirectory")
  }

  private[configuration] def finishAndProvideFiles: AgentConfiguration =
    killScript match {
      case Some(ProcessKillScript(DelayUntilFinishFile)) ⇒
        // After Agent termination, leave behind the kill script, in case of regular termination after error.
        val identifyingPort = https map { _.port } orElse httpPort getOrElse 0
        val provider = new ProcessKillScriptProvider(httpPort = identifyingPort)
        copy(killScript = Some(provider.provideTo(logDirectory)))  // logDirectory for lack of a work directory
      case _ ⇒ this
    }

  def crashKillScriptFile: Path = logDirectory / s"kill_tasks_after_crash_${https map { _.port } getOrElse httpPort.get}$ShellFileExtension"

  lazy val authUsersConfig: Config = config.getConfig("jobscheduler.agent.auth.users")
}

object AgentConfiguration {
  private val DelayUntilFinishFile = EmptyPath  // Marker for finish
  private[configuration] lazy val DefaultsConfig = ConfigFactory.parseResources(getClass.getClassLoader,
    "com/sos/scheduler/engine/agent/configuration/defaults.conf")

  def apply(args: Seq[String]) = CommandLineArguments.parse(args) { a ⇒
    fromDataDirectory(a.optionAs[Path]("-data-directory=")) withCommandLineArguments a
  }

  def fromDataDirectory(dataDirectory: Option[Path]): AgentConfiguration = {
    val data = dataDirectory map { _.toAbsolutePath }
    val config = resolvedConfig(data)
    val c = config.getConfig("jobscheduler.agent")
    var v = new AgentConfiguration(
      dataDirectory = data,
      httpPort = c.optionAs("http.port")(As(parseTcpPort)),
      https = None,  // Changed below
      httpInterfaceRestriction = c.optionAs[String]("http.ip-address"),
      externalWebServiceClasses = Nil,
      uriPathPrefix = c.as[String]("http.uri-prefix") stripPrefix "/" stripSuffix "/",
      logDirectory = c.optionAs("log.directory")(asAbsolutePath) orElse (data map { _ / "logs" }) getOrElse temporaryDirectory,
      environment = Map(),
      dotnet = DotnetConfiguration(classDllDirectory = c.optionAs("task.dotnet.class-directory")(asAbsolutePath)),
      rpcKeepaliveDuration = c.durationOption("task.rpc.keepalive.duration"),
      jobJavaOptions = c.stringSeq("task.java.options"),
      killScript = Some(ProcessKillScript(DelayUntilFinishFile)),  // Changed below
      config = config)
    v = v.withKillScript(c.optionAs[String]("task.kill.script"))
    for (o ← c.optionAs("https.port")(As(parseTcpPort))) {
      v = v withHttpsPort o
    }
    v
  }

  private def resolvedConfig(dataDirectory: Option[Path]): Config = {
    val dataConfig = dataDirectory map dataDirectoryConfig getOrElse ConfigFactory.empty
    (dataConfig withFallback DefaultsConfig).resolve
  }

  private def dataDirectoryConfig(dataDirectory: Path): Config =
    ConfigFactory
      .parseMap(Map("jobscheduler.agent.data.directory" → dataDirectory.toString))  // For substitution of ${jobscheduler.agent.data.directory}
      .withFallback(parseConfigIfExists(dataDirectory / "config/private/private.conf"))
      .withFallback(parseConfigIfExists(dataDirectory / "config/agent.conf"))

  final case class Https(port: Int, keystoreReference: KeystoreReference)

  object forTest {
    private val TaskServerLogbackResource = JavaResource("com/sos/scheduler/engine/taskserver/configuration/logback.xml")

    def apply(data: Option[Path] = None, httpPort: Int = findRandomFreeTcpPort()) =
      fromDataDirectory(data).copy(
        httpPort = Some(httpPort),
        httpInterfaceRestriction = Some("127.0.0.1"),
        jobJavaOptions = List(s"-Dlogback.configurationFile=${TaskServerLogbackResource.path}") ++ sys.props.get("agent.job.javaOptions"))
  }
}
