package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.configuration.AgentConfiguration._
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.configutils.Configs._
import com.sos.scheduler.engine.common.convert.Converters.To
import com.sos.scheduler.engine.common.process.Processes.ShellFileExtension
import com.sos.scheduler.engine.common.scalautil.FileUtils.EmptyPath
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.{SwitchStatement, implicitClass}
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.tcp.TcpUtils.{parseTcpPort, requireTcpPortNumber}
import com.sos.scheduler.engine.common.time.ScalaTime._
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
  dataDirectory: Option[Path] = None,
  httpPort: Option[Int] = None,
  https: Option[Https] = None,
  /**
   * The IP address of the only network interface, the Agent should listen to.
   * If empty, the Agent listens to all network interfaces.
   */
  httpInterfaceRestriction: Option[String] = None,
  uriPathPrefix: String = "",
  workingDirectory: Path = Paths.get(sys.props("user.dir")).toAbsolutePath,
  logDirectory: Path = temporaryDirectory,
  dotnet: DotnetConfiguration = DotnetConfiguration(),
  environment: Map[String, String] = Map(),
  externalWebServiceClasses: immutable.Seq[Class[_ <: ExternalWebService]] = Nil,
  jobJavaOptions: immutable.Seq[String] = Nil,
  rpcKeepaliveDuration: Option[Duration] = None,
  killScript: Option[ProcessKillScript] = Some(ProcessKillScript(DelayUntilFinishFile)),
  config: Config = DefaultsConfig)  // Should not be the first argument to avoid the misleading call AgentConfiguration(config)
{
  for (o ← httpPort) requireTcpPortNumber(o)
  for (o ← https) requireTcpPortNumber(o.port)
  require(!(uriPathPrefix.startsWith("/") || uriPathPrefix.endsWith("/")))
  require(workingDirectory.isAbsolute)

  def withWebService[A <: ExternalWebService : ClassTag] = withWebServices(List(implicitClass[A]))

  def withWebServices(classes: Iterable[Class[_ <: ExternalWebService]]) = copy(externalWebServiceClasses = externalWebServiceClasses ++ classes)

  def withDotnetAdapterDirectory(directory: Option[Path]) = copy(dotnet = dotnet.copy(adapterDllDirectory = directory))

  def crashKillScriptFile: Path = logDirectory / s"kill_tasks_after_crash_${https map { _.port } getOrElse httpPort.get}$ShellFileExtension"

  def withHttpsPort(port: Option[Int]): AgentConfiguration = port map withHttpsPort getOrElse copy(https = None)

  def withHttpsPort(port: Int): AgentConfiguration =
    copy(https = Some(Https(
      port,
      https map { _.keystoreReference } getOrElse
        KeystoreReference(
          (privateDirectory / "private-https.jks").toUri.toURL,
          storePassword = config.optionAs[SecretString]("jobscheduler.agent.https.keystore.password"),
          keyPassword = Some(config.as[SecretString]("jobscheduler.agent.https.keystore.password"))))))

  private def privateDirectory = dataDirectory map { _ / "config/private" } getOrElse { throw new IllegalArgumentException("Missing data") }

  lazy val authUsersConfig: Config = config.getConfig("jobscheduler.agent.auth.users")

  def withConfig(config: Config): AgentConfiguration = {
    val c = config.getConfig("jobscheduler.agent")
    var v = copy(
        config = config,
        httpPort = c.optionAs("http.port", httpPort)(To(parseTcpPort)),
        httpInterfaceRestriction = c.optionAs[String]("http.ip-address", httpInterfaceRestriction),
        uriPathPrefix = c.as[String]("http.uri-prefix", uriPathPrefix) stripPrefix "/" stripSuffix "/",
        logDirectory = c.optionAs[Path]("log.directory") map { _.toAbsolutePath } getOrElse logDirectory,
        rpcKeepaliveDuration = c.optionAs("task.rpc.keepalive.duration", rpcKeepaliveDuration)(To(parseDuration)),
        jobJavaOptions = c.stringSeq("task.java.options", jobJavaOptions))
      .withKillScript(c.optionAs[String]("task.kill.script"))
    for (o ← c.optionAs("https.port")(To(parseTcpPort))) {
      v = v.withHttpsPort(o)
    }
    for (o ← c.optionAs[Path]("task.dotnet.class-directory")) {
      v = v.copy(dotnet = DotnetConfiguration(classDllDirectory = Some(o.toAbsolutePath)))
    }
    v
  }

  def withCommandLineArguments(a: CommandLineArguments): AgentConfiguration = {
    var v = copy(
        dataDirectory = a.optionAs[Path]("-data-directory=", dataDirectory) map { _.toAbsolutePath },
        httpPort = a.optionAs("-http-port=", httpPort)(To(parseTcpPort)),
        httpInterfaceRestriction = a.optionAs[String]("-ip-address=", httpInterfaceRestriction),
        uriPathPrefix = a.as[String]("-uri-prefix=", uriPathPrefix) stripPrefix "/" stripSuffix "/",
        logDirectory = a.optionAs[Path]("-log-directory=") map { _.toAbsolutePath } getOrElse logDirectory,
        rpcKeepaliveDuration = a.optionAs("-rpc-keepalive=", rpcKeepaliveDuration)(To(parseDuration)),
        jobJavaOptions = a.optionAs[String]("-job-java-options=") match {
          case Some(o) ⇒ List(o)
          case None ⇒ jobJavaOptions
        })
      .withKillScript(a.optionAs[String]("-kill-script="))
    for (o ← a.optionAs("-https-port=")(To(parseTcpPort))) {
      v = v.withHttpsPort(o)
    }
    for (o ← a.optionAs[Path]("-dotnet-class-directory=")) {
      v = v.copy(dotnet = DotnetConfiguration(classDllDirectory = Some(o.toAbsolutePath)))
    }
    v
  }

  def withKillScript(killScriptPath: Option[String]) = killScriptPath match {
    case None ⇒ this  // -kill-script= not given: Agent uses the internally provided kill script
    case Some("") ⇒ copy(killScript = None)      // -kill-script= (empty argument) means: don't use any kill script
    case Some(o) ⇒ copy(killScript = Some(ProcessKillScript(Paths.get(o).toAbsolutePath)))
  }

  def finishAndProvideFiles: AgentConfiguration =
    if (this.killScript contains ProcessKillScript(DelayUntilFinishFile)) {
      // After Agent termination, leave behind the kill script, in case of regular termination after error.
      val identifyingPort = https map { _.port } orElse httpPort getOrElse 0
      val provider = new ProcessKillScriptProvider(httpPort = identifyingPort)
      copy(killScript = Some(provider.provideTo(logDirectory)))  // logDirectory for lack of a work directory
    }
    else this
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
    var v = new AgentConfiguration(dataDirectory = data)
    data switch { case Some(o) ⇒
      v = v.copy(logDirectory = o / "logs")
    }
    v withConfig resolvedConfig(data)
  }

  private def resolvedConfig(dataDirectory: Option[Path]): Config = {
    val dataConfig = dataDirectory match {
      case Some(data) ⇒ ConfigFactory
        .parseMap(Map("jobscheduler.agent.data.directory" → data.toString))  // For substitution of ${jobscheduler.agent.data.directory}
        .withFallback(parseConfigIfExists(data / "config/private/private.conf"))
        .withFallback(parseConfigIfExists(data / "config/agent.conf"))
      case None ⇒ ConfigFactory.empty
    }
    (dataConfig withFallback DefaultsConfig).resolve
  }

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
