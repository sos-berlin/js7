package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.configuration.AgentConfiguration._
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.convert.Converters.To
import com.sos.scheduler.engine.common.process.Processes.ShellFileExtension
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicitClass
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.tcp.TcpUtils.{parseTcpPort, requireTcpPortNumber}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.data.DotnetConfiguration
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.{Path, Paths}
import java.time.Duration
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  dataDirectory: Option[Path] = None,
  https: Option[Https] = None,
  httpPort: Option[Int] = None,
  /**
   * The IP address of the only network interface, the Agent should listen to.
   * If empty, the Agent listens to all network interfaces.
   */
  httpInterfaceRestriction: Option[String] = None,
  /** Prefix slash and suffix slash are striped. **/
  uriPathPrefix: String = "",
  directory: Path = Paths.get(sys.props("user.dir")).toAbsolutePath,
  logDirectory: Path = temporaryDirectory,
  dotnet: DotnetConfiguration = DotnetConfiguration(),
  environment: Map[String, String] = Map(),
  externalWebServiceClasses: immutable.Seq[Class[_ <: ExternalWebService]] = Nil,
  jobJavaOptions: immutable.Seq[String] = Nil,
  rpcKeepaliveDuration: Option[Duration] = None,
  killScript: Option[ProcessKillScript] = Some(UseInternalKillScript))
{
  for (o ← httpPort) requireTcpPortNumber(o)
  for (o ← https) requireTcpPortNumber(o.port)
  require(directory.isAbsolute)

  def strippedUriPathPrefix = uriPathPrefix stripPrefix "/" stripSuffix "/"

  def withWebService[A <: ExternalWebService : ClassTag] = withWebServices(List(implicitClass[A]))

  def withWebServices(classes: Iterable[Class[_ <: ExternalWebService]]) = copy(externalWebServiceClasses = externalWebServiceClasses ++ classes)

  def withDotnetAdapterDirectory(directory: Option[Path]) = copy(dotnet = dotnet.copy(adapterDllDirectory = directory))

  def crashKillScriptFile: Path = logDirectory / s"kill_tasks_after_crash_${https map { _.port } getOrElse httpPort.get}$ShellFileExtension"

  def withHttpsPort(port: Option[Int]): AgentConfiguration = port map withHttpsPort getOrElse copy(https = None)

  def withHttpsPort(port: Int): AgentConfiguration =
    copy(https = Some(Https(
      port,
      https map { _.keystoreReference } getOrElse defaultKeystoreReference)))

  private def defaultKeystoreReference = KeystoreReference(
    (privateDirectory / "private-https.jks").toUri.toURL,
    storePassword = Some(SecretString("jobscheduler")),
    keyPassword = Some(SecretString("jobscheduler")))

  private def privateDirectory = privateDirectoryOption getOrElse sys.error("Missing dataDirectory")

  def privateDirectoryOption = configDirectoryOption map { _ / "private" }

  def configDirectoryOption = dataDirectory map { _ / "config" }
}

object AgentConfiguration {
  private val ConfigResource = JavaResource("com/sos/scheduler/engine/agent/configuration/default.conf")
  lazy val DefaultConfig: Config = ConfigFactory.load(getClass.getClassLoader, ConfigResource.path)
  val UseInternalKillScript = ProcessKillScript("")   // Marker
  private[configuration] val Default = AgentConfiguration()
  private val TaskServerLogbackResource = JavaResource("com/sos/scheduler/engine/taskserver/configuration/logback.xml")

  def apply(args: Seq[String]): AgentConfiguration =
    CommandLineArguments.parse(args) { a ⇒
      val r = AgentConfiguration(
        dataDirectory = Some(a.as[Path]("-data-directory=")),
        httpPort = a.optionAs("-http-port=")(To(parseTcpPort)),
        httpInterfaceRestriction = a.optionAs[String]("-ip-address="),
        uriPathPrefix = a.as[String]("-uri-prefix=", Default.uriPathPrefix),
        logDirectory = a.as[Path]("-log-directory=", Default.logDirectory).toAbsolutePath,
        dotnet = DotnetConfiguration(classDllDirectory = a.optionAs[Path]("-dotnet-class-directory=") map { _.toAbsolutePath }),
        rpcKeepaliveDuration = a.optionAs("-rpc-keepalive=")(To(parseDuration)),
        jobJavaOptions = a.optionAs[String]("-job-java-options=").toList)
      r.copy(killScript = a.optionAs[String]("-kill-script=") match {
        case None ⇒ r.killScript  // -kill-script= not given: Agent uses the internally provided kill script
        case Some("") ⇒ None      // -kill-script= (empty argument) means: don't use any kill script
        case Some(o) ⇒ Some(ProcessKillScript(Paths.get(o).toAbsolutePath))
      })
      .withHttpsPort(a.optionAs("-https-port=")(To(parseTcpPort)))
    }

  def forTest(httpPort: Int = findRandomFreeTcpPort()) = AgentConfiguration(
    httpPort = Some(httpPort),
    httpInterfaceRestriction = Some("127.0.0.1"),
    jobJavaOptions = List(s"-Dlogback.configurationFile=${TaskServerLogbackResource.path}") ++ sys.props.get("agent.job.javaOptions"))

  final case class Https(port: Int, keystoreReference: KeystoreReference)
}
