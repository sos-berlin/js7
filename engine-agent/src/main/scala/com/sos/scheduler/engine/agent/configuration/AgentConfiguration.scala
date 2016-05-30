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
import java.nio.file.{Path, Paths}
import java.time.Duration
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  dataDirectory: Path = Paths.get(sys.props("user.dir")).toAbsolutePath,
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
  environment: Map[String, String] = Map(),
  externalWebServiceClasses: immutable.Seq[Class[_ <: ExternalWebService]] = Nil,
  jobJavaOptions: immutable.Seq[String] = Nil,
  rpcKeepaliveDuration: Option[Duration] = None,
  killScript: Option[ProcessKillScript] = Some(UseInternalKillScript))
{
  for (o ← httpPort) requireTcpPortNumber(o)
  for (o ← https) requireTcpPortNumber(o.port)
  require(!(httpPort.isDefined && https.isDefined), "Command line option -http-port= cannot be combined with -https-port=")  // Spray would expect encrypted messages for http-port, too.
  require(directory.isAbsolute)

  def strippedUriPathPrefix = uriPathPrefix stripPrefix "/" stripSuffix "/"

  def withWebService[A <: ExternalWebService : ClassTag] = withWebServices(List(implicitClass[A]))

  def withWebServices(classes: Iterable[Class[_ <: ExternalWebService]]) = copy(externalWebServiceClasses = externalWebServiceClasses ++ classes)

  def crashKillScriptFile: Path = logDirectory / s"kill_tasks_after_crash_$httpPort$ShellFileExtension"

  def withHttpsPort(port: Option[Int]): AgentConfiguration =
    port match {
      case Some(o) ⇒ withHttpsPort(o)
      case None ⇒ copy(https = None)
    }

  def withHttpsPort(port: Int): AgentConfiguration =
    copy(https = Some(Https(
      port,
      https map { _.keystoreReference } getOrElse defaultKeystoreReference)))

  def defaultKeystoreReference = KeystoreReference(
    (dataDirectory / "config/private/https.jks").toUri.toURL,
    storePassword = Some(SecretString("jobscheduler")),
    keyPassword = SecretString("jobscheduler"))
}

object AgentConfiguration {
  val UseInternalKillScript = ProcessKillScript("")   // Marker
  private val TaskServerLogbackResource = JavaResource("com/sos/scheduler/engine/taskserver/configuration/logback.xml")

  def apply(args: Seq[String]): AgentConfiguration =
    CommandLineArguments.parse(args) { a ⇒
      val default = AgentConfiguration()
      val r = new AgentConfiguration(
        dataDirectory = a.as[Path]("-data-directory=", default.dataDirectory),
        httpPort = a.optionAs("-http-port=")(To(parseTcpPort)),
        httpInterfaceRestriction = a.optionAs[String]("-ip-address="),
        uriPathPrefix = a.as[String]("-uri-prefix=", default = ""),
        logDirectory = a.as[Path]("-log-directory=", default = temporaryDirectory).toAbsolutePath,
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
