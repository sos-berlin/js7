package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicitClass
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.common.utils.TcpUtils.{parseTcpPort, requireTcpPortNumber}
import java.nio.file.{Path, Paths}
import java.time.Duration
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  httpPort: Int,
  /**
   * The IP address of the only network interface, the Agent should listen to.
   * If empty, the Agent listens to all network interfaces.
   */
  httpInterfaceRestriction: Option[String] = None,
  /** Prefix slash and suffix slash are striped. **/
  uriPathPrefix: String = "",
  directory: Path = Paths.get(sys.props("user.dir")).toAbsolutePath,
  logDirectory: Path = temporaryDirectory,
  environment: immutable.Iterable[(String, String)] = Nil,
  externalWebServiceClasses: immutable.Seq[Class[_ <: ExternalWebService]] = Nil,
  jobJavaOptions: immutable.Seq[String] = Nil,
  rpcKeepaliveDuration: Option[Duration] = None,
  killScriptFile: Option[Path] = None)
{
  requireTcpPortNumber(httpPort)
  require(directory.isAbsolute)

  def strippedUriPathPrefix = uriPathPrefix stripPrefix "/" stripSuffix "/"

  def withWebService[A <: ExternalWebService : ClassTag] = withWebServices(List(implicitClass[A]))

  def withWebServices(classes: Iterable[Class[_ <: ExternalWebService]]) = copy(externalWebServiceClasses = externalWebServiceClasses ++ classes)
}

object AgentConfiguration {
  def apply(args: Seq[String]): AgentConfiguration =
    CommandLineArguments.parse(args) { a ⇒
      new AgentConfiguration(
        httpPort = a.asConverted("-http-port=")(parseTcpPort),
        httpInterfaceRestriction = a.getString("-ip-address="),
        uriPathPrefix = a.getString("-uri-prefix=") getOrElse "",
        logDirectory = a.asConvertedOption("-log-directory=") { o ⇒ Paths.get(o).toAbsolutePath } getOrElse temporaryDirectory,
        rpcKeepaliveDuration = Some(a.asConvertedOption("-rpc-keepalive=")(parseDuration) getOrElse 300.s),
        killScriptFile = a.getString("-kill-script=") map { o ⇒ Paths.get(o).toAbsolutePath },
        jobJavaOptions = a.getString("-job-java-options=").toList)
    }

  def forTest(httpPort: Int = findRandomFreeTcpPort()) = AgentConfiguration(
    httpPort = httpPort,
    httpInterfaceRestriction = Some("127.0.0.1"),
    jobJavaOptions = sys.props.get("agent.job.javaOptions").toList)
}
