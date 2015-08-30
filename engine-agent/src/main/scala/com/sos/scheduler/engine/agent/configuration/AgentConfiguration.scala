package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.web.common.WebService
import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicitClass
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.common.utils.TcpUtils.{parseTcpPort, requireTcpPortNumber}
import java.nio.file.{Path, Paths}
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
  environment: immutable.Iterable[(String, String)] = Nil,
  webServiceClasses: immutable.Seq[Class[_ <: WebService]] = Nil,
  jobJavaOptions: immutable.Seq[String] = Nil,
  killScriptFile: Option[Path] = None)
{
  requireTcpPortNumber(httpPort)
  require(directory.isAbsolute)

  def strippedUriPathPrefix = uriPathPrefix stripPrefix "/" stripSuffix "/"

  def withWebServiceProvider[A <: WebService : ClassTag]: AgentConfiguration =
    copy(webServiceClasses = webServiceClasses :+ implicitClass[A])
}

object AgentConfiguration {
  def apply(args: Seq[String]): AgentConfiguration =
    CommandLineArguments.parse(args) { a ⇒
      new AgentConfiguration(
        httpPort = a.asConverted("-http-port=")(parseTcpPort),
        httpInterfaceRestriction = a.getString("-ip-address="),
        uriPathPrefix = a.getString("-uri-prefix=") getOrElse "",
        killScriptFile = a.getString("-kill-script=") map { o ⇒ Paths.get(o).toAbsolutePath })
    }

  def forTest(httpPort: Int = findRandomFreeTcpPort()) = AgentConfiguration(
    httpPort = httpPort,
    httpInterfaceRestriction = Some("127.0.0.1"),
    jobJavaOptions = sys.props.get("agent.job.javaOptions").toList)
}
