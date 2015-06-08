package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.utils.TcpUtils.{parseTcpPort, requireTcpPortNumber}
import java.nio.file.{Path, Paths}
import org.scalactic.Requirements._
import scala.collection.immutable

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
  directory: Path = Paths.get(sys.props("user.dir")).toAbsolutePath,
  environment: immutable.Iterable[(String, String)] = Nil)
{
  requireTcpPortNumber(httpPort)
  require(directory.isAbsolute)
}

object AgentConfiguration {
  def apply(args: Seq[String]) = {
    val arguments = CommandLineArguments(args)
    val httpPort = arguments.asConverted("-http-port=")(parseTcpPort)
    arguments.requireNoMoreArguments()
    new AgentConfiguration(httpPort = httpPort)
  }
}
