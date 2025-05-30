package js7.common.internet

import cats.Show
import java.net.{InetAddress, InetSocketAddress}
import js7.base.convert.As
import js7.base.convert.As.convert
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object IP:

  implicit object StringToInetAddress extends As[String, InetAddress]:
    def apply(o: String): InetAddress =
      // getByName resolves a blank " " name, too (macOS).
      if o.trim.isEmpty then throw new IllegalArgumentException("Missing IP address")
      InetAddress.getByName(o)

  def toInetSocketAddress(string: String, defaultHost: String, defaultPort: Option[Int] = None)
  : InetSocketAddress =
    convert[String, InetSocketAddress](string)(
      using new StringToInetSocketAddress(defaultHost, defaultPort))

  val StringToServerInetSocketAddress: StringToInetSocketAddress =
    new StringToInetSocketAddress(defaultHost = "0.0.0.0", defaultPort = None)


  final class StringToInetSocketAddress(defaultHost: String, defaultPort: Option[Int])
  extends As[String, InetSocketAddress]:
    import StringToInetSocketAddress.*

    def apply(string: String): InetSocketAddress =
      try string match
        case StandardRegex(host, port) => useDefaults(host, port)
        case IPv6Regex(host, port) => useDefaults(host, port)
        case IPv6Host(host) => useDefaults(host, defaultPortString)
        case "" => makeInetSocketAddress(defaultHost, defaultPortString)
        case _ if string.forall(_.isDigit) => makeInetSocketAddress(defaultHost, string)
        case _ => makeInetSocketAddress(string, defaultPortString)
      catch
        case NonFatal(t) => throw new IllegalArgumentException(
          s"Invalid IP address and port combination in '$string': $t", t)

    private def useDefaults(host: String, port: String) =
      makeInetSocketAddress(host.substitute("", defaultHost), port.substitute("", defaultPort.toString))

    private def defaultPortString = defaultPort.map(_.toString) getOrElse ""


  implicit object StringToInetSocketAddress extends As[String, InetSocketAddress]:
    private val StandardRegex = """([^:]*):(\d*)""".r
    private val IPv6Regex = """\[(.*)\]:(\d*)""".r
    private val IPv6Host = """\[(.*)\]""".r

    def apply(string: String): InetSocketAddress =
      string match
        case StandardRegex(host, port) => makeInetSocketAddress(host, port)
        case IPv6Regex(host, port) => makeInetSocketAddress(host, port)
        case _ => throw new IllegalArgumentException("ipAddress:port or [ip6Address]:port expected")

    private def makeInetSocketAddress(host: String, port: String) =
      if host.trim.isEmpty then throw new IllegalArgumentException("Missing IP address")
      if port.trim.isEmpty then throw new IllegalArgumentException("Missing port number")
      new InetSocketAddress(host, port.toInt)


  implicit val inetSocketAddressShow: Show[InetSocketAddress] =
    a => s"${a.getAddress.getHostAddress}:${a.getPort}"
