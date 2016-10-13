package com.sos.scheduler.engine.common.internet

import com.sos.scheduler.engine.base.convert.As
import com.sos.scheduler.engine.base.convert.As.convert
import com.sos.scheduler.engine.base.utils.ScalaUtils.RichAny
import java.net.{InetAddress, InetSocketAddress}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object IP {

  implicit object StringToInetAddress extends As[String, InetAddress] {
    def apply(o: String) = {
      if (o.isEmpty) throw new IllegalArgumentException("Missing IP number")
      InetAddress.getByName(o)
    }
  }

  def toInetSocketAddress(string: String, defaultHost: String, defaultPort: Option[Int] = None): InetSocketAddress =
    convert[String, InetSocketAddress](string)(new StringToInetSocketAddress(defaultHost, defaultPort))

  val StringToServerInetSocketAddress = new StringToInetSocketAddress(defaultHost = "0.0.0.0", defaultPort = None)

  final class StringToInetSocketAddress(defaultHost: String, defaultPort: Option[Int]) extends As[String, InetSocketAddress] {
    import StringToInetSocketAddress._

    def apply(string: String) =
      try string match {
        case StandardRegex(host, port) ⇒ useDefaults(host, port)
        case IPv6Regex(host, port) ⇒ useDefaults(host, port)
        case IPv6Host(host) ⇒ useDefaults(host, defaultPortString)
        case "" ⇒ makeInetSocketAddress(defaultHost, defaultPortString)
        case _ if string forall { _.isDigit } ⇒ makeInetSocketAddress(defaultHost, string)
        case _ ⇒ makeInetSocketAddress(string, defaultPortString)
      } catch {
        case NonFatal(t) ⇒ throw new IllegalArgumentException(s"Invalid IP address and port combination in '$string': $t", t)
      }

    private def useDefaults(host: String, port: String) =
      makeInetSocketAddress(host.substitute("", defaultHost), port.substitute("", defaultPort.toString))

    private def defaultPortString = defaultPort map { _.toString } getOrElse ""
  }

  implicit object StringToInetSocketAddress extends As[String, InetSocketAddress] {
    private val StandardRegex = """([^:]*):(\d*)""".r
    private val IPv6Regex = """\[(.*)\]:(\d*)""".r
    private val IPv6Host = """\[(.*)\]""".r

    def apply(string: String) =
      string match {
        case StandardRegex(host, port) ⇒ makeInetSocketAddress(host, port)
        case IPv6Regex(host, port) ⇒ makeInetSocketAddress(host, port)
        case _ ⇒ throw new IllegalArgumentException(s"ipAddress:port or [ip6Address]:port expected")
      }

    private def makeInetSocketAddress(host: String, port: String) = {
      if (host.trim.isEmpty) throw new IllegalArgumentException("Missing IP address")
      if (port.trim.isEmpty) throw new IllegalArgumentException("Missing port number")
      new InetSocketAddress(host, port.toInt)
    }
  }
}
