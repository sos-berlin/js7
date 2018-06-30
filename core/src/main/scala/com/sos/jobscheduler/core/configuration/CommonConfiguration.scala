package com.sos.jobscheduler.core.configuration

import cats.syntax.semigroup._
import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkahttp.https.KeyStoreRef
import com.sos.jobscheduler.common.akkahttp.web.data.{WebServerBinding, WebServerPort}
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.internet.IP.StringToServerInetSocketAddress
import com.typesafe.config.Config
import java.net.InetSocketAddress
import java.nio.file.Path
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait CommonConfiguration extends WebServerBinding.HasLocalUris
{
  def config: Config

  def configDirectory: Path

  def webServerPorts: Seq[WebServerPort]

  final lazy val keyStoreRef: Checked[KeyStoreRef] =
    KeyStoreRef.fromConfig(config, default = configDirectory resolve "private/https-keystore.p12")

  final def http: Seq[WebServerBinding.Http] =
    webServerBindings collect { case o: WebServerBinding.Http ⇒ o }

  final def https: Seq[WebServerBinding.Https] =
    webServerBindings collect { case o: WebServerBinding.Https ⇒ o }

  final def webServerBindings: Seq[WebServerBinding] =
    webServerPorts map {
      case WebServerPort.Http(port) ⇒
        WebServerBinding.Http(port)

      case WebServerPort.Https(port, mutual) ⇒
        WebServerBinding.Https(port, keyStoreRef.mapProblem(Problem("HTTPS requires a key store") |+| _).orThrow,
          mutual = mutual)
    }
}

object CommonConfiguration
{
  private val AddressAndMutual: As[String, (InetSocketAddress, Boolean)] =
    string ⇒ (StringToServerInetSocketAddress.apply(string stripSuffix ",mutual"), string endsWith ",mutual")

  def webServerPorts(a: CommandLineArguments): Seq[WebServerPort] =
    a.seqAs("-http-port=")(StringToServerInetSocketAddress).map(WebServerPort.Http) ++
    a.seqAs("-https-port=")(AddressAndMutual).map { case (address, mutual) ⇒ WebServerPort.Https(address, mutual = mutual) }
}
