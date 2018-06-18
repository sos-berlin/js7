package com.sos.jobscheduler.core.configuration

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.akkahttp.https.KeystoreReference
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.typesafe.config.Config
import java.net.InetSocketAddress
import java.nio.file.Path
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait CommonConfiguration
{
  def config: Config

  def configDirectory: Path

  def webServerPorts: Seq[(WebServerBinding.Scheme, InetSocketAddress)]

  private lazy val keystoreReference: KeystoreReference =
    KeystoreReference.fromConfig(config, configDirectory = configDirectory).orThrow

  final def httpUri: Uri =
    http.headOption.map(o ⇒ Uri(s"http://${o.address.getAddress.getHostAddress}:${o.address.getPort}"))
      .getOrElse(sys.error("No HTTP binding for agentConfiguration.httpUri"))

  final def http: Seq[WebServerBinding.Http] =
    webServerBindings collect { case o: WebServerBinding.Http ⇒ o }

  final def https: Seq[WebServerBinding.Https] =
    webServerBindings collect { case o: WebServerBinding.Https ⇒ o }

  final def webServerBindings: Seq[WebServerBinding] =
    webServerPorts map {
      case (WebServerBinding.Http, port) ⇒ WebServerBinding.Http(port)
      case (WebServerBinding.Https, port) ⇒ WebServerBinding.Https(port, keystoreReference)
    }
}
