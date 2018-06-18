package com.sos.jobscheduler.common.akkahttp.web.data

import com.sos.jobscheduler.common.akkahttp.https.KeystoreReference
import java.net.InetSocketAddress

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding {
  def address: InetSocketAddress
  def scheme: WebServerBinding.Scheme
}

object WebServerBinding {

  sealed trait Scheme

  final case class Http(address: InetSocketAddress)
  extends WebServerBinding {
    def scheme = Http

    override def toString = s"WebServerBinding.Http(${address.getAddress.getHostAddress}:${address.getPort})"
  }
  object Http extends Scheme {
    override def toString = "http"
  }

  final case class Https(address: InetSocketAddress, keystoreReference: KeystoreReference)
  extends WebServerBinding {
    def scheme = Https

    override def toString = s"WebServerBinding.Https(${address.getAddress.getHostAddress}:${address.getPort}, $keystoreReference)"
  }
  object Https extends Scheme {
    override def toString = "https"
  }
}
