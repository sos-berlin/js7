package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import java.net.InetSocketAddress

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding {
  def address: InetSocketAddress
  def scheme: String
}

object WebServerBinding {

  case class Http(address: InetSocketAddress)
  extends WebServerBinding {
    def scheme = "http"
  }

  final case class Https(
    address: InetSocketAddress,
    keystoreReference: KeystoreReference)
  extends WebServerBinding {
    def scheme = "https"
  }
}
