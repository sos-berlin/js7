package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import java.net.InetSocketAddress

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding {
  def address: InetSocketAddress
  def scheme: String
  def isUnsecuredHttp: Boolean
}

object WebServerBinding {

  final case class Http(address: InetSocketAddress)
  extends WebServerBinding {
    def scheme = "http"
    def isUnsecuredHttp = true
  }

  final case class Https(
    address: InetSocketAddress,
    keystoreReference: KeystoreReference)
  extends WebServerBinding {
    def scheme = "https"
    def isUnsecuredHttp = false
  }
}
