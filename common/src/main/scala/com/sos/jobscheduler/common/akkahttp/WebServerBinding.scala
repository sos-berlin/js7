package com.sos.jobscheduler.common.akkahttp

import com.sos.jobscheduler.common.akkahttp.https.KeystoreReference
import java.net.InetSocketAddress

/**
  * @author Joacim Zschimmer
  */
sealed trait WebServerBinding {
  def address: InetSocketAddress
  def scheme: String
  def isHttps: Boolean
}

object WebServerBinding {

  final case class Http(address: InetSocketAddress)
  extends WebServerBinding {
    def scheme = "http"
    def isHttps = false
  }

  final case class Https(
    address: InetSocketAddress,
    keystoreReference: KeystoreReference)
  extends WebServerBinding {
    def scheme = "https"
    def isHttps = true
  }
}
