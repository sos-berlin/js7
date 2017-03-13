package com.sos.jobscheduler.tunnel.data

import com.sos.jobscheduler.base.generic.{IsString, SecretString}
import com.sos.jobscheduler.common.auth.SecretStringGenerator
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TunnelToken(id: TunnelId, secret: SecretString) {
  override def toString = id.toString
}

object TunnelToken {

  implicit val jsonFormat = {
    implicit val x = SecretString.implicits.jsonFormat
    jsonFormat2(apply)
  }

  def generate(id: TunnelId) = TunnelToken(id, newSecret())

  private[tunnel] def newSecret(): SecretString =
    SecretStringGenerator.newSecretString()

  final case class Secret(string: String) extends IsString {
    override def toString = "Secret(...)"
  }

  object Secret extends IsString.HasJsonFormat[Secret]
}
