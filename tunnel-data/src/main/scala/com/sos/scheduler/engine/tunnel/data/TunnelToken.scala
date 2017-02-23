package com.sos.scheduler.engine.tunnel.data

import com.sos.scheduler.engine.base.generic.{IsString, SecretString}
import com.sos.scheduler.engine.common.auth.SecretStringGenerator
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

  private[tunnel] def newSecret() = SecretStringGenerator.newSecretString()

  final case class Secret(string: String) extends IsString {
    override def toString = "Secret(...)"
  }

  object Secret extends IsString.HasJsonFormat[Secret]
}
