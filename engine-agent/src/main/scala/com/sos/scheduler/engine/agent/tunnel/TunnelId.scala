package com.sos.scheduler.engine.agent.tunnel

import com.sos.scheduler.engine.data.base.IsString
import java.util.Base64
import scala.util.Random
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TunnelId(string: String) extends IsString

object TunnelId extends IsString.HasJsonFormat[TunnelId] {

  private val PasswordLength = 20

  final case class WithPassword(id: TunnelId, password: Password)

  object WithPassword {
    implicit val MyJsonFormat = jsonFormat2(apply)
  }

  private[tunnel] def newPassword() = Password({
    val bytes = new Array[Byte](PasswordLength)
    Random.nextBytes(bytes)
    Base64.getUrlEncoder.encodeToString(bytes)
  })

  final case class Password(string: String) extends IsString {
    override def toString = "Password(...)"
  }

  object Password extends IsString.HasJsonFormat[Password]
}
