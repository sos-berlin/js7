package js7.base.io.process

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

final case class KeyLogin(credentialKey: String, withUserProfile: Boolean)

object KeyLogin:
  implicit val jsonCodec: Codec.AsObject[KeyLogin] = deriveCodec[KeyLogin]
