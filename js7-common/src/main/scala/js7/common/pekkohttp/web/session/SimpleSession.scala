package js7.common.pekkohttp.web.session

import io.circe.syntax.EncoderOps
import io.circe.{Encoder, JsonObject}
import js7.base.time.Timestamp
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
final case class SimpleSession(sessionInit: SessionInit) extends Session:
  override def toString: String = sessionToken.toString


object SimpleSession:

  given jsonEncoder: Encoder.AsObject[SimpleSession] = o =>
    given Encoder[Timestamp] = Timestamp.StringTimestampJsonEncoder
    val lastUsed = o.lastUsed
    JsonObject(
      "number" -> o.sessionInit.sessionToken.number.asJson,
      "userId" -> o.sessionInit.loginUser.id.asJson,
      "source" -> o.sessionInit.source.asJson,
      "lastUsedFor" -> lastUsed.what.asJson,
      "lastUsedAt" -> lastUsed.timestamp.asJson,
      "loggedInAt" -> o.sessionInit.loggedInAt.asJson,
      "timeoutAt" ->
        o.sessionInit.timeoutAt.map: timeoutAt =>
          Timestamp.now + (timeoutAt - Deadline.now)
        .asJson)
