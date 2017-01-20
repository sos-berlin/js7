package com.sos.scheduler.engine.agent.data.commandresponses

import spray.json._

/**
 * @author Joacim Zschimmer
 */
trait Response

object Response {
  /**
   * Serialization of all Response for a function Command => Response, which returns the unspecific type Response.
   */
  implicit object MyJsonFormat extends RootJsonWriter[Response] {
    def write(response: Response) = response match {
      case o: FileOrderSourceContent ⇒ o.toJson
      case o: LoginResponse ⇒ o.toJson
      case o: StartTaskResponse ⇒ o.toJson
      case EmptyResponse ⇒ EmptyResponse.toJson
      case o ⇒ throw new UnsupportedOperationException(s"Class ${o.getClass.getName} is not serializable as JSON")
    }
  }
}
