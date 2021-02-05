package js7.data.workflow.instructions

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.io.process.ReturnCode

/**
  * @author Joacim Zschimmer
  */
sealed trait ReturnCodeMeaning {
  def isSuccess(returnCode: ReturnCode): Boolean
}

object ReturnCodeMeaning {
  val Default: ReturnCodeMeaning = Success(Set(ReturnCode(0)))
  val NoFailure: ReturnCodeMeaning = Failure(Set())

  final case class Success(returnCodes: Set[ReturnCode]) extends ReturnCodeMeaning {
    def isSuccess(returnCode: ReturnCode) = returnCodes contains returnCode
  }
  object Success {
    def of(returnCodes: Int*) = new Success(returnCodes.map(ReturnCode.apply).toSet)
  }

  final case class Failure(returnCodes: Set[ReturnCode]) extends ReturnCodeMeaning {
    def isSuccess(returnCode: ReturnCode) = !returnCodes.contains(returnCode)
  }
  object Failure {
    def of(returnCodes: Int*) = new Failure(returnCodes.map(ReturnCode.apply).toSet)
  }

  implicit val jsonEncoder: Encoder.AsObject[ReturnCodeMeaning] = {
    case Success(o) => JsonObject.singleton("success", o.asJson)
    case Failure(o) => JsonObject.singleton("failure", o.asJson)
  }
  implicit val jsonDecoder: Decoder[ReturnCodeMeaning] = cursor =>
    cursor.get[Set[ReturnCode]]("success").map(Success.apply) orElse
      cursor.get[Set[ReturnCode]]("failure").map(Failure.apply)
}
