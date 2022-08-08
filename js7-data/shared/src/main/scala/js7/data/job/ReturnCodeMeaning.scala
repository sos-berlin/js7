package js7.data.job

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.io.process.ReturnCode
import js7.base.utils.typeclasses.IsEmpty

/**
  * @author Joacim Zschimmer
  */
sealed trait ReturnCodeMeaning {
  def isSuccess(returnCode: ReturnCode): Boolean
}

object ReturnCodeMeaning {
  val Default: ReturnCodeMeaning = Success(Set(ReturnCode(0)))
  val NoFailure: ReturnCodeMeaning = Failure(Set())

  implicit val ReturnCodeMeaningIsEmpty: IsEmpty[ReturnCodeMeaning] =
    IsEmpty(_ == Default)

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
  implicit val jsonDecoder: Decoder[ReturnCodeMeaning] =
    c => {
      val c1 = c.downField("failure")
      if (c1.succeeded)
        c1.as[Set[ReturnCode]].map(Failure.apply)
      else
        c.get[Set[ReturnCode]]("success").map(Success.apply)
    }
}
