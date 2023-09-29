package js7.data.job

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.io.process.ReturnCode
import js7.base.io.process.ReturnCode.ordinal
import js7.base.utils.RangeSet
import js7.base.utils.typeclasses.IsEmpty

/**
  * @author Joacim Zschimmer
  */
sealed trait ReturnCodeMeaning {
  def isSuccess(returnCode: ReturnCode): Boolean
}

object ReturnCodeMeaning {
  val Default: ReturnCodeMeaning = Success(RangeSet.one(ReturnCode(0)))
  val NoFailure: ReturnCodeMeaning = Failure(RangeSet.empty)

  implicit val ReturnCodeMeaningIsEmpty: IsEmpty[ReturnCodeMeaning] =
    IsEmpty(_ == Default)

  final case class Success(returnCodes: RangeSet[ReturnCode]) extends ReturnCodeMeaning {
    def isSuccess(returnCode: ReturnCode): Boolean =
      returnCodes contains returnCode
  }
  object Success {
    def of(returnCodes: Int*): Success =
      new Success(RangeSet.fromIterable(returnCodes.map(ReturnCode.apply)))
  }

  final case class Failure(returnCodes: RangeSet[ReturnCode]) extends ReturnCodeMeaning {
    def isSuccess(returnCode: ReturnCode): Boolean =
      !returnCodes.contains(returnCode)
  }
  object Failure {
    def of(returnCodes: Int*): Failure =
      new Failure(RangeSet.fromIterable(returnCodes.map(ReturnCode.apply)))
  }

  implicit val jsonEncoder: Encoder.AsObject[ReturnCodeMeaning] = {
    implicit val jsonEncoder_ : Encoder[RangeSet[ReturnCode]] = RangeSet.jsonEncoder[ReturnCode];
    {
      case Success(o) => JsonObject.singleton("success", o.asJson)
      case Failure(o) => JsonObject.singleton("failure", o.asJson)
    }
  }

  implicit val jsonDecoder: Decoder[ReturnCodeMeaning] =
    c => {
      val c1 = c.downField("failure")
      if c1.succeeded then
        c1.as[RangeSet[ReturnCode]].map(Failure.apply)
      else
        c.get[RangeSet[ReturnCode]]("success").map(Success.apply)
    }
}
