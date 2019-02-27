package com.sos.jobscheduler.data.workflow.position

import cats.syntax.either.catsSyntaxEither
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import scala.language.implicitConversions

/** Denotes a branch in a instruction, for example fork or if-then-else..
  *
  * @author Joacim Zschimmer
  */
sealed trait BranchId {
  private[position] def toSimpleType: Any

  def normalized: BranchId
}

object BranchId
{
  implicit def apply(branchId: String): Named = Named(branchId)
  implicit def apply(index: Int): Indexed = Indexed(index)

  final case class Named(string: String) extends BranchId {
    def normalized =
      if (string startsWith "try+") "try"
      else if (string startsWith "catch+") "catch"
      else this

    private[position] def toSimpleType: String = string
    override def toString = string
  }
  object Named {
    implicit val jsonEncoder: Encoder[Named] = o => Json.fromString(o.string)
    implicit val jsonDecoder: Decoder[Named] = _.as[String] map Named.apply
  }

  final case class Indexed(number: Int) extends BranchId {
    def normalized = this
    private[position] def toSimpleType: Int = number
    override def toString = number.toString
  }
  object Indexed {
    implicit val jsonEncoder: Encoder[Indexed] = o => Json.fromInt(o.number)
    implicit val jsonDecoder: Decoder[Indexed] = _.as[Int] map Indexed.apply
  }

  implicit val jsonEncoder: Encoder[BranchId] = {
    case o: Named => o.asJson    // String
    case o: Indexed => o.asJson  // Number
  }
  implicit val jsonDecoder: Decoder[BranchId] = cursor =>
    cursor.as[Named]/*String*/ orElse cursor.as[Indexed]/*Number*/
}
