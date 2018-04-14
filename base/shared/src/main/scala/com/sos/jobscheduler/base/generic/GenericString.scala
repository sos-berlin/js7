package com.sos.jobscheduler.base.generic

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import javax.annotation.Nullable
import scala.language.implicitConversions

trait GenericString {
  def string: String

  final def isEmpty = string.isEmpty

  final def nonEmpty = string.nonEmpty

  final override def hashCode = string.hashCode ^ getClass.hashCode

  override def toString = string
}

object GenericString {

  @Nullable def stringOrNull[A <: GenericString](o: Option[A]): String = o match {
    case Some(a) ⇒ a.string
    case None ⇒ null
  }

  private implicit def jsonEncoder[A <: GenericString]: Encoder[A] =
    o ⇒ Json.fromString(o.string)

  trait HasJsonCodec[A <: GenericString] {
    def apply(o: String): A

    implicit val jsonEncoder: Encoder[A] = GenericString.jsonEncoder[A]
    implicit val jsonDecoder: Decoder[A] = _.as[String] map apply
    implicit val keyEncoder: KeyEncoder[A] = _.string
    implicit val keyDecoder: KeyDecoder[A] = o ⇒ Some(apply(o))
  }

  trait Companion[A <: GenericString] extends HasJsonCodec[A] {
    val name = getClass.simpleScalaName
    implicit val ordering: Ordering[A] = Ordering by { _.string }
    implicit val GenericStringAsString: As[String, A] = As(apply)

    implicit def self: Companion[A] = this
  }
}
