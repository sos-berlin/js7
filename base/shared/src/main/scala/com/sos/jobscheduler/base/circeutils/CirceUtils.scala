package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.utils.ScalaUtils.{RichEither, RichJavaClass}
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import io.circe.{Decoder, Encoder, HCursor, Json, JsonNumber, JsonObject, Printer}
import shapeless.Lazy

/**
  * @author Joacim Zschimmer
  */
object CirceUtils {

  def toStringJsonCodec[A](from: String ⇒ A): CirceCodec[A] =
    stringJsonCodec(_.toString, from)

  def stringJsonCodec[A](to: A ⇒ String, from: String ⇒ A): CirceCodec[A] =
    circeCodec(stringEncoder(to), stringDecoder(from))

  def stringEncoder[A](to: A ⇒ String): Encoder[A] =
    o ⇒ Json.fromString(to(o))

  def stringDecoder[A](from: String ⇒ A): Decoder[A] =
    _.as[String] map from

  def circeCodec[A: Encoder: Decoder]: CirceCodec[A] =
    new Encoder[A] with Decoder[A] {
      def apply(a: A) = implicitly[Encoder[A]].apply(a)
      def apply(c: HCursor) = implicitly[Decoder[A]].apply(c)
    }

  val CompactPrinter = Printer.noSpaces.copy(dropNullKeys = true/*Suppress None*/, preserveOrder = true/*maybe slow but readable*/)
  val PrettyPrinter = Printer.spaces2.copy(dropNullKeys = true/*Suppress None*/, colonLeft = "", lrbracketsEmpty = "")

  object implicits {
    implicit val CompactPrinter = CirceUtils.CompactPrinter
  }

  implicit class RichJson(val underlying: Json) extends AnyVal {

    def toPrettyString: String =
      PrettyPrinter.pretty(underlying)

    def compactPrint: String =
      CompactPrinter.pretty(underlying)

    def forceInt: Int = {
      val number = forceNumber
      number.toInt match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("Int", number.toString)
      }
    }

    def forceLong: Long = {
      val number = forceNumber
      number.toLong match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("Long", number.toString)
      }
    }

    def forceNumber: JsonNumber =
      underlying.asNumber match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("number", underlying.getClass.simpleScalaName)
      }


    def forceString: String =
      underlying.asString match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("string", underlying.getClass.simpleScalaName)
      }


    def forceObject: JsonObject =
      underlying.asObject match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("object", underlying.getClass.simpleScalaName)
      }

    def forceArray: Vector[Json] =
      underlying.asArray match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("array", underlying.getClass.simpleScalaName)
      }
  }

  implicit class RichCirceString(val underlying: String) extends AnyVal {
    def parseJson: Json =
      io.circe.parser.parse(underlying).force
  }

  final def deriveCirceCodec[A](implicit encode: Lazy[DerivedObjectEncoder[A]], decode: Lazy[DerivedDecoder[A]]): CirceCodec[A] =
    new Encoder[A] with Decoder[A] {
      def apply(a: A) = encode.value.apply(a)
      def apply(c: HCursor) = decode.value.tryDecode(c)
    }

  def objectCodec[A](singleton: A): CirceCodec[A] =
    new Encoder[A] with Decoder[A] {
      private val empty = Json.fromJsonObject(JsonObject.empty)

      def apply(a: A) = empty

      def apply(c: HCursor) = Right(singleton)
    }

  private def throwUnexpected(expected: String, found: String) =
    throw new JsonException(s"JSON $expected expected instead of $found")

  final class JsonException(message: String) extends RuntimeException(message)
}
