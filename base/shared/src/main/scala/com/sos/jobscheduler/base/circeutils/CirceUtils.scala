package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.utils.ScalaUtils.{RichEither, RichJavaClass}
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json, JsonNumber, JsonObject, Printer}
import scala.collection.immutable.{ListMap, Seq}
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

  def listMapCodec[K: Encoder: Decoder, V: Encoder: Decoder](keyName: String = "key", valueName: String = "value"): CirceCodec[ListMap[K, V]] =
    new Encoder[ListMap[K, V]] with Decoder[ListMap[K, V]] {
      def apply(listMap: ListMap[K, V]) =
        Json.fromValues(
          for ((key, value) ← listMap) yield
            Json.fromJsonObject(JsonObject.fromMap(
              ListMap(keyName → key.asJson) ++
                JsonObject.singleton(valueName, value.asJson).toMap)))

      private implicit val idAndScriptDecoder: Decoder[(K, V)] =
        cursor ⇒
          for {
            id ← cursor.downField(keyName).as[K]
            value ← cursor.downField(valueName).as[V]
          } yield
            id → value

      def apply(cursor: HCursor) =
        cursor.as[Seq[(K, V)]].map(ListMap.empty.++)
    }

  //def delegateCodec[A, B: Encoder: Decoder](toB: A ⇒ B, fromB: B ⇒ A): CirceCodec[A] =
  //  new Encoder[A] with Decoder[A] {
  //    def apply(a: A) = toB(a).asJson
  //    def apply(c: HCursor) = c.as[B] flatMap catched(fromB)
  //  }
  //
  //private def catched[A, B](convert: A ⇒ B)(a: A): Decoder.Result[B] =
  //  try Right(convert(a))
  //  catch { case NonFatal(t) ⇒
  //    Left(DecodingFailure(t.toStringWithCauses, Nil))
  //  }

  implicit class JsonStringInterpolator(val sc: StringContext) extends AnyVal {
    def json(args: Any*): Json = {
      require(args.isEmpty, "json string interpolator accepts no variables")
      sc.parts.mkString("").parseJson
    }

    /** Dummy interpolator returning the string itself, to allow syntax checking by IntelliJ IDEA. */
    def jsonString(args: Any*): String = {
      require(args.isEmpty, "jsonString string interpolator accepts no variables")
      sc.parts mkString ""
    }
  }

  private def throwUnexpected(expected: String, found: String) =
    throw new JsonException(s"JSON $expected expected instead of $found")

  final class JsonException(message: String) extends RuntimeException(message)
}
