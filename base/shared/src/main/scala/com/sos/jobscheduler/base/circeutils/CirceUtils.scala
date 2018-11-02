package com.sos.jobscheduler.base.circeutils

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.AnyJsonCodecs.anyToJson
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichEither, RichJavaClass}
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json, JsonNumber, JsonObject, ObjectEncoder, Printer}
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

  def objectCodec[A <: AnyRef: ObjectEncoder: Decoder]: CirceObjectCodec[A] =
    new ObjectEncoder[A] with Decoder[A] {
      def encodeObject(a: A) = implicitly[ObjectEncoder[A]].encodeObject(a)
      def apply(c: HCursor) = implicitly[Decoder[A]].apply(c)
    }

  def circeCodec[A: Encoder: Decoder]: CirceCodec[A] =
    new Encoder[A] with Decoder[A] {
      def apply(a: A) = implicitly[Encoder[A]].apply(a)
      def apply(c: HCursor) = implicitly[Decoder[A]].apply(c)
    }

  val CompactPrinter = Printer.noSpaces.copy(dropNullValues = true/*Suppress None*/, preserveOrder = true/*maybe slow but readable*/)
  val PrettyPrinter = Printer.spaces2.copy(dropNullValues = true/*Suppress None*/, colonLeft = "", lrbracketsEmpty = "")

  object implicits {
    implicit val CompactPrinter = CirceUtils.CompactPrinter
  }

  implicit final class RichJsonObject(private val underlying: JsonObject) extends AnyVal
  {
    def toPrettyString: String =
      PrettyPrinter.pretty(Json.fromJsonObject(underlying))

    def compactPrint: String =
      CompactPrinter.pretty(Json.fromJsonObject(underlying))
  }

  implicit final class RichJson(private val underlying: Json) extends AnyVal
  {
    def toPrettyString: String =
      PrettyPrinter.pretty(underlying)

    def compactPrint: String =
      CompactPrinter.pretty(underlying)

    def intOrThrow: Int = {
      val number = numberOrThrow
      number.toInt match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("Int", number.toString)
      }
    }

    def longOrThrow: Long = {
      val number = numberOrThrow
      number.toLong match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("Long", number.toString)
      }
    }

    def numberOrThrow: JsonNumber =
      underlying.asNumber match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("number", underlying.getClass.simpleScalaName)
      }


    def stringOrThrow: String =
      underlying.asString match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("string", underlying.getClass.simpleScalaName)
      }


    def jsonObjectOrThrow: JsonObject =
      underlying.asObject match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("object", underlying.getClass.simpleScalaName)
      }

    def arrayOrThrow: Vector[Json] =
      underlying.asArray match {
        case Some(o) ⇒ o
        case None ⇒ throwUnexpected("array", underlying.getClass.simpleScalaName)
      }

    def fieldOrThrow(name: String): Json =
      underlying.asObject match {
        case Some(o) ⇒ o(name) getOrElse (throw new IllegalArgumentException(s"Unknown JSON field '$name'"))
        case None ⇒ throw new IllegalArgumentException("Not a JsonObject")
      }
  }

  implicit final class RichCirceString(private val underlying: String) extends AnyVal {
    def parseJson: Json =
      io.circe.parser.parse(underlying).orThrow
  }

  implicit final class CirceUtilsChecked[A](private val underlying: Checked[A]) extends AnyVal {
    def toDecoderResult: Decoder.Result[A] =
      underlying match {
        case Valid(o) ⇒ Right(o)
        case Invalid(o) ⇒ Left(DecodingFailure(o.toString, Nil))  // Ignoring stacktrace ???
      }
  }

  final def deriveCodec[A](implicit encode: Lazy[DerivedObjectEncoder[A]], decode: Lazy[DerivedDecoder[A]]): CirceObjectCodec[A] =
    new ObjectEncoder[A] with Decoder[A] {
      def encodeObject(a: A) = encode.value.encodeObject(a)
      def apply(c: HCursor) = decode.value.tryDecode(c)
    }

  def singletonCodec[A](singleton: A): CirceObjectCodec[A] =
    new ObjectEncoder[A] with Decoder[A] {
      def encodeObject(a: A) = JsonObject.empty

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

  implicit final class JsonStringInterpolator(private val sc: StringContext) extends AnyVal {
    def json(args: Any*): Json = {
      sc.checkLengths(args)
      val p = sc.parts.iterator
      val builder = new StringBuilder(sc.parts.map(_.length).sum + 50)
      builder.append(p.next())
      for (arg ← args) {
        builder.append(toJson(arg))
        builder.append(p.next())
      }
      builder.toString.parseJson
    }

    private def toJson(arg: Any): String =
      arg match {
        case arg: String ⇒
          val j = Json.fromString(arg.toString).toString
          j.substring(1, j.length - 1)  // Interpolation is expected to occur already in quotes: "$var"
        case _ ⇒
          anyToJson(arg).toString
      }

    /** Dummy interpolator returning the string itself, to allow syntax checking by IntelliJ IDEA. */
    def jsonString(args: Any*): String = {
      require(args.isEmpty, "jsonString string interpolator does not accept variables")
      sc.parts mkString ""
    }
  }

  private def throwUnexpected(expected: String, found: String) =
    throw new JsonException(s"JSON $expected expected instead of $found")

  final class JsonException(message: String) extends RuntimeException(message)
}
