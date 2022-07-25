package js7.base.circeutils

import cats.syntax.show.*
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedAsObjectEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonNumber, JsonObject, ParsingFailure, Printer}
import java.io.{OutputStream, OutputStreamWriter}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.AnyJsonCodecs.anyToJson
import js7.base.data.{ByteArray, ByteSequence, Writable}
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.util.control.NonFatal
import shapeless.Lazy

/**
  * @author Joacim Zschimmer
  */
object CirceUtils
{
  val UnitDecoderResult: Decoder.Result[Unit] = Right(())

  def requireJson(requirement: Boolean, failure: => DecodingFailure): Decoder.Result[Unit] =
    if (!requirement)
      Left(failure)
    else
      UnitDecoderResult

  def parseJsonByteArray(bytes: Array[Byte]): Either[ParsingFailure, Json] =
    CirceUtilsForPlatform.parseJsonByteArray(bytes)

  def parseJsonByteBuffer(bytes: ByteBuffer): Either[ParsingFailure, Json] =
    CirceUtilsForPlatform.parseJsonByteBuffer(bytes)

  def toStringJsonCodec[A](from: String => A): Codec[A] =
    stringJsonCodec(_.toString, from)

  def stringJsonCodec[A](to: A => String, from: String => A): Codec[A] =
    Codec.from(stringDecoder(from), stringEncoder(to))

  def stringEncoder[A](to: A => String): Encoder[A] =
    o => Json.fromString(to(o))

  def stringDecoder[A](from: String => A): Decoder[A] =
    c => c.as[String].flatMap(string =>
      try Right(from(string))
      catch { case NonFatal(t) =>
        Left(DecodingFailure(t.toStringWithCauses, c.history))
      })

  def deriveRenamingCodec[A](rename: Map[String, String])
    (implicit encoder: Lazy[DerivedAsObjectEncoder[A]], decoder: Lazy[DerivedDecoder[A]])
  : Codec.AsObject[A] =
    Codec.AsObject.from(deriveRenamingDecoder[A](rename), encoder.value)

  def deriveRenamingDecoder[A](rename: Map[String, String])
    (implicit decode: Lazy[DerivedDecoder[A]])
  : Decoder[A] = {
    // Precalculate efficient keySet
    val keySet: Set[String] = Set.empty ++ rename.keys
    c => c.as[JsonObject]
      .flatMap(jsonObject =>
        if (!jsonObject.keys.exists(keySet))
          decode.value(c)
        else
          decode.value.decodeJson(
            Json.fromJsonObject(
              JsonObject.fromMap(jsonObject
                .toMap
                .map { case (k, v) => rename.getOrElse(k, k) -> v }))))
  }

  val CompactPrinter = Printer.noSpaces.copy(
    dropNullValues = true/*Suppress None*/,
    //reuseWriters = true,  // Remember StringBuilder in thread local
    predictSize = true)
  val PrettyPrinter = Printer.spaces2.copy(dropNullValues = true/*Suppress None*/, colonLeft = "", lrbracketsEmpty = "")

  object implicits {
    implicit val CompactPrinter = CirceUtils.CompactPrinter
  }

  implicit final class RichCirceObjectCodec[A](private val codec: Codec.AsObject[A]) extends AnyVal {
    def check(check: A => Checked[A]): Codec.AsObject[A] =
      new Codec.AsObject[A] {
        def encodeObject(a: A) =
          codec.encodeObject(a)

        def apply(c: HCursor) =
          codec(c).flatMap(a =>
            check(a) match {
              case Left(problem) => Left(DecodingFailure(problem.toString, c.history))
              case Right(a) => Right(a)
            })
      }
  }

  implicit final class RichCirceError(private val error: io.circe.Error) extends AnyVal
  {
    def toProblem = Problem.pure("JSON " + error.show
      .replace("\r\n", "\\n")
      .replace("\n", "\\n"))
  }

  implicit final class RichCirceEither[R](private val underlying: Either[io.circe.Error, R]) extends AnyVal
  {
    /** Converts to Checked with rendered error message. */
    def toChecked: Checked[R] =
      underlying.left.map(_.toProblem)
  }

  implicit final class RichJsonObject(private val underlying: JsonObject) extends AnyVal
  {
    def ++(o: JsonObject): JsonObject =
      if (o.isEmpty) underlying
      else if (underlying.isEmpty) o
      else JsonObject.fromIterable(underlying.toIterable ++ o.toIterable)

    def toByteArray: ByteArray =
      ByteArray.fromString(compactPrint)

    def compactPrint: String =
      CompactPrinter.print(Json.fromJsonObject(underlying))

    def toPrettyString: String =
      PrettyPrinter.print(Json.fromJsonObject(underlying))
  }

  implicit final class RichJson(private val underlying: Json) extends AnyVal
  {
    def toByteSequence[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq]): ByteSeq =
      ByteSeq.fromString(compactPrint)

    def toByteArray: ByteArray =
      ByteArray.fromString(compactPrint)

    def compactPrint: String =
      CompactPrinter.print(underlying)

    def toPrettyString: String =
      PrettyPrinter.print(underlying)

    def checkedAs[A: Decoder] = underlying.as[A].toChecked

    def intOrThrow: Int = {
      val number = numberOrThrow
      number.toInt match {
        case Some(o) => o
        case None => throwUnexpected("Int", number.toString)
      }
    }

    def longOrThrow: Long = {
      val number = numberOrThrow
      number.toLong match {
        case Some(o) => o
        case None => throwUnexpected("Long", number.toString)
      }
    }

    def numberOrThrow: JsonNumber =
      underlying.asNumber match {
        case Some(o) => o
        case None => throwUnexpected("number", underlying.getClass.simpleScalaName)
      }


    def stringOrThrow: String =
      underlying.asString match {
        case Some(o) => o
        case None => throwUnexpected("string", underlying.getClass.simpleScalaName)
      }


    def jsonObjectOrThrow: JsonObject =
      underlying.asObject match {
        case Some(o) => o
        case None => throwUnexpected("object", underlying.getClass.simpleScalaName)
      }

    def arrayOrThrow: Vector[Json] =
      underlying.asArray match {
        case Some(o) => o
        case None => throwUnexpected("array", underlying.getClass.simpleScalaName)
      }

    def fieldOrThrow(name: String): Json =
      underlying.asObject match {
        case Some(o) => o(name) getOrElse (throw new IllegalArgumentException(s"Unknown JSON field '$name'"))
        case None => throw new IllegalArgumentException("Not a JsonObject")
      }
  }

  implicit final class RichCirceString(private val underlying: String) extends AnyVal {
    def parseJsonAs[A: Decoder]: Checked[A] =
      io.circe.parser.parse(underlying).flatMap(_.as[A]).toChecked

    def parseJson: Checked[Json] =
      io.circe.parser.parse(underlying).toChecked

    def parseJsonOrThrow: Json =
      parseJson.orThrow
  }

  implicit final class CirceUtilsChecked[A](private val underlying: Checked[A]) extends AnyVal {
    def toDecoderResult(history: => List[CursorOp]): Decoder.Result[A] =
      underlying match {
        case Right(o) => Right(o)
        case Left(o) => Left(DecodingFailure(o.toString, history))  // Ignoring stacktrace ???
      }
  }

  implicit final class RichCirceDecoder[A](private val decoder: Decoder[A]) extends AnyVal {
    def checked(check: A => Checked[A]): Decoder[A] =
      new Decoder[A] {
        def apply(c: HCursor) = decoder(c).flatMap(a => check(a).toDecoderResult(c.history))
      }
  }

  implicit final class RichCirceCodec[A](private val codec: Codec.AsObject[A]) extends AnyVal {
    def checked(check: A => Checked[A]): Codec.AsObject[A] =
      new Codec.AsObject[A] {
        def encodeObject(a: A) = codec.encodeObject(a)
        def apply(c: HCursor) = codec(c).flatMap(a => check(a).toDecoderResult(c.history))
      }
  }

  //import io.circe.generic.extras.Configuration.default.withDefaults
  //import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
  //import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
  //private val withDefaultsConfiguration = withDefaults
  //
  //final def deriveCodecWithDefaults[A](implicit encode: Lazy[DerivedAsObjectEncoder[A]], decode: Lazy[DerivedDecoder[A]])
  //: CirceObjectCodec[A] =
  //  new Encoder.AsObject[A] with Decoder[A] {
  //    private val decodeWithDefaults = new ConfiguredDecoder[A](withDefaultsConfiguration) {
  //      todo: apply configuration
  //      override def apply(c: HCursor) = decode.value.apply(c)
  //    }
  //    def encodeObject(a: A) = encode.value.encodeObject(a)
  //    def apply(c: HCursor) = decodeWithDefaults.tryDecode(c)
  //  }

  def singletonCodec[A](singleton: A): Codec.AsObject[A] =
    new Codec.AsObject[A] {
      def encodeObject(a: A) = JsonObject.empty

      def apply(c: HCursor) = Right(singleton)
    }

  def listMapCodec[K: Encoder: Decoder, V: Encoder: Decoder](
    keyName: String = "key",
    valueName: String = "value")
  : Codec[SeqMap[K, V]] =
    new Codec[SeqMap[K, V]] {
      def apply(listMap: SeqMap[K, V]) =
        Json.fromValues(
          for ((key, value) <- listMap) yield
            Json.fromJsonObject(JsonObject.fromMap(
              SeqMap(keyName -> key.asJson) ++
                JsonObject.singleton(valueName, value.asJson).toMap)))

      private implicit val idAndScriptDecoder: Decoder[(K, V)] =
        cursor =>
          for {
            id <- cursor.downField(keyName).as[K]
            value <- cursor.downField(valueName).as[V]
          } yield
            id -> value

      def apply(cursor: HCursor) =
        cursor.as[Seq[(K, V)]].map(SeqMap.empty.++)
    }

  //def delegateCodec[A, B: Encoder: Decoder](toB: A => B, fromB: B => A): Codec[A] =
  //  new Encoder[A] with Decoder[A] {
  //    def apply(a: A) = toB(a).asJson
  //    def apply(c: HCursor) = c.as[B] flatMap caught(fromB)
  //  }
  //
  //private def caught[A, B](convert: A => B)(a: A): Decoder.Result[B] =
  //  try Right(convert(a))
  //  catch { case NonFatal(t) =>
  //    Left(DecodingFailure(t.toStringWithCauses, Nil))
  //  }

  implicit final class JsonStringInterpolator(private val sc: StringContext) extends AnyVal {
    def json(args: Any*): Json =
      JsonStringInterpolator.interpolate(sc, args).parseJsonOrThrow

    /** Dummy interpolator returning the string itself, to allow syntax checking by IntelliJ IDEA. */
    def jsonString(args: Any*): String = {
      require(args.isEmpty, "jsonString string interpolator does not accept variables")
      sc.parts mkString ""
    }
  }

  implicit def jsonWritable[A: Encoder]: Writable[A] =
    new JsonWritable[A]

  private final class JsonWritable[A: Encoder] extends Writable[A]
  {
    def writeToStream(a: A, out: OutputStream) = {
      val w = new OutputStreamWriter(out, UTF_8)
      w.write(a.asJson.compactPrint)
      w.flush()
    }
  }

  object JsonStringInterpolator
  {
    def interpolate(sc: StringContext, args: Seq[Any]): String = {
      StringContext.checkLengths(args, sc.parts)
      val p = sc.parts.iterator
      val builder = new mutable.StringBuilder(sc.parts.map(_.length).sum + 50)
      builder.append(p.next())
      for (arg <- args) {
        builder.append(toJsonString(arg))
        builder.append(p.next())
      }
      builder.toString
    }

    private def toJsonString(arg: Any): String =
      arg match {
        case arg @ (_: String | _: GenericString) =>
          val j = Json.fromString(arg.toString).toString
          j.substring(1, j.length - 1)  // Interpolation is expected to occur already in quotes: "$var"
        case _ =>
          anyToJson(arg, unknownToString = true).toString
      }
  }

  private def throwUnexpected(expected: String, found: String) =
    throw new JsonException(s"JSON $expected expected instead of $found")

  final class JsonException(message: String) extends RuntimeException(message)

  final case class JsonProblem(error: io.circe.Error) extends Problem.Coded {
    def arguments = Map("error" -> error.show.replace("\n", "\\n"))
  }
}
