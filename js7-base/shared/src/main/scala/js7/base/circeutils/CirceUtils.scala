package js7.base.circeutils

import cats.syntax.show.*
import io.circe
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, Configuration as CirceConfiguration}
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonNumber, JsonObject, ParsingFailure, Printer}
import java.io.{File, OutputStream, OutputStreamWriter}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import js7.base.circeutils.AnyJsonCodecs.anyToJson
import js7.base.data.{ByteArray, ByteSequence, Writable}
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StringInterpolators
import scala.collection.immutable.SeqMap
import scala.collection.immutable.Map.Map1
import scala.collection.mutable
import scala.deriving.Mirror
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object CirceUtils:
  val DecoderOK: Decoder.Result[Unit] = Right(())

  private implicit val circeConfiguration: CirceConfiguration =
    CirceConfiguration.default.withDefaults

  def requireJson(requirement: Boolean, failure: => DecodingFailure): Decoder.Result[Unit] =
    if !requirement then
      Left(failure)
    else
      DecoderOK

  def parseJsonByteArray(bytes: Array[Byte]): Either[ParsingFailure, Json] =
    CirceUtilsForPlatform.parseJsonByteArray(bytes)

  def parseJsonByteBuffer(bytes: ByteBuffer): Either[ParsingFailure, Json] =
    CirceUtilsForPlatform.parseJsonByteBuffer(bytes)

  def toStringJsonCodec[A](from: String => A): Codec[A] =
    stringJsonCodec(_.toString, from)

  def stringJsonCodec[A](to: A => String, from: String => A): Codec[A] =
    Codec.from(stringDecoder(from), stringEncoder(to))

  private def stringEncoder[A](to: A => String): Encoder[A] =
    o => Json.fromString(to(o))

  def stringDecoder[A](from: String => A): Decoder[A] =
    c => c.as[String].flatMap(string =>
      try Right(from(string))
      catch { case NonFatal(t) =>
        Left(DecodingFailure(t.toStringWithCauses, c.history))
      })

  inline final def deriveConfiguredCodec[A: Mirror.Of]: ConfiguredCodec[A] =
    deriveCodecWithDefaults[A]

  inline final def deriveCodecWithDefaults[A: Mirror.Of]: ConfiguredCodec[A] =
    ConfiguredCodec.derive[A](useDefaults = true)

  inline final def deriveConfiguredDecoder[A: Mirror.Of]: ConfiguredDecoder[A] =
    ConfiguredDecoder.derive[A](useDefaults = true)

  inline final def deriveRenamingCodec[A: Mirror.Of](rename: Map[String, String])
  : Codec.AsObject[A] =
    Codec.AsObject.from(deriveRenamingDecoder[A](rename), deriveCodec[A])
    //ConfiguredCodec.derive[A](
    //  transformMemberNames = name => rename.getOrElse(name, name),
    //  useDefaults = true)

  inline def deriveRenamingDecoder[A](rename: Map[String, String])(using inline A: Mirror.Of[A])
  : Decoder[A] =
    val decode = Decoder.derived[A]
    // Precalculate keySet
    val keySet: Set[String] = rename.keySet
    c => c
      .as[JsonObject]
      .flatMap(jsonObject =>
        if !jsonObject.keys.exists(keySet) then
          decode(c)
        else
          decode.decodeJson(
            Json.fromJsonObject(
              JsonObject.fromMap(jsonObject
                .toMap
                .map { case (k, v) => rename.getOrElse(k, k) -> v }))))

  val CompactPrinter: Printer = Printer.noSpaces.copy(
    dropNullValues = true/*Suppress None*/,
    //reuseWriters = true,  // Remember StringBuilder in thread local
    predictSize = true)

  val PrettyPrinter: Printer = Printer.spaces2.copy(
    dropNullValues = true/*Suppress None*/,
    colonLeft = "",
    lrbracketsEmpty = "")

  object implicits:
    implicit val CompactPrinter: Printer =
      CirceUtils.CompactPrinter

  implicit final class RichCirceError(private val error: io.circe.Error) extends AnyVal:
    def toProblem: Problem =
      Problem.pure("JSON " + error.show
        .replace("\r\n", "\\n")
        .replace("\n", "\\n"))

  implicit final class RichCirceEither[R](private val underlying: Either[io.circe.Error, R]) extends AnyVal:
    /** Converts to Checked with rendered error message. */
    def toChecked: Checked[R] =
      underlying.left.map(_.toProblem)

  implicit final class RichJsonObject(private val underlying: JsonObject) extends AnyVal:
    def ++(o: JsonObject): JsonObject =
      if o.isEmpty then underlying
      else if underlying.isEmpty then o
      else JsonObject.fromIterable(underlying.toIterable ++ o.toIterable)

    def toByteArray: ByteArray =
      ByteArray.fromString(compactPrint)

    def compactPrint: String =
      CompactPrinter.print(Json.fromJsonObject(underlying))

    def toPrettyString: String =
      PrettyPrinter.print(Json.fromJsonObject(underlying))

  implicit final class RichJson(private val underlying: Json) extends AnyVal:
    def toByteSequence[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq]): ByteSeq =
      ByteSeq.fromString(compactPrint)

    def toByteArray: ByteArray =
      ByteArray.fromString(compactPrint)

    def compactPrint: String =
      CompactPrinter.print(underlying)

    def toPrettyString: String =
      PrettyPrinter.print(underlying)

    def checkedAs[A: Decoder]: Checked[A] =
      underlying.as[A].toChecked

    def intOrThrow: Int =
      val number = numberOrThrow
      number.toInt match
        case Some(o) => o
        case None => throwUnexpected("Int", number.toString)

    def longOrThrow: Long =
      val number = numberOrThrow
      number.toLong match
        case Some(o) => o
        case None => throwUnexpected("Long", number.toString)

    def numberOrThrow: JsonNumber =
      underlying.asNumber match
        case Some(o) => o
        case None => throwUnexpected("number", underlying.getClass.simpleScalaName)


    def stringOrThrow: String =
      underlying.asString match
        case Some(o) => o
        case None => throwUnexpected("string", underlying.getClass.simpleScalaName)


    def jsonObjectOrThrow: JsonObject =
      underlying.asObject match
        case Some(o) => o
        case None => throwUnexpected("object", underlying.getClass.simpleScalaName)

    def arrayOrThrow: Vector[Json] =
      underlying.asArray match
        case Some(o) => o
        case None => throwUnexpected("array", underlying.getClass.simpleScalaName)

    def fieldOrThrow(name: String): Json =
      underlying.asObject match
        case Some(o) => o(name) getOrElse (throw new IllegalArgumentException(s"Unknown JSON field '$name'"))
        case None => throw new IllegalArgumentException("Not a JsonObject")

  implicit final class RichCirceString(private val underlying: String) extends AnyVal:
    def parseJsonAs[A: Decoder]: Checked[A] =
      io.circe.parser.parse(underlying).flatMap(_.as[A]).toChecked

    def parseJson: Checked[Json] =
      io.circe.parser.parse(underlying).toChecked

    def parseJsonOrThrow: Json =
      parseJson.orThrow

  implicit final class CirceUtilsChecked[A](private val underlying: Checked[A]) extends AnyVal:
    def toDecoderResult(history: => List[CursorOp]): Decoder.Result[A] =
      CirceUtils.toDecoderResult(history)(underlying)

  def toDecoderResult[A](history: => List[CursorOp])(checked: Checked[A]): Decoder.Result[A] =
    checked match
      case Right(o) => Right(o)
      case Left(o) => Left(DecodingFailure(o.toString, history)) // Ignoring stacktrace ???

  implicit final class RichCirceDecoder[A](private val decoder: Decoder[A]) extends AnyVal:
    def checked(check: A => Checked[A]): Decoder[A] =
      new Decoder[A]:
        def apply(c: HCursor) = decoder(c).flatMap(a => check(a).toDecoderResult(c.history))

  implicit final class RichCirceObjectCodec[A](private val codec: Codec.AsObject[A]) extends AnyVal:
    def checked(check: A => Checked[A]): Codec.AsObject[A] =
      new Codec.AsObject[A]:
        def encodeObject(a: A) = codec.encodeObject(a)
        def apply(c: HCursor) = codec(c).flatMap(a => check(a).toDecoderResult(c.history))

  def singletonCodec[A](singleton: A): Codec.AsObject[A] =
    new Codec.AsObject[A]:
      def encodeObject(a: A) = JsonObject.empty

      def apply(c: HCursor) = Right(singleton)

  def listMapCodec[K: Encoder: Decoder, V: Encoder: Decoder](
    keyName: String = "key",
    valueName: String = "value")
  : Codec[SeqMap[K, V]] =
    new Codec[SeqMap[K, V]]:
      def apply(listMap: SeqMap[K, V]) =
        Json.fromValues(
          for (key, value) <- listMap yield
            Json.fromJsonObject(JsonObject.fromMap(
              SeqMap(keyName -> key.asJson) ++
                JsonObject.singleton(valueName, value.asJson).toMap)))

      private implicit val idAndScriptDecoder: Decoder[(K, V)] =
        cursor =>
          for
            id <- cursor.downField(keyName).as[K]
            value <- cursor.downField(valueName).as[V]
          yield
            id -> value

      def apply(cursor: HCursor) =
        cursor.as[Seq[(K, V)]].map(SeqMap.empty.++)

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

  implicit final class JsonStringInterpolator(private val sc: StringContext) extends AnyVal:
    def json(args: Any*): Json =
      StringInterpolators
        .interpolate(sc, args, JsonStringInterpolator.toJsonString)
        .parseJsonOrThrow

    /** Dummy interpolator returning the string itself, to allow syntax checking by IntelliJ IDEA. */
    def jsonString(args: Any*): String =
      require(args.isEmpty, "jsonString string interpolator does not accept variables")
      sc.parts mkString ""

  implicit def jsonWritable[A: Encoder]: Writable[A] =
    new JsonWritable[A]

  private final class JsonWritable[A: Encoder] extends Writable[A]:
    def writeToStream(a: A, out: OutputStream): Unit =
      val w = new OutputStreamWriter(out, UTF_8)
      w.write(a.asJson.compactPrint)
      w.flush()

  object JsonStringInterpolator:
    def interpolate(sc: StringContext, args: Seq[Any]): String =
      StringContext.checkLengths(args, sc.parts)
      val p = sc.parts.iterator
      val builder = new mutable.StringBuilder(sc.parts.map(_.length).sum + 50)
      builder.append(p.next())
      for arg <- args do
        builder.append(toJsonString(arg))
        builder.append(p.next())
      builder.toString

    private def toJsonString(arg: Any): String =
      arg match
        case arg @ (_: String | _: GenericString | _: Path | _: File) =>
          val str = arg match
            case o: GenericString => o.string
            case o => o.toString
          val j = Json.fromString(str).toString
          // Strip quotes. Interpolation is expected to occur already in quotes: "$var"
          j.substring(1, j.length - 1)
        case _ =>
          anyToJson(arg, unknownToString = true).toString

  def reparseJson[A](a: A, codec: Codec[A]): Either[circe.Error, A] =
    reparseJson(a)(codec, codec)

  def reparseJson[A](a: A)(implicit decoder: Decoder[A], encoder: Encoder[A])
  : Either[circe.Error, A] =
    parseJson(encoder(a).compactPrint)
      .flatMap(decoder.decodeJson)

  def parseJson(string: String): Either[ParsingFailure, Json] =
    io.circe.parser.parse(string)

  private def throwUnexpected(expected: String, found: String): Nothing =
    throw new JsonException(s"JSON $expected expected instead of $found")

  final class JsonException(message: String) extends RuntimeException(message)

  final case class JsonProblem(error: io.circe.Error) extends Problem.Coded:
    def arguments: Map[String, String] =
      Map1("error", error.show.replace("\n", "\\n"))
