package js7.base.generic

import io.circe.{Codec, Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import java.time.ZoneId
import java.util.Objects.requireNonNull
import javax.annotation.Nullable
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.convert.As
import js7.base.problem.Checked.{Ops, catchNonFatal}
import js7.base.problem.{Checked, CheckedString, Problem}
import js7.base.standards.Js7NameValidator
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly

trait GenericString:

  def string: String

  def isEmpty: Boolean = string.isEmpty

  final def nonEmpty: Boolean = !isEmpty

  override def toString: String = string

  def typedToString: String =
    getClass.getSimpleName + ":" + string


object GenericString:
  @Nullable def stringOrNull[A <: GenericString](o: Option[A]): String | Null =
    o match
      case Some(a) => a.string
      case None => null

  def ordering[A <: GenericString]: Ordering[A] =
    (a, b) => a.string.compareTo(b.string)

  trait Companion[A <: GenericString]:
    val name: String = getClass.shortClassName
    implicit val ordering: Ordering[A] = Ordering.by(_.string)

    implicit def self: Companion[A] = this

    def apply(o: String): A


  trait Checked_[A <: GenericString] extends Companion[A]:
    final implicit val checkedString: CheckedString[A] = checked(_)

    protected def unchecked(string: String): A
    protected def isReserved: String => Boolean =
      _ => false

    def checked(string: String): Checked[A] =
      if isReserved(string) then
        Left(Problem(s"$name:$string is a reserved name"))
      else
        catchNonFatal:
          unchecked(requireNonNull(string))

    @TestOnly @throws[RuntimeException]
    final def apply(string: String): A =
      mayThrow(string)

    @TestOnly @throws[RuntimeException]
    final def fromZoneId(zoneId: ZoneId): A =
      mayThrow(zoneId.toString)

    /** @throws RuntimeException. */
    @throws[RuntimeException]
    final def mayThrow(string: String): A =
      checked(string).orThrow

    implicit val jsonEncoder: Encoder[A] = o =>
      if isReserved(o.string) then
        throw new IllegalArgumentException(s"$o is a reserved name and cannot be JSON-serialized")
      else
        Json.fromString(o.string)

    implicit val jsonDecoder: Decoder[A] = c =>
      c.as[String].flatMap: o =>
        checked(o).toDecoderResult(c.history)

    val jsonCodec: Codec[A] =
      Codec.from(jsonDecoder, jsonEncoder)

    implicit val keyEncoder: KeyEncoder[A] = _.string

    implicit val keyDecoder: KeyDecoder[A] = o => Some(apply(o))  // throws?

    implicit lazy val GenericStringAsString: As[String, A] = As(apply)

    override implicit def self: Checked_[A] = this


  trait NonEmpty[A <: GenericString] extends GenericString.Checked_[A]:
    override def checked(string: String): Checked[A] =
      if string.isEmpty then
        Left(EmptyStringProblem(name))
      else
        super.checked(string)


  trait NameValidating[A <: GenericString] extends Checked_[A]:
    private lazy val nameValidator = new Js7NameValidator(name)

    override def checked(string: String): Checked[A] =
      nameValidator.checked(name = string) flatMap super.checked

  final case class EmptyStringProblem(typeName: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "type" -> typeName)
