package js7.base.generic

import io.circe.{Codec, Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import java.util.Objects.requireNonNull
import javax.annotation.Nullable
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.convert.As
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, CheckedString, Problem}
import js7.base.standards.Js7NameValidator
import js7.base.utils.ScalaUtils.syntax.*

trait GenericString:
  def string: String

  def isEmpty = string.isEmpty

  final def nonEmpty = !isEmpty

  override def toString = string

  def typedToString: String =
    getClass.getSimpleName + ":" + string

object GenericString:
  @Nullable def stringOrNull[A <: GenericString](o: Option[A]): String = o match
    case Some(a) => a.string
    case None => null

  def ordering[A <: GenericString]: Ordering[A] =
    (a, b) => a.string.compareTo(b.string)

  trait Companion[A <: GenericString]:
    def apply(o: String): A

    val name = getClass.shortClassName
    implicit val ordering: Ordering[A] = Ordering.by(_.string)

    implicit def self: Companion[A] = this

  trait Checked_[A <: GenericString] extends Companion[A]:
    final implicit val checkedString: CheckedString[A] = checked(_)

    protected def unchecked(string: String): A

    def checked(string: String): Checked[A] =
      Checked.catchNonFatal(unchecked(requireNonNull(string)))

    final def apply(o: String): A = checked(o).orThrow

    implicit val jsonEncoder: Encoder[A] = o => Json.fromString(o.string)
    implicit val jsonDecoder: Decoder[A] = c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))
    val jsonCodec: Codec[A] = Codec.from(jsonDecoder, jsonEncoder)

    implicit val keyEncoder: KeyEncoder[A] = _.string
    implicit val keyDecoder: KeyDecoder[A] = o => Some(apply(o))  // throws?

    implicit lazy val GenericStringAsString: As[String, A] = As(apply)

    override implicit def self: Checked_[A] = this

  trait NonEmpty[A <: GenericString] extends GenericString.Checked_[A]:
    override def checked(string: String): Checked[A] =
      if string.nonEmpty then super.checked(string)
      else Left(EmptyStringProblem(name))

  trait NameValidating[A <: GenericString] extends Checked_[A]:
    private lazy val nameValidator = new Js7NameValidator(name)

    override def checked(string: String): Checked[A] =
      nameValidator.checked(name = string) flatMap super.checked

  final case class EmptyStringProblem(typeName: String) extends Problem.Coded:
    def arguments = Map("type" -> typeName)
