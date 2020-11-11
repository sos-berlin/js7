package js7.base.generic

import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import javax.annotation.Nullable
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.convert.As
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, CheckedString, Problem}
import js7.base.standards.NameValidator
import js7.base.utils.ScalaUtils.syntax._

trait GenericString
{
  def string: String

  final def isEmpty = string.isEmpty

  final def nonEmpty = string.nonEmpty

  override def hashCode = string.hashCode + 31 * getClass.hashCode

  override def toString = string
}

object GenericString
{
  @Nullable def stringOrNull[A <: GenericString](o: Option[A]): String = o match {
    case Some(a) => a.string
    case None => null
  }

  trait Companion[A <: GenericString]
  {
    def apply(o: String): A

    val name = getClass.simpleScalaName
    implicit val ordering: Ordering[A] = Ordering.by(_.string)

    implicit def self: Companion[A] = this
  }

  trait Checked_[A <: GenericString] extends Companion[A]
  {
    final implicit val checkedString: CheckedString[A] = checked(_)

    protected def unchecked(string: String): A

    def checked(string: String): Checked[A] = Checked.catchNonFatal(unchecked(string))

    final def apply(o: String): A = checked(o).orThrow

    implicit val jsonEncoder: Encoder[A] = o => Json.fromString(o.string)
    implicit val jsonDecoder: Decoder[A] = c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))
    implicit val keyEncoder: KeyEncoder[A] = _.string
    implicit val keyDecoder: KeyDecoder[A] = o => Some(apply(o))  // throws?

    implicit val GenericStringAsString: As[String, A] = As(apply)

    override implicit def self: Checked_[A] = this
  }

  trait NonEmpty[A <: GenericString] extends GenericString.Checked_[A]
  {
    override def checked(string: String): Checked[A] =
      if (string.nonEmpty) super.checked(string)
      else Left(EmptyStringProblem(name))
  }

  trait NameValidating[A <: GenericString] extends Checked_[A]
  {
    override def checked(string: String): Checked[A] =
      NameValidator.checked(typeName = name, name = string) flatMap super.checked
  }

  final case class EmptyStringProblem(typeName: String) extends Problem.Coded {
    def arguments = Map("type" -> typeName)
  }
}
