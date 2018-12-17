package com.sos.jobscheduler.base.generic

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.circeutils.CirceUtils.CirceUtilsChecked
import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.standards.NameValidator
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import javax.annotation.Nullable

trait GenericString
{
  def string: String

  final def isEmpty = string.isEmpty

  final def nonEmpty = string.nonEmpty

  final override def hashCode = string.hashCode + 31 * getClass.hashCode

  override def toString = string
}

object GenericString
{
  @Nullable def stringOrNull[A <: GenericString](o: Option[A]): String = o match {
    case Some(a) ⇒ a.string
    case None ⇒ null
  }

  trait Companion[A <: GenericString]
  {
    def apply(o: String): A

    val name = getClass.simpleScalaName
    implicit val ordering: Ordering[A] = Ordering by { _.string }

    implicit def self: Companion[A] = this
  }

  trait Checked_[A <: GenericString] extends Companion[A]
  {
    protected def unchecked(string: String): A

    def checked(string: String): Checked[A] = Checked.catchNonFatal(unchecked(string))

    final def apply(o: String): A = checked(o).orThrow

    implicit val jsonEncoder: Encoder[A] = o ⇒ Json.fromString(o.string)
    implicit val jsonDecoder: Decoder[A] = _.as[String] flatMap (o ⇒ checked(o).toDecoderResult)
    implicit val keyEncoder: KeyEncoder[A] = _.string
    implicit val keyDecoder: KeyDecoder[A] = o ⇒ Some(apply(o))  // throws?

    implicit val GenericStringAsString: As[String, A] = As(apply)

    override implicit def self: Checked_[A] = this
  }

  trait NonEmpty[A <: GenericString] extends GenericString.Checked_[A]
  {
    override def checked(string: String): Checked[A] =
      if (string.nonEmpty) super.checked(string)
      else Invalid(Problem(s"$name must not be empty"))
  }

  trait NameValidating[A <: GenericString] extends Checked_[A]
  {
    override def checked(string: String): Checked[A] =
      NameValidator.checked(string) mapProblem (_ withKey name) flatMap super.checked
  }
}
