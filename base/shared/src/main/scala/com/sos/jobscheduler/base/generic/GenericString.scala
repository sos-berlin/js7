package com.sos.jobscheduler.base.generic

import com.sos.jobscheduler.base.circeutils.CirceUtils.CirceUtilsChecked
import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.standards.NameValidator
import com.sos.jobscheduler.base.utils.HasTypeInfo
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
    case Some(a) => a.string
    case None => null
  }

  trait Companion[A <: GenericString]
  {
    def apply(o: String): A

    val name = getClass.simpleScalaName
    implicit val ordering: Ordering[A] = Ordering by { _.string }

    implicit def self: Companion[A] = this

    implicit val hasTypeInfo = HasTypeInfo[A](getClass.simpleScalaName)
  }

  trait Checked_[A <: GenericString] extends Companion[A]
  {
    protected def unchecked(string: String): A

    def checked(string: String): Checked[A] = Checked.catchNonFatal(unchecked(string))

    final def apply(o: String): A = checked(o).orThrow

    implicit val jsonEncoder: Encoder[A] = o => Json.fromString(o.string)
    implicit val jsonDecoder: Decoder[A] = c => c.as[String] flatMap (o => checked(o).toDecoderResult(c.history))
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
