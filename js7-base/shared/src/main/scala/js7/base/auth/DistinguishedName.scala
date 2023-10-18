package js7.base.auth

import javax.security.auth.x500.X500Principal
import js7.base.convert.As
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import scala.util.{Failure, Success, Try}

final class DistinguishedName(private val x500Principal: X500Principal):

  lazy val string = x500Principal.toString

  override def equals(other: Any) = other match
    case o: DistinguishedName => o.x500Principal == x500Principal
    case _ => false

  override def hashCode = x500Principal.hashCode

  override def toString = string


object DistinguishedName:
  def apply(string: String) = checked(string).orThrow

  def checked(string: String): Checked[DistinguishedName] =
    if string.trim.isEmpty then
      Left(EmptyStringProblem("DistinguishedName"))
    else
      Try(new X500Principal(string)) match
        case Failure(t: IllegalArgumentException) => Left(Problem(s"Invalid Distinguished Name - ${t.getMessage}"))
        case Failure(t) => throw t
        case Success(principal) => Right(new DistinguishedName(principal))

  implicit val GenericStringAsString: As[String, DistinguishedName] =
    As(apply)
