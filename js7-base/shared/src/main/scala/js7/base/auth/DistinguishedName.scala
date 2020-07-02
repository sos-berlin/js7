package js7.base.auth

import javax.naming.NamingException
import javax.naming.ldap.LdapName
import js7.base.convert.As
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import scala.util.{Failure, Success, Try}

final class DistinguishedName(private val ldapName: LdapName)
{
  lazy val string = ldapName.toString

  override def equals(other: Any) = other match {
    case o: DistinguishedName => o.ldapName == ldapName
    case _ => false
  }

  override def hashCode = ldapName.hashCode

  override def toString = string
}

object DistinguishedName
{
  def apply(string: String) = unchecked(string)

  def unchecked(string: String): DistinguishedName =
    checked(string).orThrow

  def checked(string: String): Checked[DistinguishedName] =
    Try(new LdapName(string)) match {
      case Failure(t: NamingException) => Left(Problem(s"Invalid Distinguished Name - ${t.getMessage}"))
      case Failure(t) => throw t
      case Success(ldapName) if ldapName.isEmpty => Left(EmptyStringProblem("DistinguishedName"))
      case Success(ldapName) => Right(new DistinguishedName(ldapName))
    }

  implicit val GenericStringAsString: As[String, DistinguishedName] =
    As(unchecked)
}
