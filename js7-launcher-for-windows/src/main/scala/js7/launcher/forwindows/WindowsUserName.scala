package js7.launcher.forwindows

import java.util.Locale
import js7.base.generic.GenericString

final case class WindowsUserName(string: String) extends GenericString:
  private lazy val normalized = string.toLowerCase(Locale.ROOT)

  override def equals(o: Any): Boolean = o match
    case o: WindowsUserName => normalized == o.normalized
    case _ => false

  override def hashCode: Int = normalized.hashCode

  def domain: Option[String] =
    string.indexOf('@') match
      case -1 => None
      case i => Some(string.substring(i + 1))

  def withoutDomain: String =
    string.indexOf('@') match
      case -1 => string
      case i => string.substring(0, i)
