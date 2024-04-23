package js7.base.utils

import java.util.regex.Pattern
import js7.base.generic.GenericString

/** This class implements equals (Java's Pattern does not). */
final case class SimplePattern private(pattern: Pattern)
extends GenericString:

  assert(pattern.flags == 0)

  override def equals(other: Any): Boolean =
    other match
      case o: SimplePattern => string == o.string
      case _ => false

  override def hashCode: Int =
    string.hashCode

  def string: String =
    pattern.pattern


object SimplePattern extends GenericString.Checked_[SimplePattern]:
  protected def unchecked(string: String) =
    new SimplePattern(Pattern.compile(string))
