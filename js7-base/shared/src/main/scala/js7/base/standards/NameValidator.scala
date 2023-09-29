package js7.base.standards

import js7.base.problem.Checked

/**
  * @author Joacim Zschimmer
  */
trait NameValidator:
  def typeName: String

  def checked(name: String): Checked[String]
