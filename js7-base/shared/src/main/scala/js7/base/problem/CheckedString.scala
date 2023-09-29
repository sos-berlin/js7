package js7.base.problem

/**
  * @author Joacim Zschimmer
  */
trait CheckedString[A]:
  def checked(string: String): Checked[A]
