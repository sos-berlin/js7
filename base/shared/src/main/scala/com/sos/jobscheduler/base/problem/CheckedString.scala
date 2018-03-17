package com.sos.jobscheduler.base.problem

/**
  * @author Joacim Zschimmer
  */
trait CheckedString[A] {
  def checked(string: String): Checked[A]
}
