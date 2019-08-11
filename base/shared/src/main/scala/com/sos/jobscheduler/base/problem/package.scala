package com.sos.jobscheduler.base

/**
  * @author Joacim Zschimmer
  */
package object problem
{
  type Checked[A] = Either[Problem, A]
}
