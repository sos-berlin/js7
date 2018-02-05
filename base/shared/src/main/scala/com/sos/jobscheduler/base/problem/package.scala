package com.sos.jobscheduler.base

import cats.data.Validated

/**
  * @author Joacim Zschimmer
  */
package object problem
{
  type Checked[A] = Validated[Problem, A]
}
