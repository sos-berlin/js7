package com.sos.jobscheduler.common.utils

import cats.effect.Resource
import monix.eval.Task
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
object CatsUtils
{
  def autoCloseableToResource[A <: AutoCloseable](newA: ⇒ A): Observable[A] =
    Observable.fromResource(Resource.fromAutoCloseable(Task(newA)))
}
