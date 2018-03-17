package com.sos.jobscheduler.base.monix

import cats.Applicative
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
object MonixForCats  // Monix 3 will have intergrated support for Cats 1.0.1 !!!
{
  implicit val taskApplicative: Applicative[Task] = new Applicative[Task] {
    def pure[A](a: A) = Task.pure(a)
    def ap[A, B](ff: Task[A ⇒ B])(fa: Task[A]) = for (f ← ff; a ← fa) yield f(a)
  }
}
