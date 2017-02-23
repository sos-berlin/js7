package com.sos.jobscheduler.common.scalautil

object SideEffect {
  implicit class ImplicitSideEffect[A](val any: A) extends AnyVal {
    def sideEffect(f: A => Unit) = {
      f(any)
      any
    }
  }
}
