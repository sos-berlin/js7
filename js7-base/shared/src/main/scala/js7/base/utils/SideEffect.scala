package js7.base.utils

object SideEffect:

  implicit final class ImplicitSideEffect[A](private val any: A) extends AnyVal:
    def sideEffect(f: A => Unit): A =
      f(any)
      any
