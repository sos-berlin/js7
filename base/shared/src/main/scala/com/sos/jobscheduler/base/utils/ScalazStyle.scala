package com.sos.jobscheduler.base.utils

/**
 * Methods like in Scalaz.
 *
 * @author Joacim Zschimmer
 *
 * @see https://github.com/scalaz/scalaz
 */
object ScalazStyle {

  implicit final class OptionRichBoolean(private val delegate: Boolean) extends AnyVal {

    /**
      * Conditional `Option`.
      * <p>`(true ? a) == Some(a)`
      * <br>`(false ? a) == None`
      */
    def ?[A](a: ⇒ A): Option[A] = option(a)

    /**
      * Conditional `Option`.
      * <p>`(true option a) == Some(a)`
      * <br>`(false option a) == None`
      */
    def option[A](a: ⇒ A): Option[A] = if (delegate) Some(a) else None

    /**
      * Conditional `List`.
      * <p>`(true option a) == List(a)`
      * <br>`(false option a) == Nil`
      */
    def list[A](a: ⇒ A): List[A] = if (delegate) List(a) else Nil

    /**
      * Conditional `Vector`.
      * <p>`(true option a) == Vector(a)`
      * <br>`(false option a) == Vector()`
      */
    def vector[A](a: ⇒ A): Vector[A] = if (delegate) Vector(a) else Vector()

    /**
      * Conditional `Set`.
      * <p>`(true option a) == Set(a)`
      * <br>`(false option a) == Set()`
      */
    def set[A](a: ⇒ A): Set[A] = if (delegate) Set(a) else Set()
  }
}
