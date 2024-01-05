package js7.common.scalautil

import scala.language.implicitConversions

abstract class ScalaThreadLocal[A]:
  def apply(): A


object ScalaThreadLocal:
  def threadLocal[A](f: => A): ScalaThreadLocal[A] =
    new ScalaThreadLocal[A]:
      private val threadLocal = new ThreadLocal[A]:
        override def initialValue = f

      def apply(): A = threadLocal.get

  implicit def get[A](o: ScalaThreadLocal[A]): A = o.apply()
