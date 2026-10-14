package js7.base.utils

import java.util.Objects.requireNonNull
import scala.quoted.{Expr, Quotes, Type}

object JavaUtils:

  /** Java Function&lt;A, B>. */
  type -->[A, B] = java.util.function.Function[A, B]

  val Void: Void = null.asInstanceOf[Void]


  /** 🔥 Seems not to work properly. */
  @throws[ClassCastException]
  @throws[NullPointerException]
  inline def assertTypeForJava[A](inline a: A): Unit =
    ${ JavaUtilMacros.assertTypeForJava[A]('a) }


private object JavaUtilMacros:

  def assertTypeForJava[A: Type](a: Expr[A])(using Quotes): Expr[Unit] =
    '{
      val x = $a
      if !x.isInstanceOf[A] then
        requireNonNull(x) // In JavaUtilTest, this throws a proper ClassCastException
        throw new ClassCastException // But in JLogFileTest this throws
    }
