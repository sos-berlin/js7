package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicitClass
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object Exceptions {

  def ignoreException(log: (⇒ String, Throwable) ⇒ Unit)(body: ⇒ Unit): Unit = ignoreNonFatal[Throwable](log)(body)

  def ignoreNonFatal[T <: Throwable: ClassTag](log: (⇒ String, Throwable) ⇒ Unit)(body: ⇒ Unit): Unit =
    try body
    catch {
      case NonFatal(t) if implicitClass[T] isAssignableFrom t.getClass ⇒ log(s"Ignoring exception $t", t)
    }

  def toStringWithCauses(throwable: Throwable): String = {
    val throwables = mutable.Buffer[Throwable]()
    var t = throwable
    while (t != null) {
      throwables += t
      t = t.getCause
    }
    throwables mkString ", caused by "
  }
}
