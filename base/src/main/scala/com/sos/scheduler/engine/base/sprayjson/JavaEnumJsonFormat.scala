package com.sos.scheduler.engine.base.sprayjson

import scala.collection.breakOut
import scala.reflect.ClassTag
import spray.json.{JsString, JsValue, JsonFormat}

/**
 * @author Joacim Zschimmer
 */
final class JavaEnumJsonFormat[A <: java.lang.Enum[A]](clas: Class[A]) extends JsonFormat[A] {
  private val nameToEnum: Map[String, A] = clas.getEnumConstants.toSeq.map { o ⇒ o.name -> o } (breakOut)

  def write(o: A) = JsString(o.name)

  def read(o: JsValue) = o match {
    case JsString(string) ⇒ nameToEnum(string)
    case _ => sys.error(s"${clas.getSimpleName} name expected instead of ${o.getClass.getSimpleName}")
  }
}

object JavaEnumJsonFormat {
  def apply[A <: java.lang.Enum[A] : ClassTag]() =
    new JavaEnumJsonFormat[A](implicitClass[A])

  private def implicitClass[A : ClassTag]: Class[A] =
    implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
}
