package js7.data_for_java.common

import js7.base.annotation.javaApi

@javaApi
trait JavaWrapper:
  type AsScala

  def asScala: AsScala

  override def toString: String =
    asScala.toString
