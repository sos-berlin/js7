package js7.proxy.javaapi.data.common

import js7.base.annotation.javaApi

@javaApi
trait JavaWrapper
{
  protected type AsScala

  def asScala: AsScala

  override def toString = asScala.toString
}
