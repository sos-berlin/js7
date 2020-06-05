package js7.proxy.javaapi.data

import js7.base.annotation.javaApi

@javaApi
trait JavaWrapper
{
  protected type Underlying

  protected def underlying: Underlying

  override def toString = underlying.toString
}
