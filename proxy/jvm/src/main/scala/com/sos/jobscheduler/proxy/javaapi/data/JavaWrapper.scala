package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi

@javaApi
trait JavaWrapper
{
  protected type Underlying

  protected def underlying: Underlying

  override def toString = underlying.toString
}
