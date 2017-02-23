package com.sos.jobscheduler.base.sprayjson.typed

/**
  * @author Joacim Zschimmer
  */
trait CanSerialize[A] {
  def canSerialize(a: A): Boolean
}
