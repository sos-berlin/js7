package com.sos.scheduler.engine.base.sprayjson.typed

/**
  * @author Joacim Zschimmer
  */
trait CanSerialize[A] {
  def canSerialize(a: A): Boolean
}
