package com.sos.scheduler.engine.base.generic

/**
  * @author Joacim Zschimmer
  */
final case class SecretString(string: String) {
  override def toString = "SecretString"
}
