package com.sos.scheduler.engine.common.time.timer

import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
trait PromiseFuture[A] extends DelegatedFuture[A] {

  protected final def delegatedFuture = promise.future

  protected def promise: Promise[A]
}

