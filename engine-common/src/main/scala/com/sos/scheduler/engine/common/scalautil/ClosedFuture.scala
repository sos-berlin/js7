package com.sos.scheduler.engine.common.scalautil

import scala.concurrent.Promise

/**
 * Provides a Future `closed`, which succeeds with `close`.
 * <p>
 * <b>Should be one of the first mixins</b> after `HasCloser`, for `closed` to succeed when `Closer` has anything closed.
 * <pre>
 *    class X extends HasCloser with ClosedFuture with ...
 * </pre>
 *
 * @author Joacim Zschimmer
 */
trait ClosedFuture {
  this: HasCloser ⇒

  private val closedPromise = Promise[Unit]()

  onClose { closedPromise.success(()) }

  final def closed = closedPromise.future
}
