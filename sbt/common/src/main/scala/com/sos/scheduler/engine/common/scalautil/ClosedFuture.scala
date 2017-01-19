package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import scala.concurrent.Promise

/**
 * Provides a Future `closed`, which succeeds with `close`.
 * <p>
 * <b>Should be one of the first mixins</b>, so that `closed` succeeds not before `Closer` has anything closed.
 * For example:
 * <pre>
 *    class X extends HasCloser with ClosedFuture with ...
 * </pre>
 *
 * @author Joacim Zschimmer
 */
trait ClosedFuture {

  protected def closer: Closer

  private val closedPromise = Promise[Unit]()

  closer.onClose { closedPromise.success(()) }

  final def closed = closedPromise.future
}
