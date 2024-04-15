package js7.common.scalautil

import js7.base.utils.Closer
import scala.concurrent.{Future, Promise}

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
trait ClosedFuture:

  protected def closer: Closer

  private val closedPromise = Promise[Unit]()

  closer.onClose { closedPromise.success(()) }

  final def closed: Future[Unit] = 
    closedPromise.future
