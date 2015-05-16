package com.sos.scheduler.engine.common.async

import java.time.Duration
import scala.concurrent.{Future, Promise}

/**
 * Runs concurrent (in its own thread) and calls periodically a function.
 *
 * @author Joacim Zschimmer
 */
final class ConcurrentCaller(pauses: TraversableOnce[Duration], function: () ⇒ Unit, name: String) extends AutoCloseable {

  private val terminatedPromise = Promise[Unit]()

  private object thread extends Thread {
    @volatile private var closed: Boolean = false

    setName(name)

    def close(): Unit = {
      synchronized {
        closed = true
        notifyAll()
      }
    }

    override def run(): Unit = {
      try {
        for (t ← pauses) {
          function()
          synchronized {
            if (t.toMillis > 0) {  // wait(0) blocks forever
              if (closed) return
              wait(t.toMillis)
            }
            if (closed) return
          }
        }
        function()
      }
      catch { case t: Throwable ⇒ terminatedPromise.failure(t) }
      finally terminatedPromise.trySuccess(())
    }
  }

  def close(): Unit = {
    thread.close()
    thread.join()
  }

  def start(): Unit = thread.start()

  def terminated: Future[Unit] = terminatedPromise.future
}
