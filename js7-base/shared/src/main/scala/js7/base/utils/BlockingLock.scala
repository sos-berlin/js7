package js7.base.utils

import java.util.concurrent.locks.ReentrantLock

final class BlockingLock(fair: Boolean = false):

  private val lock = ReentrantLock(fair)

  inline def apply[A](body: => A): A =
    lock(body)

  def lock[A](body: => A): A =
    lock.lockInterruptibly() 
    try
      body
    finally
      lock.unlock()
