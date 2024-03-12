package js7.base.system

import scala.annotation.nowarn

object Java17Polyfill:

  /** Required when IDE compiles with JDK >17, but code must still be compilable with JDK 17
   * and imports should be kept. */
  def java17Polyfill() = {}

  extension (thread: Thread)
    /** Since Java 19. */
    def threadId: Long =
      thread.getId: @nowarn("msg=deprecated")
