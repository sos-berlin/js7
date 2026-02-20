package js7.base.system

import scala.annotation.nowarn

object Java17Polyfill:

  /** Required when IDE compiles with JDK >17, but code must still be compilable with JDK 17
   * and imports should be kept. */
  def java17Polyfill(): Unit = {}

  extension (charSequence: CharSequence)
    /** Since Java 25. */
    def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
      charSequence match
        case string: String =>
          string.getChars(srcBegin, srcEnd, dst, dstBegin)
        case _ =>
          var i = srcBegin
          val off = dstBegin - srcBegin
          while i < srcEnd do
            dst(off + i) = charSequence.charAt(i)
            i += 1

  extension (thread: Thread)
    /** Since Java 19. */
    def threadId: Long =
      thread.getId: @nowarn("msg=deprecated")
