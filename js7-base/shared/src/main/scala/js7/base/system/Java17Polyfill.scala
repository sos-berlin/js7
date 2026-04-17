package js7.base.system

import java.util.Objects
import scala.annotation.nowarn

object Java17Polyfill:

  /** Required when IDE compiles with JDK >17, but code must still be compilable with JDK 17
   * and imports should be kept. */
  def java17Polyfill(): Unit = {}

  // Java 26 seems slower than our implementation
  //private val getCharsMethod: Method | Null =
  //  if Runtime.version.feature >= 25 then
  //    classOf[CharSequence].getMethod("getChars",
  //      classOf[Int], classOf[Int], classOf[Array[Char]], classOf[Int])
  //  else
  //    null

  extension (charSequence: CharSequence)
    /** Since Java 25. */
    def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
      //getCharsMethod match
      //  case null =>
      charSequence match
        case string: String =>
          string.getChars(srcBegin, srcEnd, dst, dstBegin)
        case _ =>
          Objects.checkFromToIndex(srcBegin, srcEnd, charSequence.length)
          Objects.checkIndex(dstBegin, dst.length - (srcEnd - srcBegin) + 1)
          var i = srcBegin
          val off = dstBegin - srcBegin
          while i < srcEnd do
            dst(off + i) = charSequence.charAt(i)
            i += 1
      //  case m: Method =>
      //    m.invoke(charSequence, srcBegin, srcEnd, dst, dstBegin)

  extension (thread: Thread)
    /** Since Java 19. */
    def threadId: Long =
      thread.getId: @nowarn("msg=deprecated")
