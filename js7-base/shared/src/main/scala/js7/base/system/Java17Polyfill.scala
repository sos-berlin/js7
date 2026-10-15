package js7.base.system

import java.lang.reflect.{InvocationTargetException, Method}
import java.net.http.HttpClient
import java.util.Objects
import scala.annotation.nowarn

object Java17Polyfill:

  private val String_indexOf = getMethod(javaVersion = 21):
    classOf[String].getMethod("indexOf", classOf[Int], classOf[Int], classOf[Int])

  /** Required when IDE compiles with JDK >17, but code must still be compilable with JDK 17
   * and imports should be kept. */
  def java17Polyfill(): Unit = {}

  // Java 26 seems slower than our implementation TODO Check again!
  //private val String_getChars = getMethod(javaVersion = 25):
  //  classOf[CharSequence].getMethod("getChars",
  //    classOf[Int], classOf[Int], classOf[Array[Char]], classOf[Int])

  extension (string: String)
    /** Since Java 21. */
    def indexOf(char: Int, beginIndex: Int, endIndex: Int): Int =
      if String_indexOf != null then
        wrapInvoke(String_indexOf.invoke(string, char, beginIndex, endIndex).asInstanceOf[Int])
      else
        if beginIndex < 0 || beginIndex > endIndex || endIndex > string.length then
          throw StringIndexOutOfBoundsException:
            s"String index out of bounds: beginIndex=$beginIndex, endIndex=$endIndex, string length=${string.length}"
        val n = endIndex min string.length
        var i = beginIndex
        while i < n do
          if string.charAt(i) == char then return i
          i += 1
        -1

  extension (charSequence: CharSequence)
    /** Since Java 25. */
    def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
      //String_getChars match
      //  case null =>
      charSequence match
        case o: CharSequenceJava17 =>
          o.getChars(srcBegin, srcEnd, dst, dstBegin)

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
      //    wrapInvoke(m.invoke(charSequence, srcBegin, srcEnd, dst, dstBegin))


  extension (thread: Thread)
    /** Since Java 19. */
    def threadId =
      thread.getId: @nowarn("msg=deprecated")


  extension (httpClient: HttpClient)
    /** Since Java 21. */
    def shutdownNow(): Unit =
      () // Java before 21 depends on the garbage collector to release resources (bad)


  private def getMethod(javaVersion: Int)(body: => Method): Method | Null =
    if Runtime.version.feature < javaVersion then
      null
    else
      try body
      catch case _: NoSuchMethodException => null


  private def wrapInvoke[R](body: => R): R =
    try body
    catch case t: InvocationTargetException => throw t.getCause
