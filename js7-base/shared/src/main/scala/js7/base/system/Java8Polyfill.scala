package js7.base.system

import scala.annotation.nowarn

object Java8Polyfill:

  val javaVersion: Int =
    // Java 9: = Runtime.version.major
    // Java 10: = Runtime.version.feature
    sys.props.get("java.version")
      .flatMap { v =>
        val parts = v.split("\\.")
        val s = parts(0) match
          case "1" => parts(1)
          case part0 => part0
        s.toIntOption
      }
      .getOrElse(0)

  /** Required when IDE compiles with JDK >8, but code must still be compilable with JDK 8
   *  and imports should be kept. */
  def java8Polyfill() = {}

  implicit final class ThreadPolyfill(private val thread: Thread) extends AnyVal:
    // Since Java 19
    def threadId: Long =
      thread.getId: @nowarn("msg=deprecated")
