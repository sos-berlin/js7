package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.base.problem.Checked
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
trait CommonCommand
{
  type Response

  def toShortString = toString

  /**
   * true if toString returns a longer string than toShortString.
   */
  def toStringIsLonger = false
}

object CommonCommand
{
  trait Companion
  {
    protected type Command <: CommonCommand
    type Response

    implicit val jsonCodec: TypedJsonCodec[Command]

    trait CommonBatch {
      this: Command =>

      def commands: Seq[Command]

      override def toString = {
        val b = mutable.Buffer[String]()
        var last = ""
        var n = 0
        def flush() = if (!last.isEmpty) {
          b += (if (n == 1) last else s"$n×$last")
          last = ""
          n = 0
        }
        for (command <- commands) {
          val name = jsonCodec.typeName(command)
          if (last != name) {
            flush()
            last = name
          }
          n += 1
        }
        flush()
        s"${jsonCodec.classToName(getClass)}(${b.mkString(", ")})"
      }
    }
    object CommonBatch {
      trait Response {
        def responses: Seq[Checked[Companion.this.Response]]
        def productPrefix: String

        override def toString = {
          val succeeded = responses.count(_.isRight)
          s"$productPrefix($succeeded succeeded and ${responses.size - succeeded} failed)"
        }
      }
    }
  }
}
