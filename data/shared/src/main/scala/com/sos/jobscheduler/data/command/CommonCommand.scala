package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.base.problem.Checked
import scala.collection.immutable.Seq

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
      this: Command ⇒

      def commands: Seq[Command]

      override def toString = {
        val n = 3
        s"${jsonCodec.classToName(getClass)}(${commands.size} commands: ${commands take n map jsonCodec.typeName mkString ", "}${if (commands.lengthCompare(n) > 0) ", ..." else ""})"
      }
    }
    object CommonBatch {
      trait Response {
        def responses: Seq[Checked[Companion.this.Response]]
        def productPrefix: String

        override def toString = {
          val succeeded = responses.count(_.isValid)
          s"$productPrefix($succeeded succeeded and ${responses.size - succeeded} failed)"
        }
      }
    }
  }
}
