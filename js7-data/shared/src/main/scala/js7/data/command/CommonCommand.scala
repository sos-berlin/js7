package js7.data.command

import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.log.CorrelIdWrapped
import js7.base.problem.Checked
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
trait CommonCommand:
  type Response

  def toShortString: String = toString

  /**
   * true if toString returns a longer string than toShortString.
   */
  def toStringIsLonger = false


object CommonCommand:
  trait Companion:
    protected type Command <: CommonCommand
    protected type Response

    implicit val jsonCodec: TypedJsonCodec[Command]

    trait CommonBatch:
      this: Command =>

      def commands: Seq[CorrelIdWrapped[Command]]

      override def toString: String =
        val b = mutable.Buffer.empty[String]
        var last = ""
        var n = 0
        def flush(): Unit = if !last.isEmpty then
          b += (if n == 1 then last else s"$n×$last")
          last = ""
          n = 0
        for CorrelIdWrapped(_, command) <- commands do
          val name = jsonCodec.typeName(command)
          if last != name then
            flush()
            last = name
          n += 1
        flush()
        s"${jsonCodec.classToName(getClass)}(${b.mkString(", ")})"

      override def toShortString =
        s"Batch(${commands.size} commands, ${commands.take(1).map(o => o.value.toShortString + ", ").mkString} ...)"

    object CommonBatch:
      trait Response:
        def responses: Seq[Checked[Companion.this.Response]]
        def productPrefix: String

        override def toString: String =
          val succeeded = responses.count(_.isRight)
          s"$productPrefix($succeeded succeeded and ${responses.size - succeeded} failed)"
