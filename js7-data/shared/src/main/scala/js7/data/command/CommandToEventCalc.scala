package js7.data.command

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.command.CommandToEventCalc.*
import js7.data.event.{Event, EventCalc, EventDrivenState, KeyedEvent}
import scala.reflect.ClassTag

trait CommandToEventCalc[S <: EventDrivenState[S, E], E <: Event, Ctx]:

  private type CommandEventConverter[Cmd <: CommonCommand] =
    CommandToEventCalc.Companion[S, E, Ctx]#CommandEventConverter[Cmd]

  protected def cmdExecutors: Iterable[CommandEventConverter[?]]

  private lazy val cmdClassToExecutor: Map[Class[? <: CommonCommand], CommandEventConverter[?]] =
    cmdExecutors.map(o => o.commandClass -> o).toMap

  def commandToEventCalc(cmd: CommonCommand): EventCalc[S, E, Ctx] =
    cmdClassToExecutor.get(cmd.getClass) match
      case None =>
        EventCalc.fail:
          Problem(s"${cmd.getClass.scalaName} is not a registered command")
      case Some(cmdToEventCalc) =>
        cmdToEventCalc.toEventCalc(cmd.asInstanceOf[cmdToEventCalc.Command])


object CommandToEventCalc:

  trait Companion[S <: EventDrivenState[S, E], E <: Event, Ctx]:
    private type CmdToEventCalc[Cmd <: CommonCommand] = Cmd => EventCalc[S, E, Ctx]

    /** Convert a command to an `EventCalc`. */
    trait CommandEventConverter[Cmd <: CommonCommand : ClassTag]:
      private[CommandToEventCalc] type Command = Cmd

      private[CommandToEventCalc] final val commandClass: Class[Cmd] = implicitClass[Cmd]

      def toEventCalc(cmd: Cmd): EventCalc[S, E, Ctx]

    /** A `CommandEventConverter` returning an `EventCalc`. */
    final class ToEventCalc[Cmd <: CommonCommand : ClassTag](
      cmdToEventCalc: CmdToEventCalc[Cmd])
    extends CommandEventConverter[Cmd]:
      def toEventCalc(cmd: Cmd): EventCalc[S, E, Ctx] =
        cmdToEventCalc(cmd)


    /** A simple `CommandEventConverter` returning `Seq[KeyedEvent]`. */
    final class ToEventSeq[Cmd <: CommonCommand : ClassTag](
      val commandToEvents: (Cmd, S) => Checked[Seq[KeyedEvent[E]]])
      extends CommandEventConverter[Cmd]:
      def toEventCalc(cmd: Cmd): EventCalc[S, E, Ctx] =
        EventCalc: coll =>
          coll.addChecked(commandToEvents(cmd, coll.aggregate))
