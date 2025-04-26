package js7.data.command

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.command.CommandToEventCalc.*
import js7.data.event.EventCalc.OpaqueEventColl
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
        EventCalc.problem:
          Problem(s"${cmd.getClass.scalaName} is not a registered command")
      case Some(cmdToEventCalc) =>
        cmdToEventCalc.toEventCalc(cmd.asInstanceOf[cmdToEventCalc.Command])


object CommandToEventCalc:

  trait Companion[S <: EventDrivenState[S, E], E <: Event, Ctx]:

    /** Convert a command to an `EventCalc`. */
    trait CommandEventConverter[Cmd <: CommonCommand : ClassTag]:
      private[CommandToEventCalc] type Command = Cmd

      private[CommandToEventCalc] final val commandClass: Class[Cmd] = implicitClass[Cmd]

      def toEventCalc(cmd: Cmd): EventCalc[S, E, Ctx]

    object CommandEventConverter:
      def checked[Cmd <: CommonCommand : ClassTag](
        toCheckedKeyedEvents: (Cmd, S) => OpaqueEventColl[S, E, Ctx] ?=>
          Checked[IterableOnce[KeyedEvent[E]]])
      : CommandEventConverter[Cmd] =
        CmdToEventCalc[Cmd]: cmd =>
          EventCalc.checked: aggregate =>
            toCheckedKeyedEvents(cmd, aggregate)


    /** A `CommandEventConverter` returning an `EventCalc`. */
    final class CmdToEventCalc[Cmd <: CommonCommand : ClassTag](
      cmdToEventCalc: Cmd => EventCalc[S, E, Ctx])
    extends CommandEventConverter[Cmd]:
      def toEventCalc(cmd: Cmd): EventCalc[S, E, Ctx] =
        cmdToEventCalc(cmd)
