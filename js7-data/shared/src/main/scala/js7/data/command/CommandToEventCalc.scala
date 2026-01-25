package js7.data.command

import js7.base.problem.{Checked, Problem}
import js7.base.scalasource.ScalaSourceLocation
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.event.EventCalc.OpaqueEventColl
import js7.data.event.{Event, EventCalc, EventColl, EventDrivenState_, KeyedEvent}
import scala.reflect.ClassTag

trait CommandToEventCalc[S <: EventDrivenState_[S, E], E <: Event](
  using src: ScalaSourceLocation):

  private type CommandEventConverter[Cmd <: IsEventEmittingCommand] =
    CommandToEventCalc.Companion[S, E]#CommandEventConverter[Cmd]

  protected def cmdExecutors: Iterable[CommandEventConverter[?]]

  private lazy val cmdClassToExecutor: Map[Class[? <: IsEventEmittingCommand], CommandEventConverter[?]] =
    cmdExecutors.map(o => o.commandClass -> o).toMap

  def commandToEventCalc(cmd: IsEventEmittingCommand): EventCalc[S, E] =
    cmdClassToExecutor.get(cmd.getClass) match
      case None =>
        EventCalc.problem:
          Problem(s"${cmd.getClass.shortClassName} is not a registered command ($src)")
      case Some(cmdToEventCalc) =>
        cmdToEventCalc.toEventCalc(cmd.asInstanceOf[cmdToEventCalc.Command])


object CommandToEventCalc:

  trait Companion[S <: EventDrivenState_[S, E], E <: Event]:

    /** Convert a command to an `EventCalc`. */
    trait CommandEventConverter[Cmd <: IsEventEmittingCommand : ClassTag]:
      private[CommandToEventCalc] type Command = Cmd

      private[CommandToEventCalc] final val commandClass: Class[Cmd] = implicitClass[Cmd]

      def toEventCalc(cmd: Cmd): EventCalc[S, E]

    object CommandEventConverter:
      def checked[Cmd <: IsEventEmittingCommand : ClassTag](
        toCheckedKeyedEvents: (Cmd, S) => OpaqueEventColl[S, E] ?=>
          Checked[IterableOnce[KeyedEvent[E]]])
      : CommandEventConverter[Cmd] =
        eventCalc: cmd =>
          EventCalc.checked: aggregate =>
            toCheckedKeyedEvents(cmd, aggregate)

      def coll[Cmd <: IsEventEmittingCommand : ClassTag](
        toEventColl: (Cmd, EventColl[S, E]) => Checked[EventColl[S, E]])
      : CommandEventConverter[Cmd] =
        eventCalc: cmd =>
          EventCalc(toEventColl(cmd, _))

      /** A `CommandEventConverter` returning an `EventCalc`. */
      def eventCalc[Cmd <: IsEventEmittingCommand : ClassTag](cmdToEventCalc: Cmd => EventCalc[S, E])
      : CommandEventConverter[Cmd] =
        new CommandEventConverter[Cmd]:
          def toEventCalc(cmd: Cmd): EventCalc[S, E] =
            cmdToEventCalc(cmd)
