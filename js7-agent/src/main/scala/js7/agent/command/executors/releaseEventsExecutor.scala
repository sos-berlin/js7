package js7.agent.command.executors

import js7.agent.command.AgentCommandToEventCalc.CommandEventConverter
import js7.agent.data.commands.AgentCommand.ReleaseEvents
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey

private[command] def releaseEventsExecutor =
  CommandEventConverter.checked[ReleaseEvents]: (cmd, agentState) =>
    if agentState.meta.isEmpty then // to be sure
      Left(AgentNotDedicatedProblem)
    else
      agentState.meta.controllerId.toUserId.flatMap: userId =>
        import cmd.untilEventId
        val current = agentState.journalState.userIdToReleasedEventId(userId) // Must contain userId
        if untilEventId < current then
          Left(ReverseReleaseEventsProblem(
            requestedUntilEventId = untilEventId, currentUntilEventId = current))
        else
          Right:
            (NoKey <-: JournalEventsReleased(userId, untilEventId)) :: Nil
