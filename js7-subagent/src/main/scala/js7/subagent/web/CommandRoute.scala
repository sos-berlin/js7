package js7.subagent.web

import js7.base.auth.AgentDirectorPermission
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.completeTask
import js7.common.pekkohttp.StandardMarshallers.checkedToResponseMarshaller
import js7.core.command.CommandMeta
import js7.core.web.EntitySizeLimitProvider
import js7.data.subagent.SubagentCommand
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.http.scaladsl.server.Directives.{as, entity, pathEnd, post, withSizeLimit}
import org.apache.pekko.http.scaladsl.server.Route

private trait CommandRoute extends SubagentRouteProvider with EntitySizeLimitProvider:
  protected def executeCommand(command: Numbered[SubagentCommand], meta: CommandMeta)
  : Task[Checked[SubagentCommand.Response]]

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val commandRoute: Route =
    (pathEnd & post & withSizeLimit(entitySizeLimit))(
      authorizedUser(AgentDirectorPermission)(user =>
        entity(as[Numbered[SubagentCommand]])(command =>
          completeTask(
            executeCommand(command, CommandMeta(user))
              .map(_.map(o => o: SubagentCommand.Response))))))
