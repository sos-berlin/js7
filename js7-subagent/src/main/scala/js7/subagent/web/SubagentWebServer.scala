package js7.subagent.web

import cats.effect.Resource
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.journal.watch.EventWatch
import js7.subagent.SubagentCommandExecutor
import js7.subagent.configuration.SubagentConf
import monix.eval.Task
import org.apache.pekko.actor.ActorSystem

object SubagentWebServer
{
  def resource(
    eventWatch: EventWatch,
    commandExecutor: SubagentCommandExecutor,
    sessionRegister: SessionRegister[SimpleSession],
    restartAsDirector: Task[Unit],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, PekkoWebServer & PekkoWebServer.HasUri] =
    PekkoWebServer.resource(
      conf.webServerBindings,
      conf.config,
      (binding, whenShuttingDown) =>
        Task.deferAction(implicit s => Task(
          new SubagentBoundRoute(
            binding,
            whenShuttingDown,
            eventWatch,
            commandExecutor,
            sessionRegister,
            restartAsDirector,
            conf.config))))
}
