package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.journal.watch.EventWatch
import js7.subagent.SubagentCommandExecutor
import js7.subagent.configuration.SubagentConf
import monix.eval.Task

object SubagentWebServer
{
  def resource(
    eventWatch: EventWatch,
    commandExecutor: SubagentCommandExecutor,
    sessionRegister: SessionRegister[SimpleSession],
    restartAsDirector: Task[Unit],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer with AkkaWebServer.HasUri] =
    AkkaWebServer.resource(
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
