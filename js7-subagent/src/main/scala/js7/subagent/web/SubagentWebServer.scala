package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.subagent.SubagentCommandExecutor
import js7.subagent.configuration.SubagentConf
import monix.eval.Task
import monix.execution.Scheduler

object SubagentWebServer
{
  def resource(
    commandExecutor: Task[SubagentCommandExecutor],
    sessionRegister: SessionRegister[SimpleSession],
    convertToDirector: Task[Unit],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem, scheduler: Scheduler)
  : Resource[Task, AkkaWebServer & AkkaWebServer.HasUri] =
    AkkaWebServer.resource(
      conf.webServerBindings,
      conf.config,
      (binding, whenShuttingDown) =>
        for (commandExecutor <- commandExecutor) yield
          new SubagentBoundRoute(
            binding,
            whenShuttingDown,
            commandExecutor,
            sessionRegister,
            convertToDirector,
            conf.config))
}
