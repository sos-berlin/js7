package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.journal.watch.EventWatch
import js7.subagent.SubagentCommandExecuter
import js7.subagent.configuration.SubagentConf
import monix.eval.Task

object SubagentWebServer
{
  def resource(
    subagentCommandExecuter: SubagentCommandExecuter,
    eventWatch: EventWatch,
    sessionRegister: SessionRegister[SimpleSession],
    convertToDirector: Task[Unit],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer & AkkaWebServer.HasUri] =
    AkkaWebServer.resource(
      conf.webServerBindings,
      conf.config,
      (binding, whenShuttingDown) =>
        Task.deferAction(implicit s => Task(
          new SubagentBoundRoute(
            binding,
            whenShuttingDown,
            subagentCommandExecuter,
            eventWatch,
            sessionRegister,
            convertToDirector,
            conf.config))))
}
