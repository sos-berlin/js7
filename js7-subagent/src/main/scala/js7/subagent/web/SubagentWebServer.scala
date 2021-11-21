package js7.subagent.web

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.journal.watch.EventWatch
import js7.subagent.CommandExecutor
import js7.subagent.configuration.SubagentConf
import monix.eval.Task

// TODO Sollte der Subagent nicht auch den AgentWebServer verwenden?
object SubagentWebServer
{
  def resource(
    eventWatch: EventWatch,
    commandExecutor: CommandExecutor,
    sessionRegister: SessionRegister[SimpleSession],
    conf: SubagentConf)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AkkaWebServer] =
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
            conf.config))))
}
