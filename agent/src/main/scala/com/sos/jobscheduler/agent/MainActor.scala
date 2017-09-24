package com.sos.jobscheduler.agent

import akka.actor.{Actor, Props, Terminated}
import com.google.inject.Injector
import com.softwaremill.tagging._
import com.sos.jobscheduler.agent.MainActor._
import com.sos.jobscheduler.agent.command.{CommandActor, CommandHandler, SessionActor}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.{AgentActor, AgentHandle}
import com.sos.jobscheduler.agent.web.common.LoginSession
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister
import com.sos.jobscheduler.common.akkautils.CatchingSupervisorStrategy
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Logger
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class MainActor(
  agentConfiguration: AgentConfiguration,
  sessionRegister: SessionRegister[LoginSession],
  injector: Injector,
  readyPromise: Promise[Ready],
  stoppedPromise: Promise[Completed])
  (implicit executionContext: ExecutionContext)
extends Actor {

  import agentConfiguration.akkaAskTimeout
  import context.{actorOf, watch}

  override val supervisorStrategy = CatchingSupervisorStrategy(stoppedPromise)

  private val agentActor = watch(actorOf(Props { injector.instance[AgentActor] }, "agent"))
  private val agentHandle = new AgentHandle(agentActor)(akkaAskTimeout)

  private val commandHandler = injector.option[CommandHandler] getOrElse { // Only tests bind a CommandHandler
    val sessionActor = actorOf(Props { new SessionActor(sessionRegister) }, "session").taggedWith[SessionActor]
    val actor = actorOf(Props { new CommandActor(sessionActor, agentHandle) }, "command")
    new CommandActor.Handle(actor)(akkaAskTimeout)
  }

  override def preStart() = {
    super.preStart()
    for (t ← stoppedPromise.future.failed) readyPromise.tryFailure(t)
    agentActor ! AgentActor.Input.Start
  }

  override def postStop() = {
    if (!readyPromise.isCompleted) {
      readyPromise.tryFailure(new RuntimeException("MainActor has stopped before AgentActor has become ready") with NoStackTrace)
    }
    if (!stoppedPromise.isCompleted) {
      stoppedPromise.tryFailure(new RuntimeException("MainActor has stopped unexpectedly") with NoStackTrace)
    }
    logger.info("Terminated")
    super.postStop()
  }

  def receive = {
    case AgentActor.Output.Ready ⇒
      readyPromise.success(Ready(commandHandler, agentHandle))

    case Terminated(`agentActor`) ⇒
      logger.debug("AgentActor has stopped")
      stoppedPromise.trySuccess(Completed)
      context.stop(self)
  }
}

object MainActor {
  private val logger = Logger(getClass)

  final case class Ready(commandHandler: CommandHandler, agentHandle: AgentHandle)
}
