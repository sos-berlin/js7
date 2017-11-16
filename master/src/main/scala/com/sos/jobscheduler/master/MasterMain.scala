package com.sos.jobscheduler.master

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.Collections.implicits.RichArray
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.SideEffect._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.master.MasterMain._
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * JobScheduler Master.
  *
  * @author Joacim Zschimmer
  */
final class MasterMain(conf: MasterConfiguration) extends HasCloser {
  def this(args: Seq[String]) = this(MasterConfiguration.fromCommandLine(args))

  lazy val injector = Guice.createInjector(new MasterModule(conf)) sideEffect { o ⇒
    closer.registerAutoCloseable(o.instance[Closer])
  }
  locally {
    injector.instance[ActorSystem].whenTerminated onComplete { _ ⇒
      logger.debug("ActorSystem terminated")
    }
  }
}

object MasterMain {
  private val OrderScheduleDuration = 1 * 60.s
  private val ShutdownTimeout = 5.s
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    try
      start(MasterConfiguration.fromCommandLine(args.toImmutableSeq))
        .flatMap { _.terminated }
        .onComplete {
          case Success(Completed) ⇒
            println("JobScheduler Master terminated")
            logger.debug("Terminated")
            Log4j.shutdown()
            sys.runtime.exit(0)
          case Failure(t) ⇒
            exitJava(t)
        }
    catch { case t: Throwable ⇒
      println(s"JOBSCHEDULER MASTER TERMINATED DUE TO ERROR: $t")
      logger.error(t.toString, t)
      Log4j.shutdown()
      sys.runtime.exit(1)
    }
  }

  def start(conf: MasterConfiguration): Future[RunningMaster] =
    for (master ← RunningMaster(conf)) yield {
      val hook = JavaShutdownHook.add("MasterMain") {
        // TODO Interfers with Akkas CoordinatedShutdown shutdown hook
        onJavaShutdown(master)
      }
      master.executeCommand(MasterCommand.ScheduleOrdersEvery(OrderScheduleDuration)) // Will block on recovery until Agents are started: await 99.s
      master.terminated andThen { case _ ⇒
        hook.remove()
      }
      master
    }

  private def exitJava(throwable: Throwable): Unit = {
    println(s"JOBSCHEDULER MASTER TERMINATED DUE TO ERROR: ${throwable.toStringWithCauses}")
    logger.error(throwable.toString, throwable)
    Log4j.shutdown()
    sys.runtime.exit(1)
  }

  private def onJavaShutdown(master: RunningMaster): Unit = {
    logger.info("Terminating Master due to Java shutdown")
    master.terminated await ShutdownTimeout
    master.close()
    Log4j.shutdown()
  }
}
