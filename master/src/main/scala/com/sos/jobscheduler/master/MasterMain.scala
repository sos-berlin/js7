package com.sos.jobscheduler.master

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.{RichClosersAutoCloseable, RichClosersCloser}
import com.sos.jobscheduler.common.scalautil.Collections.implicits.RichArray
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
import scala.concurrent.Future

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
  lazy val master = injector.instance[Master]

  JavaShutdownHook.add("MasterMain") {
    logger.info("Terminate")
    close()
    Log4j.shutdown()
  }.closeWithCloser

  def run(): Nothing = {
    start() await 1.h
    master.executeCommand(MasterCommand.ScheduleOrdersEvery(OrderScheduleDuration))  // Will block on recovery until Agents are started: await 99.s
    while (true) sleep(100 * 365 * 24.h)
    sys.error("Unreachable code for now")
  }

  def start(): Future[Completed] = {
    master.start()
  }
}

object MasterMain {
  private val OrderScheduleDuration = 1 * 60.s
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    try run(args.toImmutableSeq)
    catch { case t: Throwable ⇒
      println(s"JOBSCHEDULER MASTER TERMINATED DUE TO ERROR: $t")
      logger.error(t.toString, t)
      System.exit(1)
    }
  }

  def run(args: Seq[String]): Unit =
    run(MasterConfiguration.fromCommandLine(args))

  def run(conf: MasterConfiguration): Unit =
    autoClosing(new MasterMain(conf)) { _.run() }
}
