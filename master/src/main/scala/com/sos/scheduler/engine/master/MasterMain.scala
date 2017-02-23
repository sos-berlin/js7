package com.sos.scheduler.engine.master

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.{RichClosersAutoCloseable, RichClosersCloser}
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichArray
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.scalautil.SideEffect._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.JavaShutdownHook
import com.sos.scheduler.engine.master.MasterMain._
import com.sos.scheduler.engine.master.configuration.MasterConfiguration
import com.sos.scheduler.engine.master.configuration.inject.MasterModule
import com.sos.scheduler.engine.master.oldruntime.InstantInterval
import java.time.Instant.now
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
  private lazy val master = injector.instance[Master]

  JavaShutdownHook.add("MasterMain") {
    logger.info("Terminate")
    close()
  }.closeWithCloser

  def run(): Nothing = {
    start() await 1.h
    master.addScheduledOrders(InstantInterval(now, 5 * 60.s))   // TODO schedule orders every period
    while (true) sleep(100 * 365 * 24.h)
    sys.error("Unreachable code for now")
  }

  def start(): Future[Completed] = {
    master.start()
  }
}

object MasterMain {
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
