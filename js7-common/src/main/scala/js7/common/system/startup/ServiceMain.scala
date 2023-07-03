package js7.common.system.startup

import cats.effect.{Resource, Sync, SyncIO}
import cats.syntax.flatMap.*
import com.typesafe.config.Config
import izumi.reflect.Tag
import js7.base.BuildInfo
import js7.base.configutils.Configs.logConfig
import js7.base.io.process.ReturnCode
import js7.base.log.Logger.syntax.*
import js7.base.log.{Log4j, Logger}
import js7.base.service.{MainService, MainServiceTerminationException, Service}
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.ThreadPools
import js7.common.system.startup.Js7ReturnCodes.terminationToReturnCode
import js7.common.system.startup.StartUp.{logJavaSettings, nowString, printlnWithClock, startUpLine}
import js7.common.utils.JavaShutdownHook
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.{Deadline, Duration}

object ServiceMain
{
  private var _runningSince: Option[Deadline] = None

  def mainThenExit[Conf <: BasicConfiguration, S <: MainService: Tag](
    args: Array[String],
    name: String,
    argsToConf: CommandLineArguments => Conf,
    useLockFile: Boolean = false)(
    toResource: (Conf, Scheduler) => Resource[Task, S],
    use: (Conf, S, Scheduler) => Task[ProgramTermination] =
      (_: Conf, state: S, _: Scheduler) => state.untilTerminated)
  : Unit = {
    val returnCode =
      returnCodeMain(args, name, argsToConf, useLockFile = useLockFile)(toResource, use)
    JavaMain.exitIfNonZero(returnCode)
  }

  /** Returns the return code. */
  def returnCodeMain[Conf <: BasicConfiguration, S <: MainService: Tag](
    args: Array[String],
    name: String,
    argsToConf: CommandLineArguments => Conf,
    useLockFile: Boolean = false)(
    toServiceResource: (Conf, Scheduler) => Resource[Task, S],
    use: (Conf, S, Scheduler) => Task[ProgramTermination] =
    (_: Conf, state: S, _: Scheduler) => state.untilTerminated)
  : ReturnCode = {
    logging.startUp(name)
    handleProgramTermination(name) {
      JavaMainLockfileSupport.runMain(args, useLockFile = useLockFile) { commandLineArguments =>
        lazy val conf = {
          val conf = argsToConf(commandLineArguments)
          commandLineArguments.requireNoMoreArguments() // throws
          conf
        }
        logging.logFirstLines(commandLineArguments, conf)
        logging.blockingRun(name, conf.config, toServiceResource(conf, _))(use(conf, _, _))
      }
    }
  }

  private def handleProgramTermination(name: String)(body: => ProgramTermination): ReturnCode =
    try {
      val termination = body
      logging.onProgramTermination(name, termination)
    } catch logging.catcher

  def readyMessageWithLine(prefix: String): String =
    prefix +
      _runningSince.fold("")(o => s" (after ${o.elapsed.pretty})") +
      "\n" + "─" * 80

  /** For usage after logging system has properly been initialized. */
  private object logging {
    private lazy val logger = {
      Logger.initialize()
      Logger[ServiceMain.type]
    }

    def startUp(name: String): Unit = {
      val nanoTime = System.nanoTime() // Before anything else, fetch clock

      printlnWithClock(s"JS7 $name ${BuildInfo.longVersion}")
      // Log early for early timestamp and proper logger initialization by a
      // single (non-concurrent) call
      // Log a bar, in case the previous log file is being appended
      logger.info(s"JS7 $name ${BuildInfo.longVersion}\n${"━" * 80}")

      _runningSince = Some(Deadline(Duration.fromNanos(nanoTime)))
      StartUp.initializeMain()
    }

    def onProgramTermination(name: String, termination: ProgramTermination): ReturnCode =
      try {
        // Log complete timestamp in case of short log timestamp
        val msg = s"JS7 $name terminates now" +
          (termination.restart ?? " and is expected to restart") + s" ($nowString)"
        logger.info(msg)
        printlnWithClock(msg)

        terminationToReturnCode(termination)
      } catch catcher

    def catcher: PartialFunction[Throwable, ReturnCode] = {
      case t: Throwable =>
        logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
        System.err.println(t.toStringWithCauses)
        t.printStackTrace(System.err)
        ReturnCode.StandardFailure
    }

    def logFirstLines(commandLineArguments: CommandLineArguments, conf: => BasicConfiguration)
    : Unit = {
      logger.info(startUpLine())
      logger.debug(commandLineArguments.toString)

      val paths = conf.maybeConfigDirectory.map(o => s"config=$o") ++
        conf.maybeDataDirectory.map(o => s"data=$o")
      if (paths.nonEmpty) logger.info(paths.mkString(" "))

      logConfig(conf.config)
      logJavaSettings()
    }

    ///** Adds an own ThreadPool and a shutdown hook. */
    //def blockingRun[S <: MainService: Tag](name: String, config: Config)(
    //  resource: Scheduler => Resource[Task, S])
    //: ProgramTermination =
    //  blockingRun(name, config, resource)((_: S).untilTerminated)

    /** Adds an own ThreadPool and a shutdown hook. */
    private[ServiceMain] def blockingRun[S <: MainService: Tag](
      name: String,
      config: Config,
      resource: Scheduler => Resource[Task, S])(
      use: (S, Scheduler) => Task[ProgramTermination])
    : ProgramTermination =
      ThreadPools.standardSchedulerResource[SyncIO](name, config)
        .use(implicit scheduler => SyncIO(
          withShutdownHook(resource(scheduler))
            .use(use(_, scheduler))
            .onErrorHandle(catchMainServiceTermination)
            .awaitInfinite))
        .unsafeRunSync()

    private def catchMainServiceTermination: PartialFunction[Throwable, ProgramTermination] = {
      case t: MainServiceTerminationException =>
        logger.debug(t.toStringWithCauses)
        logger.info(t.getMessage)
        t.termination
    }

    private def withShutdownHook[S <: MainService: Tag](serviceResource: Resource[Task, S])
      (implicit scheduler: Scheduler)
    : Resource[Task, S] =
      serviceResource
        .toAllocatedResource
        .flatTap(allocated =>
          shutdownHookResource[Task](name = allocated.allocatedThing.toString)(
            onJavaShutdown(allocated)))
        .map(_.allocatedThing)

    private def onJavaShutdown[S <: Service](allocatedService: Allocated[Task, S])
      (implicit s: Scheduler)
    : Unit = {
      logger.warn(s"Trying to shut down JS7 $allocatedService due to Java shutdown")
      val stop = logger.debugTask("onJavaShutDown stop service")(
        allocatedService.release)
      for (t <- stop.attempt.runSyncUnsafe().left) {
        logger.error(s"onJavaShutdown: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      }
    }

    private def shutdownHookResource[F[_] : Sync](name: String)(onJavaShutdown: => Unit)
    : Resource[F, Unit] =
      Resource
        .fromAutoCloseable(Sync[F].delay(
          addJavaShutdownHook(name, () => onJavaShutdown)))
        .map(_ => ())

    private def addJavaShutdownHook(name: String, onJavaShutdown: () => Unit)
    : AutoCloseable = {
      JavaShutdownHook.add(name) {
        try onJavaShutdown()
        catch {
          case t: Throwable =>
            logger.debug(t.toStringWithCauses, t)
            throw t
        } finally
          Log4j.shutdown()
      }
    }
  }
}
