package js7.common.system.startup

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, ResourceIO}
import izumi.reflect.Tag
import js7.base.configutils.Configs.logConfig
import js7.base.log.{Log4j, Logger}
import js7.base.service.{MainService, MainServiceTerminationException}
import js7.base.system.startup.StartUp
import js7.base.system.startup.StartUp.{logJavaSettings, nowString, printlnWithClock}
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.{BuildInfo, utils}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import scala.concurrent.duration.{Deadline, Duration}

object ServiceMain:
  private var _runningSince: Option[Deadline] = None

  locally:
    _runningSince = Some(Deadline(Duration.fromNanos(System.nanoTime())))

  // Run initialization code above.
  def initialize(): Unit = ()

  /** Returns the return code. */
  def runAsMain[Conf <: BasicConfiguration, S <: MainService: Tag](
    args: Seq[String],
    name: String,
    argsToConf: CommandLineArguments => Conf,
    useLockFile: Boolean = false,
    suppressShutdownLogging: Boolean = false)
    (toServiceResource: Conf => ResourceIO[S],
    use: (Conf, S) => IO[ProgramTermination] =
    (_: Conf, service: S) => service.untilTerminated)
  : IO[ExitCode] =
    IO
      .defer:
        printlnWithClock(s"JS7 $name ${BuildInfo.longVersion}")
        StartUp.initializeMain()

        JavaMainLockfileSupport
          .runMain(name, args.toVector, useLockFile = useLockFile):
            commandLineArguments => IO.defer:
              lazy val conf =
                val conf = argsToConf(commandLineArguments)
                commandLineArguments.requireNoMoreArguments() // throws
                conf
              logging.logFirstLines(commandLineArguments, conf)
              logging.run(toServiceResource(conf))(use(conf, _))
          .attempt.map:
            case Left(throwable) => logging.throwableToExitCode(throwable)
            case Right(termination) => logging.onProgramTermination(name, termination)
      .guarantee:
        IO.unlessA(suppressShutdownLogging)(IO:
          Log4j.shutdown())

  def blockingRun[S <: MainService](
    name: String,
    timeout: Duration = Duration.Inf)
    (using rt: IORuntime, S: Tag[S])
    (resource: ResourceIO[S],
    use: S => ProgramTermination = (_: S)
      .untilTerminated
      .map(o => o: ProgramTermination) // because we have no Tag[S#Termination]
      .await(timeout))
  : ProgramTermination =
    resource
      .toAllocated
      .await(timeout)
      .useSync(timeout + 1.s)(use)

  def readyMessageWithLine(prefix: String): String =
    prefix +
      _runningSince.fold("")(o => s" (after ${o.elapsed.pretty})") +
      "\n" + "─" * 80

  private def startUp(name: String): Unit =
    val nanoTime = System.nanoTime() // Before anything else, fetch clock

      printlnWithClock(s"JS7 $name ${BuildInfo.prettyVersion}")

    _runningSince = Some(Deadline(Duration.fromNanos(nanoTime)))
    StartUp.initializeMain()


  /** For usage after logging system has properly been initialized. */
  private object logging:
    private lazy val logger =
      Logger.initialize("JS7 Engine") // In case it has not yet been initialized
      Logger[ServiceMain.type]

    def onProgramTermination(name: String, termination: ProgramTermination): ExitCode =
      try
        // Log complete timestamp in case of short log timestamp
        val msg = s"JS7 $name terminates now" +
          (termination.restart ?? " and is expected to restart") + s" ($nowString)"
        logger.info(msg)
        printlnWithClock(msg)

        termination.toExitCode
      catch throwableToExitCode

    def throwableToExitCode: PartialFunction[Throwable, ExitCode] =
      case t: Throwable =>
        logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
        System.err.println(t.toStringWithCauses)
        t.printStackTrace(System.err)
        ExitCode.Error

    def logFirstLines(commandLineArguments: CommandLineArguments, conf: => BasicConfiguration)
    : Unit =
      logger.debug(commandLineArguments.toString)

      val paths = conf.maybeConfigDirectory.map(o => s"config=$o") ++
        conf.maybeDataDirectory.map(o => s"data=$o")
      if paths.nonEmpty then logger.info(paths.mkString(" "))

      logConfig(conf.config)
      logJavaSettings()

    private[ServiceMain] def run[S <: MainService: Tag](
      resource: ResourceIO[S])
      (use: S => IO[ProgramTermination])
    : IO[ProgramTermination] =
      resource
        .use: service =>
          use(service)
        .recover:
          case t: MainServiceTerminationException =>
            logger.debug(t.toStringWithCauses)
            logger.info(t.getMessage)
            t.termination

    //private def onMainServiceCanceled[S <: Service](allocatedService: Allocated[IO, S])
    //: IO[Unit] =
    //  IO.defer:
    //    logger.warn(s"Trying to shut down $allocatedService due to Java shutdown")
    //    val stop = logger.debugIO("onJavaShutDown stop service")(
    //      allocatedService.release)
    //    stop.onError(t => IO:
    //      logger.error(s"onMainServiceCanceled: ${t.toStringWithCauses}", t.nullIfNoStackTrace))


    //private def withShutdownHook[S <: MainService: Tag](serviceResource: ResourceIO[S])
    //  (implicit scheduler: IORuntime)
    //: ResourceIO[S] =
    //  serviceResource
    //    .toAllocatedResource
    //    .flatTap(allocated =>
    //      shutdownHookResource[IO](name = allocated.allocatedThing.toString)(
    //        onJavaShutdown(allocated)))
    //    .map(_.allocatedThing)
    //
    //private def onJavaShutdown[S <: Service](allocatedService: Allocated[IO, S])
    //  (implicit s: IORuntime)
    //: Unit =
    //  logger.warn(s"Trying to shut down $allocatedService due to Java shutdown")
    //  val stop = logger.debugIO("onJavaShutDown stop service")(
    //    allocatedService.release)
    //  for t <- stop.attempt.unsafeRunSync().left do
    //    logger.error(s"onJavaShutdown: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
    //
    //private def shutdownHookResource[F[_] : Sync](name: String)(onJavaShutdown: => Unit)
    //: Resource[F, Unit] =
    //  Resource
    //    .fromAutoCloseable(Sync[F].delay(
    //      addJavaShutdownHook(name, () => onJavaShutdown)))
    //    .map(_ => ())
    //
    //private def addJavaShutdownHook(name: String, onJavaShutdown: () => Unit)
    //: AutoCloseable =
    //  JavaShutdownHook.add(name):
    //    try onJavaShutdown()
    //    catch
    //      case t: Throwable =>
    //        logger.debug(t.toStringWithCauses, t)
    //        throw t
    //    finally
    //      Log4j.shutdown()
