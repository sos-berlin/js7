package js7.launcher.internal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.lang.reflect.Modifier.isPublic
import java.lang.reflect.{Constructor, InvocationTargetException}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.Classes.superclassesOf
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{InternalExecutable, JobConf}
import js7.data.value.NamedValues
import js7.data.value.expression.Scope.evalExpressionMap
import js7.launcher.internal.InternalJob.{JobContext, Step}
import js7.launcher.internal.InternalJobLauncher.*
import js7.launcher.{OrderProcess, ProcessOrder}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

final class InternalJobLauncher(
  executable: InternalExecutable,
  val jobConf: JobConf,
  val jobArguments: NamedValues,
  blockingJobEC: ExecutionContext,
  clock: AlarmClock)
  (implicit ioRuntime: IORuntime, iox: IOExecutor)
extends JobLauncher:

  private val internalJobLazy = Lazy[Checked[InternalJob]](
    toInstantiator(executable.className)
      .flatMap(_()))

  val start: IO[Checked[Unit]] =
    IO { internalJobLazy() }
      .flatMapT: internalJob =>
        logger.traceIO(s"${executable.className} start"):
          internalJob.start
      .flatTap:
        case Left(problem) => IO(
          logger.debug(s"${executable.className} start: $problem", problem.throwableOption.orNull))
        case Right(_) => IO.unit
      .unsafeMemoize

  val stop: IO[Unit] =
    IO.defer:
      internalJobLazy.value.fold(_ => IO.unit, internalJob =>
        logger.traceIO(s"${executable.className} stop"):
          internalJob.stop)
    .unsafeMemoize

  def toOrderProcess(processOrder: ProcessOrder): IO[Checked[OrderProcess]] =
    IO:
      for
        internalJob <- internalJobLazy()
        step <- toStep(processOrder)
      yield
        internalJob.toOrderProcess(step)

  private def toStep(processOrder: ProcessOrder): Checked[InternalJob.Step] =
    for args <- evalExpressionMap(executable.arguments, processOrder.scope) yield
      Step(processOrder, args)

  private def toInstantiator(className: String): Checked[() => Checked[InternalJob]] =
    catchNonFatalFlatten:
      loadClass(className)
        .flatMap: cls =>
          if classOf[InternalJob].isAssignableFrom(cls) then
            getConstructor(cls.asInstanceOf[Class[InternalJob]])
              .map(con => () => construct(con, toJobContext(cls)))
          else
            tryAdapter(cls)

  private def tryAdapter(cls: Class[?]): Checked[() => Checked[InternalJob]] =
    val internalJobs = superclassesOf(cls)
      .flatMap: cls =>
        Option(cls.getAnnotation(classOf[InternalJobAdapter]))
      .map(_.value)
    if internalJobs.sizeIs > 1 then
      Left(Problem(s"Class ${cls.getName} has multiple @InternalJobAdapter annotations"))
    else
      internalJobs
        .headOption
        .toChecked(Problem(s"Class '${cls.getName}' is not an InternalJob"))
        .flatMap(getConstructor)
        .map(con =>
          () => construct(con, toJobContext(cls)))

  private def toJobContext(cls: Class[?]) =
    JobContext(cls, executable, jobArguments, jobConf, ioRuntime, iox, blockingJobEC, clock,
      jobConf.systemEncoding)

  override def toString = s"InternalJobLauncher(${jobConf.jobKey} ${executable.className})"


object InternalJobLauncher:
  private val logger = Logger[this.type]

  private def loadClass(className: String): Checked[Class[? <: AnyRef]] =
    try Right(Class.forName(className).asInstanceOf[Class[? <: AnyRef]])
    catch
      case t: ExceptionInInitializerError =>
        Left(Problem.pure(Option(t.getCause).getOrElse(t).toStringWithCauses))
      case t: LinkageError =>
        Left(Problem.pure(t.toStringWithCauses))
      case NonFatal(t) =>
        Left(Problem.pure(t.toStringWithCauses))

  private val isAllowedConstructorParamameterClass = Set[Class[?]](
    classOf[InternalJob.JobContext])

  private def getConstructor(clas: Class[? <: InternalJob]): Checked[Constructor[InternalJob]] =
    catchNonFatalFlatten:
      val constructors = clas
        .getConstructors.asInstanceOf[Array[Constructor[InternalJob]]]
        .filter(o => isPublic(o.getModifiers))
      constructors
        .find(_.getParameterTypes.forall(isAllowedConstructorParamameterClass))
        .toChecked(Problem.pure(
          s"Class '${clas.getName}' does not have an appropriate public constructor (empty or InternalJob.JobContext)"))

  private def construct(constructor: Constructor[InternalJob], jobContext: JobContext)
  : Checked[InternalJob] =
    val args = constructor.getParameterTypes
      .map(cls =>
        if cls.isAssignableFrom(classOf[InternalJob.JobContext]) then
          jobContext
        else
          sys.error(s"Unsupported constructor parameter: ${cls.getName}"))  // Should not happen
    try
      Right(constructor.newInstance(args*))
    catch
      case t @ (_: InvocationTargetException | _: ExceptionInInitializerError) =>
        Left(Problem.fromThrowable(Option(t.getCause) getOrElse t))
      case NonFatal(t) =>
        Problem.fromThrowable(t)
