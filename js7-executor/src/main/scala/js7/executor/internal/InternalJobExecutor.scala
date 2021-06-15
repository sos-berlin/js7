package js7.executor.internal

import cats.syntax.traverse._
import java.lang.reflect.Modifier.isPublic
import java.lang.reflect.{Constructor, InvocationTargetException}
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.utils.Classes.superclassesOf
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{InternalExecutable, JobConf, JobResource, JobResourcePath}
import js7.executor.ProcessOrder
import js7.executor.internal.InternalJob.{JobContext, Step}
import js7.executor.internal.InternalJobExecutor._
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

final class InternalJobExecutor(
  executable: InternalExecutable,
  val jobConf: JobConf,
  protected val pathToJobResource: JobResourcePath => Checked[JobResource],
  blockingJobScheduler: Scheduler)
  (implicit scheduler: Scheduler, iox: IOExecutor)
extends JobExecutor
{
  private val internalJobLazy = Lazy[Checked[InternalJob]](
    toInstantiator(executable.className)
      .flatMap(_()))

  val start: Task[Checked[Unit]] =
    Task { internalJobLazy() }
      .flatMapT(_.start)
      .tapEval {
        case Left(problem) => Task(
          logger.debug(s"${executable.className} start: $problem", problem.throwableOption.orNull))
        case Right(_) => Task.unit
      }
      .memoize

  val stop: Task[Unit] =
    Task.defer {
      internalJobLazy.fold(_ => Task.unit, _.stop)
    }.memoize

  def prepareOrderProcess(processOrder: ProcessOrder) =
    Task {
      for {
        internalJob <- internalJobLazy()
        step <- toStep(processOrder)
      } yield internalJob.toOrderProcess(step)
    }

  private def toStep(processOrder: ProcessOrder): Checked[InternalJob.Step] =
    for {
      args <- processOrder.scope.evaluator.evalExpressionMap(executable.arguments)
      resourceToArgs <- processOrder.jobResources
        .traverse(jobResource =>
          processOrder.scopeForJobResource.evaluator.evalExpressionMap(jobResource.settings)
            .map(jobResource.path -> _))
    } yield Step(
      processOrder,
      args,
      resourceToArgs.to(ListMap))

  private def toInstantiator(className: String): Checked[() => Checked[InternalJob]] =
    Checked.catchNonFatal(
      loadClass(className)
        .flatMap(cls =>
          if (classOf[InternalJob] isAssignableFrom cls)
            getConstructor(cls.asInstanceOf[Class[InternalJob]])
              .map(con => () => construct(con, toJobContext(cls)))
          else
            tryAdapter(cls))
    ).flatten

  private def tryAdapter(cls: Class[_]): Checked[() => Checked[InternalJob]] = {
    val internalJobs = superclassesOf(cls)
      .flatMap(cls =>
        Option(cls.getAnnotation(classOf[InternalJobAdapter])))
      .map(_.value)
    if (internalJobs.sizeIs > 1)
      Left(Problem(s"Class ${cls.getName} has multiple @InternalJobAdapter annotations"))
    else
      internalJobs
        .headOption
        .toChecked(Problem(s"Class '${cls.getName}' is not an InternalJob"))
        .flatMap(getConstructor)
        .map(con =>
          () => construct(con, toJobContext(cls)))
  }

  private def toJobContext(cls: Class[_]) =
    JobContext(cls, executable, jobConf, scheduler, iox, blockingJobScheduler)

  override def toString = s"InternalJobExecutor(${jobConf.jobKey} ${executable.className})"
}

object InternalJobExecutor
{
  private val logger = Logger(getClass)

  private def loadClass(className: String): Checked[Class[_ <: AnyRef]] =
    try Right(Class.forName(className).asInstanceOf[Class[_ <: AnyRef]])
    catch {
      case t: ExceptionInInitializerError =>
        Left(Problem.pure(Option(t.getCause).getOrElse(t).toStringWithCauses))
      case t: LinkageError =>
        Left(Problem.pure(t.toStringWithCauses))
      case NonFatal(t) =>
        Left(Problem.pure(t.toStringWithCauses))
    }

  private val isAllowedConstructorParamameterClass = Set[Class[_]](
    classOf[InternalJob.JobContext])

  private def getConstructor(clas: Class[_ <: InternalJob]): Checked[Constructor[InternalJob]] =
    Checked.catchNonFatal {
      val constructors = clas
        .getConstructors.asInstanceOf[Array[Constructor[InternalJob]]]
        .filter(o => isPublic(o.getModifiers))
      constructors
        .find(_.getParameterTypes.forall(isAllowedConstructorParamameterClass))
        .toChecked(Problem.pure(
          s"Class '${clas.getName}' does not have an appropriate public constructor (empty or InternalJob.JobContext)"))
    }.flatten

  private def construct(constructor: Constructor[InternalJob], jobContext: JobContext)
  : Checked[InternalJob] = {
    val args = constructor.getParameterTypes
      .map(cls =>
        if (cls isAssignableFrom classOf[InternalJob.JobContext])
          jobContext
        else
          sys.error(s"Unsupported constructor parameter: ${cls.getName}"))  // Should not happen
    try
      Right(constructor.newInstance(args: _*))
    catch {
      case t @ (_: InvocationTargetException | _: ExceptionInInitializerError) =>
        Left(Problem.fromThrowable(Option(t.getCause) getOrElse t))
      case NonFatal(t) =>
        Problem.fromThrowable(t)
    }
  }
}
