package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, Sync}
import java.util.concurrent.{ConcurrentHashMap, Executor}
import js7.base.log.Logger
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.control.NonFatal

/** Used for Environment and for tests having their own IORuntimes. */
object OurIORuntimeRegister:

  private lazy val logger = Logger[this.type]
  private val ecToRuntime = new ConcurrentHashMap[ExecutionContext, Entry]

  def toIORuntime(ec: ExecutionContext): IORuntime =
    toEntry(ec).ioRuntime

  def register[F[_]](ioRuntime: IORuntime)(using F: Sync[F]): Resource[F, Unit] =
    Resource(F.delay:
      add(ioRuntime)
      () -> F.delay(remove(ioRuntime)))

  private def add(ioRuntime: IORuntime): Unit =
    ecToRuntime.put(ioRuntime.compute, Entry(ioRuntime))

  private def remove(ioRuntime: IORuntime): Unit =
    ecToRuntime.remove(ioRuntime)

  private[catsutils] def environment: IO[Environment] =
    entry.map(_.enviromment)

  def toEnvironment(ioRuntime: IORuntime): Environment =
    toEntry(ioRuntime.compute).enviromment

  private def entry: IO[Entry] =
    IO.executionContext.map(toEntry)

  private def toEntry(ec: ExecutionContext): Entry =
    ecToRuntime.get(ec) match
      case null =>
        val msg = "Current ExecutionContext is not registered in OurIORuntimeRegister: " +
          ecToLabel(ec)
        logger.error(msg)
        throw new RuntimeException(msg)
      case o => o

  private def ioRuntimeToLabel(ioRuntime: IORuntime): String =
    ecToLabel(ioRuntime.compute)

  private def ecToLabel(ec: ExecutionContext): String =
    ec match
      case ec: ExecutionContextExecutor =>
        try
          // cats.effect.unsafe.WorkStealingThreadPool ?
          ec.getClass.getDeclaredMethod("threadPrefix").invoke(ec).asInstanceOf[String]
        catch case NonFatal(_) =>
          try
            // scala.concurrent.impl.ExecutionContextImpl ?
            ec.getClass.getDeclaredMethod("executor").invoke(ec).asInstanceOf[Executor].toString
          catch case NonFatal(_) => ec.toString
      case _ => ec.toString


  private final class Entry(val ioRuntime: IORuntime):
    val enviromment = new Environment(label = ioRuntimeToLabel(ioRuntime))
