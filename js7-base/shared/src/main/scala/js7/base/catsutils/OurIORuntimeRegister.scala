package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, Sync}
import java.util.concurrent.ConcurrentHashMap
import js7.base.log.Logger
import scala.concurrent.ExecutionContext

/** Used for Environment and for tests having their own IORuntimes. */
object OurIORuntimeRegister:

  private val logger = Logger[this.type]
  private val ecToRuntime = new ConcurrentHashMap[ExecutionContext, Entry]

  def toIORuntime(compute: ExecutionContext): IORuntime =
    toEntry(compute).ioRuntime

  def register[F[_]](ioRuntime: IORuntime)(using F: Sync[F]): Resource[F, Unit] =
    Resource(F.delay:
      add(ioRuntime) -> F.delay(remove(ioRuntime)))

  def add(ioRuntime: IORuntime): Unit =
    ecToRuntime.put(ioRuntime.compute, Entry(ioRuntime))

  def remove(ioRuntime: IORuntime): Unit =
    ecToRuntime.remove(ioRuntime)

  private[catsutils] def environment: IO[Environment] =
    entry.map(_.enviromment)

  private[catsutils] def toEnvironment(ioRuntime: IORuntime): Environment =
    toEntry(ioRuntime.compute).enviromment

  private def entry: IO[Entry] =
    IO.executionContext.map(toEntry)

  private def toEntry(compute: ExecutionContext): Entry =
    ecToRuntime.get(compute) match
      case null =>
        val msg = "Current IORuntime is not registered in OurIORuntimeRegister"
        logger.error(msg)
        throw new RuntimeException(msg)
      case o => o

  private final class Entry(val ioRuntime: IORuntime):
    val enviromment = new Environment
