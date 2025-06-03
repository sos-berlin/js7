package js7.launcher.forjava.internal

import cats.effect.unsafe.IORuntime
import java.io.Writer
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.launcher.StdWriter

private final class BlockingStdWriter(stdWriter: StdWriter)(using IORuntime) extends Writer:

  def write(array: Array[Char], offset: Int, len: Int): Unit =
    write(new String(array, offset, len))

  override def write(string: String): Unit =
    stdWriter.write(string).awaitInfinite

  /** This class doesn't buffer. */
  def flush(): Unit = {}

  /** Does nothing, because stdout and stdin are closed by the JS7 Engine. */
  def close(): Unit = {}
