package js7.executor.forjava.internal

import java.io.{PrintWriter, Writer}
import javax.annotation.Nonnull
import js7.base.utils.Lazy
import js7.executor.forjava.internal.BlockingInternalJob._
import js7.executor.internal.InternalJob.{JobContext, OrderContext}
import js7.executor.internal.InternalJobAdapter

/** For non-asynchronous thread-blocking internal Jobs written in Java.
  * Constructor and methods are executed in (from call to call changing) threads
  * out of a thread pool for blocking execution. */
@InternalJobAdapter(classOf[BlockingInternalJobAdapter])
trait BlockingInternalJob
{
  /** Reserved. */
  final def start() = {}

  /** Reserved. */
  final def stop() = {}

  /** Process the order in a seperate thread. */
  @throws[Exception]
  @Nonnull
  def processOrder(@Nonnull context: JOrderContext): JOrderResult
}

object BlockingInternalJob
{
  final case class JJobContext(asScala: JobContext)
  extends JavaJobContext

  final case class JOrderContext(asScala: OrderContext, outWriter: Writer, errWriter: Writer)
  extends JavaOrderContext
  {
    private lazy val outLazy = Lazy(new PrintWriter(outWriter, true))
    private lazy val errLazy = Lazy(new PrintWriter(errWriter, true))

    lazy val out: PrintWriter = outLazy()
    lazy val err: PrintWriter = errLazy()

    private[internal] def close(): Unit = {
      for (o <- outLazy) o.close()
      for (o <- errLazy) o.close()
    }
  }
}
