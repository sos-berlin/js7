package js7.base.thread

import java.util.concurrent.Semaphore
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.Tests.isIntelliJIdea
import scala.concurrent.TimeoutException
import scala.concurrent.duration.{Deadline, FiniteDuration, SECONDS}
import scala.util.control.NonFatal

final class VirtualThreadsTest extends OurTestSuite:
  // Start JVM with -Djs7.virtualThreads !!!
  // With a JVM still pinning blocked threads: -Djdk.virtualThreadScheduler.maxPoolSize=1000000

  // A million blocking threads with 1MiB stack size means 1TB of memory is needed.
  // But this memory would be needed without virtual threads, anyway.

  private val logger = Logger[this.type]

  "System properties" in:
    logger.info(s"js7.virtualThreads=${sys.props("js7.virtualThreads")}")
    logger.info(s"jdk.virtualThreadScheduler.maxPoolSize=${
      sys.props("jdk.virtualThreadScheduler.maxPoolSize")}")

  "Simple test without blocking threads" in:
    val n = 10_000
    runThreads(Vector.fill(n)(() => ()), timeout = 10.s)

  "Read stdout of processes in one virtual thread" in:
    readStdoutThreads(n = 1)

  "Read stdout of processes in 256 virtual threads" in:
    readStdoutThreads(n = 256)

  "Read stdout of processes in 257 virtual threads will fail" in:
    // Test may fail without -Djdk.virtualThreadScheduler.maxPoolSize=N, where N >= 257
    readStdoutThreads(n = 257)

  private lazy val logPendingingOnce: Unit =
    alert("Tests are pending because of not isIntelliJIdea")

  private def readStdoutThreads(n: Int): Unit =
    if !isIntelliJIdea then
      logPendingingOnce
      pending

    val processes =
      val pb = new ProcessBuilder("sleep", "99999")
      (1 to n).toVector.map(_ => pb.start)

    try
      runThreads(
        processes.map: process =>
          () =>
            // Read stdout of the process. This blocks the thread.
            val len = process.getInputStream.read()
            assert(len == -1 /*EOF*/),
        whenAllStarted = () => processes.foreach(_.toHandle.destroyForcibly()),
        timeout = 10.s)
    catch case NonFatal(t) =>
      processes.foreach(_.toHandle.destroyForcibly())
      processes.foreach(_.waitFor())
      throw t

    processes.foreach(_.waitFor())


  private def runThreads(
    runnables: Vector[() => Unit],
    whenAllStarted: () => Unit = () => (),
    timeout: FiniteDuration)
  : Unit =
    VirtualThreads.newVirtualThreadFactory match
      case None =>
        logger.info("❌ Virtual threads are not enabled • Start JVM with -Djs7.virtualThreads ❌")
        pending

      case Some(threadFactory) =>
        val n = runnables.size
        val t = Deadline.now
        val terminatedSemaphore = new Semaphore(0)
        val runningSemaphore = new Semaphore(0)
        val count = Atomic(0L)

        logger.info(s"Starting $n virtual threads")

        val threads = runnables.map: runnable =>
          val thread = threadFactory.newThread: () =>
            val cnt = count += 1
            try
              //Logger.infoCall(s"Virtual thread $cnt"):
                runningSemaphore.release()
                runnable()
                terminatedSemaphore.acquire()
            catch case NonFatal(t) =>
              logger.error(t.toString)
            finally
              count -= 1
          thread.start()
          thread

        // Wait until all threads are running
        val acquired = runningSemaphore.tryAcquire(n, timeout.toSeconds, SECONDS)
        if !acquired then
          throw new TimeoutException(s"Not all virtual threads started within $timeout")
        assert(count.get == n)
        logger.info:
          s"✔️  All $n virtual threads are running · " + itemsPerSecondString(t.elapsed, n)

        whenAllStarted()

        // Release the threads
        terminatedSemaphore.release(n)
        threads.foreach(_.join())
        assert(count.get == 0)
