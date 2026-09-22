package js7.base.io.process

import cats.effect.IO
import scala.jdk.OptionConverters.*

object ProcessExtensions:

  extension (process: Pid | Js7Process)
    def toPid: Pid =
      process match
        case pid: Pid => pid
        case process: Js7Process => process.pid

    def isAlive: Boolean =
      process match
        case pid: Pid => pid.maybeProcessHandle.exists(_.isAlive)
        case process: Js7Process => process.isAlive

    def maybeProcessHandle: Option[ProcessHandle] =
      process match
        case pid: Pid => ProcessHandle.of(pid.number).toScala
        case process: Js7Process => process.maybeHandle


  extension (processHandle: ProcessHandle)
    /** Java 26: **onExit MAY BLOCK WHILE BEING BLOCKED READING STDOUT/STDERR.** !!!
      *
      * USE ONLY WHILE NOT READING STDOUT OR STDERR! Otherwise, it could block.
      *
      * Prefer waitFor or [[Js7Process#ourOnExit]].
      *
      * ## Why onExit fails while reading stdout — *according to Antrophic Claude*
      *
      * `java.lang.ProcessImpl.initStreams` registers a *synchronous* `handle(...)` on the same
      * `ExitCompletion` that `ProcessHandle.onExit()` hangs off.
      * That handler calls `ProcessPipeInputStream.processExited()`,
      * which is `synchronized` on the stream (`ProcessImpl.java:590`).
      *
      * Your `pumpStdouterrToSink` is blocked in `channel.read()` on that very
      * `ProcessPipeInputStream` — deliberately, since `waitForEndOfProcessAndStdouterr`'s contract
      * is to keep reading past process termination to catch the background child.
      * A blocked `BufferedInputStream.read()` **holds that monitor** (`ProcessPipeInputStream`
      * is a subclass, so JDK 21+'s ReentrantLock retrofit doesn't apply — it falls back to the
      * object monitor).
      * So `processExited()` blocks for the child's whole lifetime: 20 s.
      *
      * Normally the reaper thread eats that block and `onExit`'s async dependent, pushed later,
      * is popped first.
      * But `CompletableFuture.postFire` makes the async worker *help* drain the source's
      * stack before running its own dependents.
      * Caught in the act:
      *
      * ```
      * ForkJoinPool.commonPool-worker-1  BLOCKED
      *   ProcessPipeInputStream.processExited(ProcessImpl.java:590) <- blocked 20s
      *   ProcessImpl.lambda$initStreams$0(ProcessImpl.java:353)
      *   CompletableFuture.postComplete(531)
      *   CompletableFuture.postFire(629) <- "helping" the source's leftover stack
      *   CompletableFuture$UniHandle.tryFire(935) <- YOUR onExit dependent, already completed
      *   ForkJoinPool.runWorker
      * ```
      *
      * So whether `unsafeOnExitIO` fires at 1 s or 20 s is a pure race between the reaper thread
      * and the common-pool worker over who runs the blocking `initStreams` handler. With `n = 1`
      * the window is effectively never hit; with `n = 10` you lose it once or twice per run.
      * `t.elapsed < childSleep` then fails because the order isn't processed until
      * the child dies at 20 s.
      */
    def unsafeOnExitIO: IO[Unit] =
      IO.fromCompletableFuture:
        IO:
          processHandle.onExit()
      .void
