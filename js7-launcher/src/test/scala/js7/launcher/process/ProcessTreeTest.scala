package js7.launcher.process

import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.Stopwatch
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.Tests.isIntelliJIdea
import js7.launcher.process.ProcessTreeTest.*
import scala.jdk.OptionConverters.*
import scala.jdk.StreamConverters.*

final class ProcessTreeTest extends OurTestSuite:

  private lazy val processHandle =
    ProcessHandle.current.parent.toScala.getOrElse(ProcessHandle.current)

  "Speed" in:
    if isIntelliJIdea then
      val n = 10

      info("ProcessHandle.allProcesses: " +
        measureTime(10 * n, "calls", warmUp = 5):
          ProcessHandle.allProcesses)

      info("ProcessHandle.allProcesses.to(Vector): " +
        measureTime(10 * n, "calls", warmUp = 5):
          ProcessHandle.allProcesses.toScala(Vector))

      info("ProcessHandle.allProcesses.flatMap(_.parent ...): " +
        measureTime(n, "allProcesses", warmUp = 5):
          ProcessHandle.allProcesses.toScala(Vector)
            .flatMap(h => h.parent.toScala.map(_ -> h)))

      info("lazyProcessHandleToTree of a process: " +
        measureTime(n, "trees", warmUp = 5):
          lazyProcessHandleToTree(processHandle))

      info("lazyProcessHandleToTree of PID 1: " +
        measureTime(n, "trees", warmUp = 5):
          lazyProcessHandleToTree(ProcessHandle.of(1).get))

      info("eagerProcessHandleToTree of a process: " +
        measureTime(n, "trees", warmUp = 5):
          eagerProcessHandleToTree(processHandle))

      info("eagerProcessHandleToTree of PID 1:" +
        measureTime(n, "trees", warmUp = 5):
          eagerProcessHandleToTree(ProcessHandle.of(1).get))

      info("descendants of PID 1: " +
        measureTime(10 * n, "descendants", warmUp = 5):
          ProcessHandle.of(1).toScala.get.descendants.toScala(Vector))

  "Log trees"  - {
    "eagerProcessHandleToTree" in:
      log(eagerProcessHandleToTree(processHandle))

    "eagerProcessHandleToTree of PID 1" in:
      log(eagerProcessHandleToTree(ProcessHandle.of(1).toScala.get))
  }


object ProcessTreeTest:
  private val logger = Logger(getClass)

  private def log(tree: ProcessTree, indent: Int = 0): Unit =
    logger.info("  " * indent + tree.root.pid + " " + tree.root.info.command.toScala.getOrElse(""))
    for child <- tree.children do log(child, indent + 1)

  // Slow for big trees, fast for small trees
  private def lazyProcessHandleToTree(processHandle: ProcessHandle): ProcessTree =
    ProcessTree(processHandle, processHandle.children.toScala(Vector).map(lazyProcessHandleToTree))

  private def eagerProcessHandleToTree(processHandle: ProcessHandle): ProcessTree =
    val toChildren: Map[ProcessHandle, Vector[ProcessHandle]] =
      ProcessHandle.allProcesses.toScala(Vector)
        .flatMap(h => h.parent/*SLOW!!!*/.toScala.map(_ -> h))
        .groupMap(_._1)(_._2)

    def toTree(h: ProcessHandle): ProcessTree =
      ProcessTree(h, toChildren.getOrElse(h, Vector.empty).map(toTree))

    toTree(processHandle)


  private final case class ProcessTree(root: ProcessHandle, children: Vector[ProcessTree])
