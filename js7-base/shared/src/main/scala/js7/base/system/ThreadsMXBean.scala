package js7.base.system

import java.lang.management.{ManagementFactory, ThreadInfo}
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*

trait ThreadsMXBean:
  this: ThreadsMXBean.Bean.type =>

  //def getG1GCThreads: Int =
  //  computed().g1GCThreads

  def getFileSystemWatchThreads: Double =
    zeroToNaN(computed().fileSystemWatchThreads)

  def getCommonPoolThreads: Double =
    zeroToNaN(computed().commonPoolThreads)

  def getPekkoThreads: Double =
    zeroToNaN(computed().pekkoThreads)

  def getCatsEffectComputeThreads: Int =
    computed().catsEffectComputeThreads

  def getCatsEffectBlockingThreads: Int =
    computed().catsEffectBlockingThreads

  def getProcessReaperThreads: Double =
    zeroToNaN(computed().processReaperThreads)

  def getJettyThreads: Double =
    zeroToNaN(computed().jettyThreads)

  def getOtherThreads: Int =
    computed().otherThreads

  private def zeroToNaN(n: Int): Double =
    n match
      case 0 => Double.NaN
      case n => n.toDouble


object ThreadsMXBean:
  private val CacheDuration = 900.ms.toNanos
  //private val meter = CallMeter("ThreadsMXBean")

  object Bean extends ThreadsMXBean:
    private var lastTime = System.nanoTime() - 99.days.toNanos
    private var _computed: Computed = Computed(Array.empty)

    protected[ThreadsMXBean] def computed(): Computed =
      val t = System.nanoTime()
      val elapsed = t - lastTime
      lastTime = t
      if elapsed >= CacheDuration then
        //meter:
          val threads = ManagementFactory.getThreadMXBean.dumpAllThreads(false, false, 0)
          _computed = Computed(threads)
      _computed


  private final class Computed(threads: Array[ThreadInfo]):
    // Always 0
    //val g1GCThreads =
    //  countThreads(threads): name =>
    //    name.startsWith("G1 ") || name.startsWith("GC ") // These threads are not visible ???

    val fileSystemWatchThreads =
      countThreads(threads): name =>
        name.startsWith("FileSystemWatchService")

    val commonPoolThreads =
      countThreads(threads): name =>
        name.startsWith("ForkJoinPool.commonPool")

    val pekkoThreads =
      countThreads(threads): name =>
        name.startsWith("pekko-")

    val catsEffectComputeThreads =
      countThreads(threads): name =>
        name.length > 4 && name.startsWith("js7-") && name(4).isDigit

    val catsEffectBlockingThreads =
      countThreads(threads): name =>
        name.startsWith("js7-blocker-") || name.startsWith("js7-blocking-")

    val processReaperThreads =
      countThreads(threads): name =>
        name.startsWith("process reaper")

    val jettyThreads =
      countThreads(threads): name =>
        name.startsWith("qtp")

    val otherThreads =
      threads.length
        //- g1GCThreads
        - fileSystemWatchThreads
        - commonPoolThreads
        - pekkoThreads
        - catsEffectComputeThreads
        - catsEffectBlockingThreads
        - processReaperThreads
        - jettyThreads

    // threads argument avoids that threads sticks to the Bean
    private def countThreads[A](threads: Array[ThreadInfo])(predicate: String => Boolean): Int =
      threads.count(t => predicate(t.getThreadName))
