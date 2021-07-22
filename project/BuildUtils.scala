import BuildInfos.prettyVersion
import java.lang.ProcessBuilder.Redirect
import java.security.Security
import sbt.ModuleID
import scala.jdk.CollectionConverters._

object BuildUtils
{
  val isWindows = sys.props("os.name") startsWith "Windows"
  val isMac = sys.props("os.name") startsWith "Mac OS"

  val testParallelization: Int = {
    val factor = sys.props.get("test.parallel") match {
      case None => 1
      case Some("") => 1 * sys.runtime.availableProcessors
      case Some(o) if o.last == 'x' => (o.dropRight(1).toDouble * sys.runtime.availableProcessors + 0.01).toInt
      case Some(o) => o.toInt
    }
    if (factor != 1) println(s"build.sbt: test.parallel=$factor")
    factor
  }

  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)

  // Initial call to Logger for proper slf4j and log4j initialization ???
  logger.info(s"Building $prettyVersion, test.parallel=$testParallelization")

  if (sys.props("java.runtime.version") startsWith "1.8.0_15") {  // Special for Java 8u151 or Java 8u152 (delete this)
    Security.setProperty("crypto.policy", "unlimited")
  }

  //if (System.getProperty("js7.journal.slow-check-state") == null) {
    System.setProperty("js7.journal.slow-check-state", "on")
  //}

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] =
    o :: Nil

  implicit final class PercentModuleIDSeq(private val delegate: Seq[sbt.ModuleID])
  extends AnyVal {
    def %(configurations: String) = delegate.map(_ % configurations)
  }

  def runProcess(args: String*): Seq[String] = {
    val process = new java.lang.ProcessBuilder(args.toSeq.asJava)
      .redirectError(Redirect.INHERIT)
      .start()
    process.getErrorStream.close()
    val stdout = scala.io.Source.fromInputStream(process.getInputStream).getLines().toVector
    val exitCode = process.waitFor()
    if (exitCode != 0)
      throw new RuntimeException(s"Command failed: ${args.head}\n" + stdout.mkString("\n"))
    stdout
  }

  final class ProcessException(commandLine: String, returnCode: Int, stdout: String, stderr: String) extends RuntimeException
  {
    override def getMessage =
      s"""Command failed with exit code $returnCode
         |$commandLine
         |""".stripMargin +
        Seq(stderr, stdout).mkString("\n")
  }
}
