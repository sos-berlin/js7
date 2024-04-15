import java.lang.ProcessBuilder.Redirect
import java.nio.file.Files.{createDirectories, deleteIfExists}
import java.nio.file.Paths
import sbt.{ModuleID, ProjectReference}
import sbtcrossproject.CrossPlugin.autoImport.JVMCrossProjectOps
import sbtcrossproject.CrossProject
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

object BuildUtils
{
  createDirectories(Paths.get("logs"))
  //Files.list(Paths.get("logs")).toArray(new Array[Path](_)).foreach(Files.delete)
  deleteIfExists(Paths.get("logs/build.log"))

  val isWindows: Boolean = sys.props("os.name") startsWith "Windows"
  val isMac: Boolean = sys.props("os.name") startsWith "Mac OS"

  val testParallelization: Int = {
    val factor: Int = sys.props.get("test.parallel") match {
      case None => 1
      case Some("") => BuildCPU.testParallelization
      case Some(o) if o.last == 'x' =>
        (BuildCPU.testParallelization * o.dropRight(1).toDouble + 0.01).toInt
      case Some(o) => o.toInt
    }
    if (factor != 1) println(s"build.sbt: test.parallel=$factor")
    factor
  }

  private val logger = org.slf4j.LoggerFactory.getLogger("BuildUtils")

  // Initial call to Logger for proper slf4j and log4j initialization ???
  logger.info(s"test.parallel=$testParallelization")
  logger.debug(s"cpu=${BuildCPU.cpu}")

  System.setProperty("js7.test", "on")

  // IntelliJ does not compile (`js7-tester` % "test") when used by js7-base ???
  // It compiles when "test" is changed to "compile".
  val isIntelliJIdea: Boolean = sys.props.get("idea.managed").contains("true")
  val testWhenIntelliJ: String = if (isIntelliJIdea) "compile" else "test"

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] =
    o :: Nil

  implicit final class PercentModuleIDSeq(private val delegate: Seq[sbt.ModuleID])
  extends AnyVal {
    def %(configurations: String): Seq[ModuleID] = 
      delegate.map(_ % configurations)
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
    override def getMessage: String =
      s"""Command failed with exit code $returnCode
         |$commandLine
         |""".stripMargin +
        Seq(stderr, stdout).mkString("\n")
  }

  object Implicit {
    implicit def crossProjectToJvmProjectReference(project: CrossProject): ProjectReference =
      project.jvm
  }
}
