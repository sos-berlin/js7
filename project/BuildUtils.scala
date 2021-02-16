import com.typesafe.sbt.GitPlugin.autoImport.git
import java.lang.ProcessBuilder.Redirect
import java.nio.ByteBuffer
import java.security.Security
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.{Instant, OffsetDateTime}
import java.util.{Base64, UUID}
import sbt.Keys.version
import sbt.{Def, ModuleID}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object BuildUtils
{
  val isWindows = sys.props("os.name") startsWith "Windows"
  val isMac = sys.props("os.name") startsWith "Mac OS"

  private val CommitHashLength = 7
  private val toUrlBase64 = Base64.getUrlEncoder.withoutPadding.encodeToString _
  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)

  if (sys.props("java.runtime.version") startsWith "1.8.0_15") {  // Special for Java 8u151 or Java 8u152 (delete this)
    Security.setProperty("crypto.policy", "unlimited")
  }

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

  val gitBranch = Def.setting {
    var branch = git.gitCurrentBranch.value
    if (branch.isEmpty || git.gitHeadCommit.value.getOrElse("")/*commit hash*/.startsWith(branch)) {
      branch = sys.env.getOrElse("GIT_BRANCH", "")  // Maybe set by Jenkins Git plugin
    }
    branch
  }

  private val isUncommitted =
    Def.setting(git.gitUncommittedChanges.value || git.gitHeadCommit.value.isEmpty/*no Git?*/)

  private val commitHash: Def.Initialize[Option[String]] =
    Def.setting(git.gitHeadCommit.value.filter(_.nonEmpty).orElse(sys.env.get("GIT_COMMIT"/*Jenkins?*/)))

  val longVersion: Def.Initialize[String] =
    Def.setting(
      if (isUncommitted.value)
        // "2.0.0-SNAPSHOT+UNCOMMITTED.20210127.1200"
        version.value + "+UNCOMMITTED." +
          Instant.now.toString
            .filter(c => c != '-' && c != ':')
            .take(13)
            .replace('T', '.')
      else if (version.value endsWith "-SNAPSHOT")
        // "2.0.0-SNAPSHOT+9abcdef"
        version.value + commitHash.value.fold("")("+" + _.take(CommitHashLength))
      else
        // "2.0.0-M1"
        version.value)

  /** Git commit date as "yyyy-mm-ddThh:mmZ". */
  lazy val committedAt: Def.Initialize[Option[String]] =
    Def.setting(git.gitHeadCommitDate.value
      .map(o => parseInstant(o).toString)
      .map(_.take(13) + "Z"))

  private val prettyVersion: Def.Initialize[String] =
    Def.setting {
      val sb = new StringBuilder
      sb ++= longVersion.value
      val extras = mutable.Buffer.empty[String]
      if (version.value endsWith "-SNAPSHOT") {
        extras += gitBranch.value + " branch"
        if (!longVersion.value.contains("+UNCOMMITTED.")) {
          extras += Instant.now.toString.take(16) + "Z"  // yyyy-mm-ddThh:mm
        }
      } else {
        committedAt.value.foreach(extras += _.take(10))
      }
      if (extras.nonEmpty) {
        sb.append(extras.mkString(" (", " ", ")"))
      }
      sb.toString
    }

  val buildId: String = {
    val uuid = UUID.randomUUID
    val buffer = ByteBuffer.wrap(new Array[Byte](16))
    buffer.putLong(uuid.getMostSignificantBits)
    buffer.putLong(uuid.getLeastSignificantBits)
    toUrlBase64(buffer.array)
  }

  val buildInfoMap = Def.setting(ListMap[String, Any](
    "buildTime" -> System.currentTimeMillis,
    "buildId" -> buildId,
    "version" -> version.value,
    "longVersion" -> longVersion.value,
    "prettyVersion" -> prettyVersion.value,
    "commitId" -> git.gitHeadCommit.value,
    "commitMessage" -> git.gitHeadMessage.value))

  private val instantFormatter = new DateTimeFormatterBuilder()
    .append(ISO_LOCAL_DATE_TIME)
    .appendPattern("XX")
    .toFormatter

  /** Parses 2019-01-14T12:00:00Z and 2019-01-14T13:00:00+01:00. */
  private def parseInstant(s: String) =
    OffsetDateTime.parse(s, instantFormatter).toInstant

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] =
    o :: Nil

  implicit final class PercentModuleIDSeq(private val delegate: Seq[sbt.ModuleID])
  extends AnyVal {
    def %(configurations: String) = delegate.map(_ % configurations)
  }

  // Initial call to Logger for proper slf4j and log4j initialization ???
  logger.info(s"Building $prettyVersion, test.parallel=$testParallelization")

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
