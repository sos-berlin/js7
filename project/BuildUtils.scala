import com.typesafe.sbt.GitPlugin.autoImport.git
import java.nio.ByteBuffer
import java.security.Security
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.{Instant, OffsetDateTime}
import java.util.{Base64, UUID}
import sbt.Keys.version
import sbt.{Def, ModuleID}
import scala.collection.immutable.ListMap

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

  private val isUncommitted =
    Def.setting(git.gitUncommittedChanges.value || git.gitHeadCommit.value.isEmpty/*no Git?*/)

  private val commitHash: Def.Initialize[Option[String]] =
    Def.setting(git.gitHeadCommit.value.filter(_.nonEmpty).orElse(sys.env.get("GIT_COMMIT"/*Jenkins?*/)))

  val longVersion: Def.Initialize[String] =
    Def.setting(
      if (isUncommitted.value)
        // "2.0.0-SNAPSHOT-UNCOMMITTED"
        version.value + "-UNCOMMITTED"
      else if (version.value endsWith "-SNAPSHOT") {
        // "2.0.0-SNAPSHOT-2019-01-14T1200-9abcdef"
        version.value + "-" + committedAt.value.getOrElse("?") +
          git.gitHeadCommit.value.fold("")(o => "-" + o.take(CommitHashLength))
      } else
        // "2.0.0-M1"
        version.value)

  private lazy val committedAt: Def.Initialize[Option[String]] =
    Def.setting(git.gitHeadCommitDate.value
      .map(o => parseInstant(o).toString)
      .map(_.take(16) + "Z"))

  private val prettyVersion: Def.Initialize[String] =
    Def.setting {
      val sb = new StringBuilder
      sb ++= version.value
      if (version.value endsWith "-SNAPSHOT") {
        val branch = git.gitCurrentBranch.value match {
          case o if o.isEmpty || git.gitHeadCommit.value.getOrElse("").startsWith(o) =>
            sys.env.getOrElse("GIT_BRANCH", "")  // Maybe set by Jenkins Git plugin
          case o => o
        }
        sb ++= " "
        sb ++= (branch +: commitHash.value.map(_ take CommitHashLength) ++: committedAt.value.toList)
          .filter(_.nonEmpty)
          .mkString("(", " ", ")")
      }
      if (isUncommitted.value) sb ++= " UNCOMMITTED " + Instant.now.toString.take(16) + "Z"
      sb.toString.trim
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
    "longVersion" -> BuildUtils.longVersion.value,
    "prettyVersion" -> BuildUtils.prettyVersion.value,
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
}
