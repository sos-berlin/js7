import com.typesafe.sbt.GitPlugin.autoImport.git
import java.nio.ByteBuffer
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.{Instant, OffsetDateTime}
import java.util.{Base64, UUID}
import sbt.Def
import sbt.Keys.version
import scala.collection.immutable.ListMap
import scala.collection.mutable

object BuildInfos
{
  private val CommitHashLength = 7
  private val toUrlBase64 = Base64.getUrlEncoder.withoutPadding.encodeToString _

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

  val prettyVersion: Def.Initialize[String] =
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

  val buildPropertiesString: Def.Initialize[String] =
    Def.setting(
      buildInfoMap.value
        .mapValues {
          case None => ""
          case Some(v) => v.toString.trim
          case v => v.toString.trim
        }
        .map { case (k, v) => s"build.$k=$v\n" }
        .mkString)

  private val instantFormatter = new DateTimeFormatterBuilder()
    .append(ISO_LOCAL_DATE_TIME)
    .appendPattern("XX")
    .toFormatter

  /** Parses 2019-01-14T12:00:00Z and 2019-01-14T13:00:00+01:00. */
  private def parseInstant(s: String) =
    OffsetDateTime.parse(s, instantFormatter).toInstant
}
