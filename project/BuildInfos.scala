import com.typesafe.sbt.GitPlugin.autoImport.git
import java.nio.ByteBuffer
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.{Instant, OffsetDateTime}
import java.util.{Base64, UUID}
import sbt.Def
import sbt.Keys.version
import scala.collection.immutable.ListMap

object BuildInfos
{
  private val CommitHashLength = 7
  private val toUrlBase64 = Base64.getUrlEncoder.withoutPadding.encodeToString _
  private val now = Instant.now()

  val versionIsTagged = Def.setting {
    git.gitCurrentTags.value.contains("v" + version.value)
  }

  private val shortCommitHash: Def.Initialize[String] =
    Def.setting(
      git.gitHeadCommit.value.filter(_.nonEmpty)
        .orElse(sys.env.get("GIT_COMMIT" /*Jenkins?*/))
        .getOrElse("")
        .take(CommitHashLength))

  private val branch = Def.setting {
    val branch = git.gitCurrentBranch.value
    if (branch.isEmpty || shortCommitHash.value.startsWith(branch))
      sys.env.getOrElse("GIT_BRANCH", "") // Maybe set by Jenkins Git plugin
    else
      branch
  }

  private val isUncommitted = Def.setting {
    val isUncommitted = git.gitUncommittedChanges.value || git.gitHeadCommit.value.isEmpty/*no Git?*/
    if (isUncommitted && !version.value.endsWith("-SNAPSHOT")) {
        println(
          s"❓ Uncommitted files but version does not ends with -SNAPSHOT: ${version.value} ❓")
      }
    isUncommitted
  }

  /** Git commit date as "yyyy-mm-ddThh:mmZ". */
  lazy val committedAt: Def.Initialize[Option[String]] =
    Def.setting(git.gitHeadCommitDate.value
      .map(o => parseInstant(o).toString)
      .map(_.take(13) + "Z"))

  lazy val info: Def.Initialize[Info] = Def.setting {
    val versionIsTagged = git.gitCurrentTags.value.contains("v" + version.value)
    if (isUncommitted.value)
      new Uncommitted(version.value, branch = branch.value,
        commitHash = git.gitHeadCommit.value.getOrElse(""))
    else if (!versionIsTagged) {
      if (!version.value.contains("-SNAPSHOT")) {
        println(s"❗ Commit is not tagged with v${version.value} ❗")
      }
      new Untagged(version.value, branch = branch.value, commitHash = shortCommitHash.value)
    } else
      new ReproducibleRelease(version.value, branch = branch.value,
        commitHash = shortCommitHash.value)
  }

  sealed trait Info {
    def version: String
    def longVersion: String
    def prettyVersion: String
    def commitHash: String
    def buildId: String

    final lazy val buildInfoMap = ListMap[String, Any](
      "version" -> version,
      "longVersion" -> longVersion,
      "prettyVersion" -> prettyVersion,
      "buildId" -> buildId,
      "commitId" -> commitHash)

    final lazy val buildPropertiesString: String =
      buildInfoMap
        .mapValues {
          case None => ""
          case Some(v) => v.toString.trim
          case v => v.toString.trim
        }
        .map { case (k, v) => s"build.$k=$v\n" }
        .mkString
  }

  final class ReproducibleRelease(val version: String, branch: String, val commitHash: String)
  extends Info {
    val longVersion =
      version

    val prettyVersion =
      longVersion + (if (branch == "main") "" else s" ($branch)")

    val buildId =
      longVersion
  }

  final class Untagged(val version: String, branch: String, val commitHash: String)
  extends Info {
    val longVersion =
      s"$version+$commitHash"

    val prettyVersion =
      longVersion + (
        if (version.contains("-SNAPSHOT") && branch.nonEmpty)
          s" ($branch)"
        else "")

    val buildId =
      longVersion
  }

  /** Uncommitted contains build time dependent values. Not for release versions. */
  final class Uncommitted(val version: String, val branch: String, val commitHash: String)
  extends Info {
    /** "2.0.0+UNCOMMITTED.20210127.120000" */
    val longVersion =
      version + "+UNCOMMITTED." +
        now.toString
          .filter(c => c != '-' && c != ':')
          .take(13)
          .replace('T', '.')

    val prettyVersion =
      s"$longVersion (${now.toString.take(15) + "Z"})"

    val buildId = {
      val uuid = UUID.randomUUID
      val buffer = ByteBuffer.wrap(new Array[Byte](16))
      buffer.putLong(uuid.getMostSignificantBits)
      buffer.putLong(uuid.getLeastSignificantBits)
      toUrlBase64(buffer.array)
    }
  }

  private val instantFormatter = new DateTimeFormatterBuilder()
    .append(ISO_LOCAL_DATE_TIME)
    .appendPattern("XX")
    .toFormatter

  /** Parses 2019-01-14T12:00:00Z and 2019-01-14T13:00:00+01:00. */
  private def parseInstant(s: String) =
    OffsetDateTime.parse(s, instantFormatter).toInstant
}
