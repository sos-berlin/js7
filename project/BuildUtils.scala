import com.typesafe.sbt.GitPlugin.autoImport.git
import java.nio.ByteBuffer
import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.{Instant, OffsetDateTime}
import java.util.{Base64, UUID}
import sbt.Keys.version
import sbt.{Def, ModuleID}

object BuildUtils
{
  val isWindows = sys.props("os.name") startsWith "Windows"
  val isMac = sys.props("os.name") startsWith "Mac OS"

  private val CommitHashLength = 7

  private val isUncommitted = Def.setting(git.gitUncommittedChanges.value || git.gitHeadCommit.value.isEmpty/*no Git?*/)
  private val commitDate: Def.Initialize[Option[String]] =
    Def.setting(
      if (git.gitUncommittedChanges.value) Some(Instant.now.toString)
      else git.gitHeadCommitDate.value)

  private val commitHash: Def.Initialize[Option[String]] =
    Def.setting(git.gitHeadCommit.value filter (_.nonEmpty) orElse sys.env.get("GIT_COMMIT"))

  val longVersion: Def.Initialize[String] =
    Def.setting(
      if (isUncommitted.value)
        // "2.0.0-SNAPSHOT-UNCOMMITTED"
        version.value + "-UNCOMMITTED"
      else if (version.value endsWith "-SNAPSHOT") {
        // "2.0.0-SNAPSHOT-2019-01-14T1200-9abcdef"
        val ts = git.gitHeadCommitDate.value.fold(Instant.now)(parseInstant).toString
        version.value + "-" + ts.take(13) + ts.substring(14, 16) + git.gitHeadCommit.value.fold("")(o ⇒ "-" + o.take(CommitHashLength))
      } else
        // "2.0.0-M1"
        version.value)

  val prettyVersion: Def.Initialize[String] =
    Def.setting {
      val sb = new StringBuilder
      sb ++= version.value
      if (version.value endsWith "-SNAPSHOT") {
        val branch = git.gitCurrentBranch.value match {
          case o if o.isEmpty || git.gitHeadCommit.value.getOrElse("").startsWith(o) ⇒
            sys.env.getOrElse("GIT_BRANCH", "")  // Maybe set by Jenkins Git plugin
          case o ⇒ o
        }
        sb ++= " "
        sb ++= (branch +: commitHash.value.map(_ take CommitHashLength) ++: commitDate.value.toList)
          .filter(_.nonEmpty)
          .mkString("(", " ", ")")
      }
      if (isUncommitted.value) sb ++= " UNCOMMITTED"
      sb.toString.trim
    }

  private val instantFormatter = new DateTimeFormatterBuilder().append(ISO_LOCAL_DATE_TIME).appendPattern("XX").toFormatter

  /** Parses 2019-01-14T12:00:00Z and 2019-01-14T13:00:00+01:00. */
  private def parseInstant(s: String) = OffsetDateTime.parse(s, instantFormatter).toInstant

  val buildId: String = {
    val uuid = UUID.randomUUID
    val buffer = ByteBuffer.wrap(new Array[Byte](16))
    buffer.putLong(uuid.getMostSignificantBits)
    buffer.putLong(uuid.getLeastSignificantBits)
    Base64.getUrlEncoder.encodeToString(buffer.array) stripSuffix "=="
  }

  implicit def singleModuleIDToList(o: sbt.ModuleID): List[ModuleID] = o :: Nil

  implicit final class PercentModuleIDSeq(private val delegate: Seq[sbt.ModuleID]) extends AnyVal {
    def %(configurations: String) = delegate map { _ % configurations }
  }
}
