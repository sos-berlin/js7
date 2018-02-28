package com.sos.jobscheduler.core.filebased

import cats.data.Validated.Valid
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedAddedOrChanged, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedVersion, RepoEvent, TypedPath}
import scala.collection.immutable
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class Repo private(
  versions: List[FileBasedVersion],
  pathToVersionToFileBased: Map[TypedPath, Map[FileBasedVersion, Option[FileBased]]])
{
  import Repo._

  lazy val currentVersion: Map[TypedPath, FileBased] =
    for {
      (path, versionToFileBased) ← pathToVersionToFileBased
      fileBasedOption ← versionToFileBased.fileBasedOption(versions)
      fileBased ← fileBasedOption
    } yield path → fileBased

  def currentFileBaseds: immutable.Iterable[FileBased] =
    currentVersion.values.toImmutableIterable

  assert(versions.nonEmpty || pathToVersionToFileBased.isEmpty)

  def applyEvents(events: Seq[RepoEvent]): Checked[Repo] = {
    var result = Checked(this)
    val iterator = events.iterator
    while (result.isValid && iterator.hasNext) {
      result = result flatMap (_.applyEvent(iterator.next()))
    }
    result
  }

  def applyEvent(event: RepoEvent): Checked[Repo] =
    event match {
      case VersionAdded(version) ⇒
        if (versions exists version.==)
          DuplicateVersionProblem(version)
        else
          Valid(copy(versions = version :: versions))

      case _ ⇒
        if (versions.isEmpty)
          Problem(s"Missing first event VersionAdded for Repo")
        else
          Valid(event match {
            case FileBasedAddedOrChanged(fileBased) ⇒
              addEntry(fileBased.path, Some(fileBased))

            case FileBasedDeleted(path) ⇒
              addEntry(path, None)
          })
    }

  private def addEntry(path: TypedPath, fileBasedOption: Option[FileBased]): Repo = {
    val version = versions.head
    copy(pathToVersionToFileBased =
      pathToVersionToFileBased
        .updated(path, pathToVersionToFileBased.getOrElse(path, Map.empty)
          .updated(version, fileBasedOption)))
  }

  def get(versioned: TypedPath.Versioned[TypedPath]): Checked[FileBased] = {
    for {
      versionToFileBased ← pathToVersionToFileBased.checked(versioned.path)
      fileBasedOption ← versionToFileBased.checked(versioned.version) orElse (
        for {
          history ← historyBefore(versioned.version)
          fb ← versionToFileBased.fileBasedOption(history) toChecked Problem(s"Not found: $versioned")
        } yield fb): Checked[Option[FileBased]]
      fileBased ← fileBasedOption.toChecked(DeletedProblem(versioned))
    } yield fileBased
  }

  def eventsFor(is: TypedPath.AnyCompanion ⇒ Boolean): Seq[RepoEvent] =
    toEvents collect {
      case e: VersionAdded ⇒ e
      case e: FileBasedEvent if is(e.path.companion) ⇒ e
    }

  def toEvents: Seq[RepoEvent] = {
    val versionToFileBasedOptions: Map[FileBasedVersion, Seq[Either[TypedPath, FileBased]]] =
      pathToVersionToFileBased.toVector
        .flatMap { case (path, versionToFileBasedOpt) ⇒
          versionToFileBasedOpt map { case (v, opt) ⇒ v → (opt map Right.apply getOrElse Left(path)) }
        }
        .groupBy(_._1).mapValues(_ map (_._2))
    versions.tails
      .map {
        case Nil ⇒  // Last of tails
          Vector.empty
        case version :: history ⇒
          Vector(VersionAdded(version)) ++
            versionToFileBasedOptions.getOrElse(version, Nil).map {
              case Left(path) ⇒
                FileBasedDeleted(path)
              case Right(fileBased) ⇒
                val vToF = pathToVersionToFileBased(fileBased.path)
                if (vToF.fileBasedOption(history).flatten.isDefined)
                  FileBasedChanged(fileBased)
                else
                  FileBasedAdded(fileBased)
            }
      }
      .toVector.reverse.flatten
  }

  //def currentVersionAsAddedEvents: Seq[FileBasedAdded] =
  //  currentVersion.values.map(FileBasedAdded.apply).toVector

  private[filebased] def historyBefore(version: FileBasedVersion): Checked[List[FileBasedVersion]] =
    versions.dropWhile(version.!=) match {
      case Nil ⇒ Problem(s"No such '$version'")
      case _ :: tail ⇒ Valid(tail)
    }
}

object Repo {
  val empty = new Repo(Nil, Map.empty)

  private implicit class RichVersionToFileBasedOption(private val versionToFileBasedOption: Map[FileBasedVersion, Option[FileBased]])
  extends AnyVal {
    /** None: no such version; Some(None): deleted */
    def fileBasedOption(history: List[FileBasedVersion]): Option[Option[FileBased]] =
      history.collectFirst { case v if versionToFileBasedOption contains v ⇒ versionToFileBasedOption(v) }
  }

  final case class DeletedProblem private[Repo](versionedPath: TypedPath.Versioned_)
    extends Problem.Lazy(s"Has been deleted: $versionedPath")

  final case class DuplicateVersionProblem private[Repo](version: FileBasedVersion)
    extends Problem.Lazy(s"Duplicate version '$version'")
}
