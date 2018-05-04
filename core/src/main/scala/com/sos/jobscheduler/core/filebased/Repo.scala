package com.sos.jobscheduler.core.filebased

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichTraversable, RichTraversableOnce}
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.common.scalautil.Memoizer
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedAddedOrChanged, FileBasedChanged, FileBasedDeleted, FileBasedEvent, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedId_, RepoEvent, TypedPath, VersionId}
import scala.collection.immutable
import scala.collection.immutable.Seq

/**
  * Representation of versioned FileBased (configuration objects).
  * @author Joacim Zschimmer
  */
final case class Repo private(versions: List[VersionId], idToFileBased: Map[FileBasedId_, Option[FileBased]])
{
  assert(versions.nonEmpty || idToFileBased.isEmpty)

  import Repo._

  lazy val pathToVersionToFileBased: Map[TypedPath, Map[VersionId, Option[FileBased]]] =
    idToFileBased.map { case (id, fileBased) ⇒ (id.path, id.versionId, fileBased) }
      .groupBy (_._1)
      .mapValues (_ toKeyedMap (_._2) mapValues (_._3))

  lazy val currentVersion: Map[TypedPath, FileBased] =
    for {
      (path, versionToFileBased) ← pathToVersionToFileBased
      fileBasedOption ← versionToFileBased.fileBasedOption(versions)
      fileBased ← fileBasedOption
    } yield path → fileBased

  private val typeToPathToCurrentFileBased: FileBased.Companion_ ⇒ Map[TypedPath, FileBased] =
    Memoizer { companion ⇒
      currentVersion collect {
        case (path, fileBased) if fileBased.companion == companion ⇒
          path → fileBased
      }
    }

  def onlyCurrentVersion: Repo =
    Repo(
      versions.head :: Nil,
      currentVersion.values.map(fileBased ⇒ fileBased.id → Some(fileBased)).toMap)

  def currentTyped[A <: FileBased](implicit A: FileBased.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentFileBased(A).asInstanceOf[Map[A#Path, A]]

  def versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  def currentFileBaseds: immutable.Iterable[FileBased] =
    currentVersion.values.toImmutableIterable

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

      case event: FileBasedEvent ⇒
        if (versions.isEmpty)
          Problem(s"Missing first event VersionAdded for Repo")
        else
          event match {
            case event @ FileBasedAddedOrChanged(fileBased) ⇒
              if (fileBased.id.versionId != versionId)
                Problem(s"Version in ${event.toShortString} does not match current version '${versionId.string}'")
              else
                Valid(addEntry(fileBased.path, Some(fileBased)))

            case FileBasedDeleted(path) ⇒
              Valid(addEntry(path, None))
          }
    }

  private def addEntry(path: TypedPath, fileBasedOption: Option[FileBased]): Repo = {
    val version = versions.head
    copy(idToFileBased = idToFileBased + ((path % version) → fileBasedOption))
  }

  def pathToCurrentId[P <: TypedPath](path: P): Checked[FileBasedId[P]] =
    for {
      vToF ← pathToVersionToFileBased.checked(path)
      o ← vToF.fileBasedOption(versions) toChecked Problem(s"No such path '$path'")/*should no happen*/
      fileBased ← o toChecked Problem(s"Has been deleted: $path")
    } yield fileBased.id.asInstanceOf[FileBasedId[P]]

  /** Returns the FileBased to a FileBasedId. */
  def idTo[A <: FileBased](id: FileBasedId[A#Path])(implicit A: FileBased.Companion[A]): Checked[A] =
    for {
      versionToFileBased ← pathToVersionToFileBased.checked(id.path)
      fileBasedOption ← versionToFileBased.checked(id.versionId) orElse (
        for {
          history ← historyBefore(id.versionId)
          fb ← versionToFileBased.fileBasedOption(history) toChecked Problem(s"No such '$id'")
        } yield fb): Checked[Option[FileBased]]
      fileBased ← fileBasedOption.toChecked(DeletedProblem(id))
    } yield fileBased.cast[A]

  /** Converts the Repo to an event sequence, regarding only a given type. */
  def eventsFor(is: TypedPath.AnyCompanion ⇒ Boolean): Seq[RepoEvent] =
    toEvents collect {
      case e: VersionAdded ⇒ e
      case e: FileBasedEvent if is(e.path.companion) ⇒ e
    }

  /** Converts the Repo to an event sequence. */
  private[filebased] def toEvents: Seq[RepoEvent] = {
    val versionToFileBasedOptions: Map[VersionId, Seq[Either[FileBased/*added/changed*/, TypedPath/*deleted*/]]] =
      pathToVersionToFileBased.toVector.sortBy(_._1)/*for testing*/
        .flatMap { case (path, versionToFileBasedOpt) ⇒
          versionToFileBasedOpt map { case (v, opt) ⇒ v → (opt map Left.apply getOrElse Right(path)) }
        }
        .groupBy(_._1).mapValues(_ map (_._2))
    versions.tails
      .map {
        case Nil ⇒  // Last of tails
          Vector.empty
        case version :: history ⇒
          Vector(VersionAdded(version)) ++
            versionToFileBasedOptions.getOrElse(version, Nil).map {
              case Left(fileBased) ⇒
                val vToF = pathToVersionToFileBased(fileBased.path)
                if (vToF.fileBasedOption(history).flatten.isDefined)
                  FileBasedChanged(fileBased)
                else
                  FileBasedAdded(fileBased)
              case Right(path) ⇒
                FileBasedDeleted(path)
            }
      }
      .toVector.reverse.flatten
  }

  private[filebased] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versions.dropWhile(versionId.!=) match {
      case Nil ⇒ Problem(s"No such '$versionId'")
      case _ :: tail ⇒ Valid(tail)
    }

  def newVersionId(): VersionId = {
    val v = VersionId("#" + Timestamp.now.toIsoString)
    if (!versions.contains(v))
      v
    else
      Iterator.from(1).map(i ⇒ VersionId(s"${v.string}-$i")).collectFirst { case w if !versions.contains(w) ⇒ w } .get
  }
}

object Repo {
  val empty = new Repo(Nil, Map.empty)

  private implicit class RichVersionToFileBasedOption(private val versionToFileBasedOption: Map[VersionId, Option[FileBased]])
  extends AnyVal {
    /** None: no such version; Some(None): deleted */
    def fileBasedOption(history: List[VersionId]): Option[Option[FileBased]] =
      history.collectFirst { case v if versionToFileBasedOption contains v ⇒ versionToFileBasedOption(v) }
  }

  final case class DeletedProblem private[Repo](id: FileBasedId_)
    extends Problem.Lazy(s"Has been deleted: $id")

  final case class DuplicateVersionProblem private[Repo](versionId: VersionId)
    extends Problem.Lazy(s"Duplicate VersionId '${versionId.string}'")
}
