package com.sos.scheduler.engine.data.folder

import com.google.common.base.Splitter
import com.sos.scheduler.engine.data.filebased.{AbsolutePath, HasPath, TypedPath}
import com.sos.scheduler.engine.data.folder.FolderTree._
import scala.collection.JavaConversions._
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
final case class FolderTree[A](
  path: FolderPath,
  leafs: immutable.Seq[Leaf[A]],
  subfolders: immutable.Seq[FolderTree[A]])

object FolderTree {
  private val PathSplitter = Splitter.on('/')

  def apply[A <: HasPath](root: FolderPath, hasPaths: Iterable[A]): FolderTree[A] =
    apply(root, hasPaths, _.path)

  def apply[A](root: FolderPath, objects: Iterable[A], toPath: A ⇒ TypedPath): FolderTree[A] =
    fromNameSeqs(root, objects map { o ⇒ (split(toPath(o)) drop root.nesting) → o })

  implicit def ordering[A]: Ordering[FolderTree[A]] = Ordering by { _.path.lastName }

  private[folder] def split(path: AbsolutePath): Vector[String] =
    PathSplitter.split(path.string stripPrefix "/").toVector

  private def fromNameSeqs[A](folderPath: FolderPath, paths: Iterable[(Iterable[String], A)]): FolderTree[A] = {
    val (leafPaths, folderPaths) = paths partition { case (nameSeq, obj) ⇒ nameSeq.tail.isEmpty }
    val leafs = for ((nameSeq, obj) ← leafPaths) yield Leaf(nameSeq.head, obj)
    val subfolders = for ((name, paths) ← folderPaths groupBy { case (nameSeq, obj) ⇒ nameSeq.head })
      yield fromNameSeqs(folderPath / name, for ((nameSeq, obj) ← paths) yield nameSeq.tail → obj)
    new FolderTree(folderPath, leafs.toVector, subfolders.toVector)
  }

  final case class Leaf[A](name: String, obj: A)
}
