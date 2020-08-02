package js7.controller.repo

import js7.data.crypt.FileBasedVerifier.Verified
import js7.data.filebased.{FileBased, TypedPath, VersionId}
import scala.collection.immutable.Seq

final case class VerifiedUpdateRepo(
  versionId: VersionId,
  verifiedFileBased: Seq[Verified[FileBased]],
  delete: Seq[TypedPath])
