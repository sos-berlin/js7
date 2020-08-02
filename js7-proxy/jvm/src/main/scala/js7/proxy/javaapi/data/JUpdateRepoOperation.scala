package js7.proxy.javaapi.data

import js7.base.crypt.SignedString
import js7.data.filebased.{TypedPath, UpdateRepoOperation}

final case class JUpdateRepoOperation(underlying: UpdateRepoOperation.ItemOperation)
extends JavaWrapper
{
  type Underlying = UpdateRepoOperation.ItemOperation
}

object JUpdateRepoOperation
{
  def addOrReplace(signedString: SignedString) =
    new JUpdateRepoOperation(UpdateRepoOperation.AddOrReplace(signedString))

  def delete(path: TypedPath) =
    new JUpdateRepoOperation(UpdateRepoOperation.Delete(path))
}
