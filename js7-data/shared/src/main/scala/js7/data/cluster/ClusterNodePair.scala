package js7.data.cluster

final case class ClusterNodePair[A](primary: A, backup: Option[A] = None)
{
  def nodes: List[A] =
    primary :: backup.toList
}
