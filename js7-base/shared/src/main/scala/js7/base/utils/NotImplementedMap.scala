package js7.base.utils

final class NotImplementedMap[K, V] extends Map[K, V]
{
  def removed(key: K): Map[K, V] =
    throw new NotImplementedError

  def updated[V1 >: V](key: K, value: V1): Map[K, V1] =
    throw new NotImplementedError

  def get(key: K): Option[V] =
    throw new NotImplementedError

  def iterator: Iterator[(K, V)] =
    throw new NotImplementedError
}
