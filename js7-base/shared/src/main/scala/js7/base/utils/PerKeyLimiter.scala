package js7.base.utils

/**
  * @author Joacim Zschimmer
  */
final class PerKeyLimiter[K, V](limit: Int, toKey: V => K) extends (V => Boolean) {

  require(limit >= 0, "Negative limit")

  private var count = -1
  private var key: K = _

  def apply(v: V): Boolean = {
    val k = toKey(v)
    if count == -1 || k != key then {
      key = k
      count = 0
    }
    count < limit && {
      count += 1
      true
    }
  }
}
