package js7.base.number

object Numbers:
  // Like java.lang.Math.addExact
  def addSaturating(a: Long, b: Long): Long =
    val r = a + b
    if ((a ^ r) & (b ^ r)) < 0 then
      if a > 0 && b > 0 then
        Long.MaxValue
      else
        Long.MinValue
    else
      r

  // Like java.lang.Math.addExact
  def subtractSaturating(a: Long, b: Long): Long =
    val r = a - b
    if ((a ^ b) & (a ^ r)) < 0 then
      if a > 0 && b < 0 then
        Long.MaxValue
      else
        Long.MinValue
    else
      r
