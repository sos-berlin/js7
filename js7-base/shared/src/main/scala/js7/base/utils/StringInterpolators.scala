package js7.base.utils

object StringInterpolators {

  def interpolate(sc: StringContext, args: Seq[Any], argToString: Any => String, reserve: Int = 100)
  : String = {
    val builder = new java.lang.StringBuilder(sc.parts.view.map(_.length).sum + reserve)
    interpolateTo(sc, args, argToString: Any => String)(builder)
    builder.toString
  }

  def interpolateTo(sc: StringContext, args: Seq[Any], argToString: Any => String)
    (appendable: Appendable)
  : Unit = {
    StringContext.checkLengths(args, sc.parts)
    val p = sc.parts.iterator
    appendable.append(p.next())
    for (arg <- args) {
      appendable.append(argToString(arg))
      appendable.append(p.next())
    }
  }
}
