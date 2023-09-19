package js7.base.utils

private object TestsPlatform {
  def allActiveClasses: Set[String] =
    Thread.currentThread().getStackTrace.map(_.getClassName).toSet
}
