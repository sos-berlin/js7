package js7.base.test

object TestExtensions:

  given autoSome[A]: Conversion[A, Some[A]] =
    Some(_)
