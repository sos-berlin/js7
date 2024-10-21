addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.12.0")
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.1")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.4.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.2.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.12.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
//addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.14.0")
//addSbtPlugin("com.thoughtworks.sbt-scala-js-map" % "sbt-scala-js-map" % "2.0.0")

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.24.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.24.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.24.1"
libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.13"
