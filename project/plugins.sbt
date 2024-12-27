addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.13.0")
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.1.0")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.10.4")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.4.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
//addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.17.0")
//addSbtPlugin("com.thoughtworks.sbt-scala-js-map" % "sbt-scala-js-map" % "2.0.0")

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.24.2"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.24.2"
libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.24.2"
libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.16"
