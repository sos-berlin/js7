//addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.13")
addSbtPlugin("com.github.sbt" % "sbt-git" % "1.0.3-M1")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.7")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.8.0")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.1.0")
//addSbtPlugin("com.thoughtworks.sbt-scala-js-map" % "sbt-scala-js-map" % "2.0.0")

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.18.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.18.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.18.0"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.36"
