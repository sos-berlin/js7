addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.20")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.29")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")
//addSbtPlugin("com.thoughtworks.sbt-scala-js-map" % "sbt-scala-js-map" % "2.0.0")
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.12.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.12.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.12.1"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.28"

