addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
//addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.2-RC2")

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.8.2"
//libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.21"
