name := "testi"

version := "1.0"

scalaVersion := "2.12.1"

val sversion = "2.12.1"

libraryDependencies ++= Seq(
  "org.jsoup" % "jsoup" % "1.8.3"

)


credentials += Credentials(Path.userHome / ".credentials")
