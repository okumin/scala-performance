name := "performance-test"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-http-core-experimental_2.11" % "1.0",
  "com.typesafe.play" % "play-iteratees_2.11" % "2.4.3",
  "org.scalaz" %% "scalaz-concurrent" % "7.1.3",
  "com.storm-enroute" %% "scalameter" % "0.7" % "test"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
