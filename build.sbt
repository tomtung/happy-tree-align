name := "happy-tree-align"

organization := "edu.isi.nlg"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
	"com.github.scopt" %% "scopt" % "2.1.0",
	"org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"
