seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

name := "happy-tree-align"

organization := "edu.isi.nlg"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
	"com.github.scopt" %% "scopt" % "2.1.0",
	"com.thoughtworks.xstream" % "xstream" % "1.4.4",
	"com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
	"org.slf4j" % "slf4j-simple" % "1.7.2",
	"org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

mainClass := Some("edu.isi.nlg.happytreealign.Main")
