name := "twitter4s-demo"

version := "0.1"

scalaVersion := "2.12.1"

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions in ThisBuild ++= Seq("-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials",
  "-feature",
  "-deprecation")

libraryDependencies ++= Seq(
  "com.danielasfregola" %% "twitter4s" % "5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.9",
  "oauth.signpost" % "signpost-core" % "1.2",
  "oauth.signpost" % "signpost-commonshttp4" % "1.2",
"commons-io" % "commons-io" % "2.4", 
	"commons-lang" % "commons-lang" % "2.6",
  "org.apache.httpcomponents" % "httpclient" % "4.2"
)
