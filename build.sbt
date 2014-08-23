resolvers += "tpolecat"  at "http://dl.bintray.com/tpolecat/maven"

scalacOptions in Compile := Seq("-feature", "-deprecation", "-language:higherKinds", "-language:implicitConversions")

libraryDependencies := Seq(
  "org.scalaz"   %% "scalaz-core"       % "7.1.0",
  "org.scalaz"   %% "scalaz-concurrent" % "7.1.0",
  "org.tpolecat" %% "atto-core"         % "0.4.0"
)

crossScalaVersions := Seq("2.10.4", "2.11.2")