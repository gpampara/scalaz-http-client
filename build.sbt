resolvers += "tpolecat"  at "http://dl.bintray.com/tpolecat/maven"

scalacOptions in Compile := Seq("-feature")

libraryDependencies := Seq(
  "org.scalaz"   %% "scalaz-core"       % "7.0.5",
  "org.scalaz"   %% "scalaz-concurrent" % "7.0.5",
  "org.tpolecat" %% "atto"              % "0.1"
)
