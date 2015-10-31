scalaVersion := "2.11.7"

name := "ParseHProf"

version := "1.0"

libraryDependencies += "org.scodec" %% "scodec-core" % "1.8.2"

libraryDependencies ++= {
  if (scalaBinaryVersion.value startsWith "2.10")
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full))
  else Nil
}

libraryDependencies += "org.scodec" %% "scodec-scalaz" % "1.1.0"

libraryDependencies += "org.scodec" %% "scodec-stream" % "0.10.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

libraryDependencies += "org.scodec" %% "scodec-bits" % "1.0.10"

EclipseKeys.withSource := true