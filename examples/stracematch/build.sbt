packAutoSettings

lazy val commonSettings = Seq(
  organization := "jp.go.aist.cspe",
  version := "0.2.0",
  scalaVersion := "2.11.5"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "strace_match",
    resolvers ++= Seq(
      "LocalRepo" at "file://" + file(Path.userHome.absolutePath + "/.ivy2/local").getAbsolutePath
    ),
    libraryDependencies ++= Seq("com.github.cb372" %% "scalacache-guava" % "0.5.2", 
    "io.github.nicolasstucki" %% "multisets" % "0.2",
    "io.spray" %%  "spray-json" % "1.3.2",
    "jp.go.aist.cspe" %% "csp_e" % "0.2.0"
    )
  )