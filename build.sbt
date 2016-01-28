
lazy val commonSettings = Seq(
  organization := "jp.go.aist.cspe",
  version := "0.3.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "csp_e",
    resolvers ++= Seq(
      "LocalRepo" at "file://" + file(Path.userHome.absolutePath + "/.ivy2/local").getAbsolutePath
    ),
    libraryDependencies ++= Seq(
    "io.github.nicolasstucki" %% "multisets" % "0.2")
  )