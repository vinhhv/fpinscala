import sbt._
import sbt.Keys._


lazy val `root` = Project("red-book", file("."))
  .settings(ScalaConfig.commonSettings)
  .settings(libraryDependencies ++= Dependencies.common.value)
  .settings(moduleName := "red-book")
  .settings(name := "red-book")
  .settings(mainClass in (Compile, run) := Some("io.vinhhv.redbook.Main"))
  .settings(libraryDependencies ++= Dependencies.server.value)
