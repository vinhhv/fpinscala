import sbt._

object Version {
  object server {
    val flyway = "6.5.7"
    val quill  = "3.5.3"

    val circe       = "0.13.0"
    val circeExtras = "0.12.2"

    val http4s  = "0.21.2"
    val grpc    = "1.31.1"
    val logging = "3.9.2"

    val mysql = "8.0.21"

    val scalatest = "3.1.1"

    val zio        = "1.0.0"
    val zioCats    = "2.1.4.0"
    val zioConfig  = "1.0.0-RC26"
    val zioPrelude = "0.0.0+271-9bbfa4fd-SNAPSHOT"
  }

  object client {}

  object common {
    val kindProjector = "0.10.3"
  }
}

object Dependencies {
  object compiler {
    addCompilerPlugin("org.typelevel" %% "kind-projector" % Version.common.kindProjector)
  }

  object common {
    val logging = "com.typesafe.scala-logging" %% "scala-logging" % Version.server.logging
    val value   = List(logging)
  }

  object server {
    // Database
    val flyway    = "org.flywaydb" % "flyway-core"          % Version.server.flyway
    val mysql     = "mysql"        % "mysql-connector-java" % Version.server.mysql
    val quill     = "io.getquill" %% "quill-core"           % Version.server.quill
    val quillJdbc = "io.getquill" %% "quill-jdbc"           % Version.server.quill

    // Cats
    val catsEffect = "org.typelevel" %% "cats-effect" % "2.1.4"
    val cats       = List(catsEffect)

    // Circe
    val circeCore    = "io.circe" %% "circe-core"           % Version.server.circe
    val circeExtras  = "io.circe" %% "circe-generic-extras" % Version.server.circeExtras
    val circeGeneric = "io.circe" %% "circe-generic"        % Version.server.circe
    val circeParser  = "io.circe" %% "circe-parser"         % Version.server.circe
    val circe        = List(circeGeneric, circeCore, circeParser, circeExtras)

    val scalactic    = "org.scalactic" %% "scalactic" % Version.server.scalatest
    val scalatest    = "org.scalatest" %% "scalatest" % Version.server.scalatest % "test"
    val scalatestAll = List(scalactic, scalatest)

    val zioCore    = "dev.zio"  %% "zio"              % Version.server.zio
    val zioCats    = ("dev.zio" %% "zio-interop-cats" % Version.server.zioCats).excludeAll(ExclusionRule("dev.zio"))
    val zioMacros  = "dev.zio"  %% "zio-macros"       % Version.server.zio
    val zioPrelude = "dev.zio"  %% "zio-prelude"      % Version.server.zioPrelude
    val zio        = List(zioCore, zioCats, zioMacros, zioPrelude)

    val zioConfigCore     = "dev.zio" %% "zio-config"          % Version.server.zioConfig
    val zioConfigMagnolia = "dev.zio" %% "zio-config-magnolia" % Version.server.zioConfig
    val zioConfigTypesafe = "dev.zio" %% "zio-config-typesafe" % Version.server.zioConfig
    val zioConfig         = List(zioConfigCore, zioConfigMagnolia, zioConfigTypesafe)

    val zioTest    = "dev.zio" %% "zio-test"     % Version.server.zio % "test"
    val zioTestSbt = "dev.zio" %% "zio-test-sbt" % Version.server.zio % "test"
    val zioTestAll = List(zioTest, zioTestSbt)

    val serviceDependencies = circe ++ zio ++ zioConfig
    val storageDependencies = List(flyway, mysql, quill, quillJdbc)
    val testDependencies    = scalatestAll ++ zioTestAll
    val value               = serviceDependencies ++ storageDependencies ++ testDependencies
  }

  object client {}

}
