import sbt._
import sbt.Keys._
import sbt.TestFramework
import sbt.util.Level
// import wartremover.WartRemover.autoImport._

object ScalaConfig {
  //  val warts = Warts.allBut(
  //      Wart.Any
  //    , Wart.JavaSerializable
  //    , Wart.Nothing
  //    , Wart.Product
  //    , Wart.PublicInference
  //    , Wart.Serializable
  //  )

  // see https://docs.scala-lang.org/overviews/compiler-options/index.html#Standard_Settings
  private val stdOptions = Seq(
      "-deprecation"
    , "-encoding"
    , "UTF-8"
    , "-explaintypes"
    , "-feature"
    , "-language:existentials"
    , "-language:higherKinds"
    , "-language:implicitConversions"
    , "-language:postfixOps"
    , "-opt-warnings"
    , "-opt:l:inline"
    , "-opt-inline-from:<source>"
    , "-unchecked"
    , "-Xcheckinit"
    , "-Xfatal-warnings"
    , "-Xlint"
    , "-Xlint:implicit-recursion"
    , "-Xlint:inaccessible"
    , "-Ymacro-annotations"
    , "-Ywarn-extra-implicit"
    , "-Ywarn-numeric-widen"
    , "-Ywarn-unused"
    , "-Ywarn-value-discard"
    , "-Wconf:src=src_managed/.*&cat=unused:s"
  )

  val commonSettings = {
    Seq(
        organization := BuildConfig.organization
      , scalaVersion := "2.13.3"
      , scalacOptions := stdOptions
      , logLevel := Level.Info
      , version := BuildConfig.appVersion
      //      , wartremoverErrors in (Compile, compile) ++= warts
      //      , wartremoverErrors in (Test, compile) ++= warts
      //      , wartremoverExcluded += sourceManaged.value
      , testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
      , cancelable in Global := true
      // , fork in Global := true, // https://github.com/sbt/sbt/issues/2274
      , resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    )
  }
}
