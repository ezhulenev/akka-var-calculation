import sbt._, Keys._
import scala.language.postfixOps

object BuildSettings {

  val buildOrganization   =  "kkalc"
  val buildVersion        =  "0.0.1"
  val buildScalaVersion   =  "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization          := buildOrganization,
    scalaVersion          := buildScalaVersion,
    shellPrompt           := ShellPrompt.buildShellPrompt,
    maxErrors             := 5,
    exportJars            := true,
    scalacOptions         += "-unchecked",
    scalacOptions         += "-deprecation",
    scalacOptions         += "-feature",
    libraryDependencies   += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

  val buildResolvers = resolvers ++= Seq(
    "Typesafe Repo"       at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype Snapshots"  at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases"   at "http://oss.sonatype.org/content/repositories/releases"
  )
}

object ProjectBuild extends Build {

  import BuildSettings._

  val release           = settingKey[Boolean]("Perform release")
  val gitHeadCommitSha  = settingKey[String]("current git commit SHA")

  private def getVersion(release: Boolean, sha: String) = {
    import java.text.SimpleDateFormat
    import java.util.{Calendar, TimeZone}
    val utcTz = TimeZone.getTimeZone("UTC")
    val cal = Calendar.getInstance(utcTz)
    val sdf = new SimpleDateFormat("yyyyMMdd'T'HHmmss'Z'")
    sdf.setCalendar(cal)
    if (!release) s"$buildVersion-${sdf.format(cal.getTime)}-$sha" else buildVersion
  }

  def KkalcProject(name: String) = {
    Project(id = name, base = file(name)).
      // Apply global settings
      settings(buildSettings:_*).
      settings(buildResolvers:_*).

      // Dependency graph
      settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*).

      // Versioning settings
      settings(release                       := sys.props("kkalc-release") == "true").
      settings(gitHeadCommitSha in ThisBuild := Process("git rev-parse --short HEAD").lines.head).
      settings(version in ThisBuild          := getVersion(release.value, gitHeadCommitSha.value)).

      // Publishing settings (disable publishing)
      settings(publish := ())

  }

  lazy val root = Project("kkalc", file(".")).
    settings(publish :=()).
    settings(publishLocal :=()).
    aggregate(example, core)

  lazy val example = KkalcProject("example").
    settings(libraryDependencies ++= Dependencies.example).
    dependsOn(core)

  lazy val core = KkalcProject("core").
    settings(libraryDependencies ++= Dependencies.core)

}

object Dependencies {

  object Versions {
    // Logging
    val Slf4j              = "1.7.5"
    val Logback            = "1.0.13"

    // Scalaz
    val Scalaz             = "7.0.6"

    // Joda
    val JodaTime           = "2.3"
    val JodaConvert        = "1.5"

    // Akka
    val Akka               = "2.3.0"

    // Test libraries
    val ScalaMock          = "3.1.RC1"
    val ScalaTest          = "2.0"
  }

  object Compile {
    import Dependencies.{Versions => V}

    // Logging
    val slf4jApi            =   "org.slf4j"                  % "slf4j-api"                   % V.Slf4j
    val logback             =   "ch.qos.logback"             % "logback-classic"             % V.Logback

    // Scalaz
    val scalazCore          =  "org.scalaz"                 %% "scalaz-core"                 % V.Scalaz

    // Joda
    val jodaTime            =   "joda-time"                  % "joda-time"                   % V.JodaTime
    val jodaConvert         =   "org.joda"                   % "joda-convert"                % V.JodaConvert

    // Akka libraries
    val akkaActor           =   "com.typesafe.akka"         %% "akka-actor"                  % V.Akka
    val akkaCluster         =   "com.typesafe.akka"         %% "akka-cluster"                % V.Akka

  }

  object Test {
    import Dependencies.{Versions => V}

    val akkaTestkit         =  "com.typesafe.akka"          %% "akka-testkit"                % V.Akka

    val scalaMock           =  "org.scalamock"              %% "scalamock-scalatest-support" % V.ScalaMock     % "test"
    val scalaTest           =  "org.scalatest"              %% "scalatest"                   % V.ScalaTest     % "test"
  }

  // Projects dependencies

  val example = Seq()

  val core =
    Seq(Compile.slf4jApi, Compile.logback, Compile.jodaTime, Compile.jodaConvert, Compile.akkaActor, Compile.akkaCluster, Compile.scalazCore) ++
    Seq(Test.scalaTest, Test.scalaMock, Test.akkaTestkit)

}

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) {}
    def buffer[T] (f: => T): T = f
  }

  val current = """\*\s+([\w-/]+)""".r

  def gitBranches = ("git branch --no-color" lines_! devnull).mkString

  val buildShellPrompt = {
    (state: State) => {
      val currBranch =
        current findFirstMatchIn gitBranches map (_ group 1) getOrElse "-"
      val currProject = Project.extract (state).currentProject.id
      "%s:%s> ".format (
        currProject, currBranch
      )
    }
  }
}