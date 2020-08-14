/*
 * Copyright (c) 2017-2019 The Typelevel Cats-effect Project Developers
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import microsites.{ConfigYml, ExtraMdFileConfig}

import scala.util.Try
import scala.sys.process._

ThisBuild / baseVersion := "2.0.0"

ThisBuild / crossScalaVersions := Seq("2.12.11", "2.13.3")
ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8", "adopt@11")

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"
ThisBuild / startYear := Some(2017)

ThisBuild / developers := List(
  Developer("djspiewak", "Daniel Spiewak", "", url("https://github.com/djspiewak")),
  Developer("mpilquist", "Michael Pilquist", "", url("https://github.com/mpilquist")),
  Developer("alexelcu", "Alexandru Nedelcu", "", url("https://alexn.org")),
  Developer("SystemFw", "Fabio Labella", "", url("https://github.com/systemfw"))
)

ThisBuild / homepage := Some(url("https://typelevel.org/cats-effect/"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/typelevel/cats-effect"), "git@github.com:typelevel/cats-effect.git")
)

val CatsVersion = "2.2.0-RC2"
val DisciplineScalatestVersion = "2.0.0"
val SilencerVersion = "1.7.1"

replaceCommandAlias(
  "ci",
  "; project /; headerCheck ;scalafmtSbtCheck ;scalafmtCheckAll; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant; doc"
)

replaceCommandAlias(
  "release",
  "; reload; project /; +mimaReportBinaryIssuesIfRelevant; +publishIfRelevant; sonatypeBundleRelease; microsite/publishMicrosite"
)

val commonSettings = Seq(
  scalacOptions in (Compile, doc) ++= {
    val isSnapshot = git.gitCurrentTags.value.map(git.gitTagToVersionNumber.value).flatten.isEmpty

    val path =
      if (isSnapshot)
        scmInfo.value.get.browseUrl + "/blob/" + git.gitHeadCommit.value.get + "€{FILE_PATH}.scala"
      else
        scmInfo.value.get.browseUrl + "/blob/v" + version.value + "€{FILE_PATH}.scala"

    Seq("-doc-source-url", path, "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath)
  },
  sources in (Compile, doc) := (sources in (Compile, doc)).value,
  scalacOptions in (Compile, doc) ++=
    Seq("-doc-root-content", (baseDirectory.value.getParentFile / "shared" / "rootdoc.txt").getAbsolutePath),
  scalacOptions in (Compile, doc) ++=
    Opts.doc.title("cats-effect"),
  scalacOptions in Test += "-Yrangepos",
  scalacOptions in Test ~= (_.filterNot(Set("-Wvalue-discard", "-Ywarn-value-discard"))),
  // Disable parallel execution in tests; otherwise we cannot test System.err
  parallelExecution in Test := false,
  parallelExecution in IntegrationTest := false,
  testForkedParallel in Test := false,
  testForkedParallel in IntegrationTest := false,
  concurrentRestrictions in Global += Tags.limit(Tags.Test, 1),
  headerLicense := Some(HeaderLicense.Custom("""|Copyright (c) 2017-2019 The Typelevel Cats-effect Project Developers
                                                |
                                                |Licensed under the Apache License, Version 2.0 (the "License");
                                                |you may not use this file except in compliance with the License.
                                                |You may obtain a copy of the License at
                                                |
                                                |    http://www.apache.org/licenses/LICENSE-2.0
                                                |
                                                |Unless required by applicable law or agreed to in writing, software
                                                |distributed under the License is distributed on an "AS IS" BASIS,
                                                |WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
                                                |See the License for the specific language governing permissions and
                                                |limitations under the License.""".stripMargin))
)

val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Set(organization.value %% name.value % "2.0.0")
      case Some((2, 12)) => Set(organization.value %% name.value % "1.0.0")
      case _             => Set.empty
    }
  },
  mimaBinaryIssueFilters ++= {
    import com.typesafe.tools.mima.core._
    import com.typesafe.tools.mima.core.ProblemFilters._
    Seq(
      // Ignore any binary compatibility issues/problems that match the internals package
      exclude[Problem]("cats.effect.internals.*"),
      // All internals - https://github.com/typelevel/cats-effect/pull/403
      exclude[DirectMissingMethodProblem]("cats.effect.concurrent.Semaphore#AbstractSemaphore.awaitGate"),
      exclude[DirectMissingMethodProblem]("cats.effect.concurrent.Semaphore#AsyncSemaphore.awaitGate"),
      exclude[DirectMissingMethodProblem]("cats.effect.concurrent.Semaphore#ConcurrentSemaphore.awaitGate"),
      // All internals — https://github.com/typelevel/cats-effect/pull/424
      exclude[MissingClassProblem]("cats.effect.concurrent.Deferred$UncancelabbleDeferred"),
      // Laws - https://github.com/typelevel/cats-effect/pull/473
      exclude[ReversedMissingMethodProblem]("cats.effect.laws.AsyncLaws.repeatedAsyncFEvaluationNotMemoized"),
      exclude[ReversedMissingMethodProblem]("cats.effect.laws.BracketLaws.bracketPropagatesTransformerEffects"),
      exclude[ReversedMissingMethodProblem]("cats.effect.laws.discipline.BracketTests.bracketTrans"),
      // Static forwarder not generated. We tried. - https://github.com/typelevel/cats-effect/pull/584
      exclude[DirectMissingMethodProblem]("cats.effect.IO.fromFuture"),
      // Incompatible signatures should not cause linking problems.
      exclude[IncompatibleSignatureProblem]("cats.effect.IO.ioParallel"),
      exclude[IncompatibleSignatureProblem]("cats.effect.IOInstances.ioParallel"),
      // Signature changes to make Resource covariant, should not cause linking problems. - https://github.com/typelevel/cats-effect/pull/731
      exclude[IncompatibleSignatureProblem]("cats.effect.Resource.use"),
      exclude[IncompatibleSignatureProblem]("cats.effect.Resource.flatMap"),
      exclude[IncompatibleSignatureProblem]("cats.effect.Resource.map"),
      exclude[IncompatibleSignatureProblem]("cats.effect.Resource.mapK"),
      exclude[IncompatibleSignatureProblem]("cats.effect.Resource.allocated"),
      exclude[IncompatibleSignatureProblem]("cats.effect.Resource.evalMap"),
      exclude[IncompatibleSignatureProblem]("cats.effect.Resource.evalTap"),
      // change in encoding of value classes in generic methods https://github.com/lightbend/mima/issues/423
      exclude[IncompatibleSignatureProblem]("cats.effect.Blocker.apply"),
      exclude[IncompatibleSignatureProblem]("cats.effect.Blocker.fromExecutorService"),
      // Tracing - https://github.com/typelevel/cats-effect/pull/854
      exclude[DirectMissingMethodProblem]("cats.effect.IO#Async.apply"),
      exclude[DirectMissingMethodProblem]("cats.effect.IO#Bind.apply"),
      exclude[IncompatibleResultTypeProblem]("cats.effect.IO#Async.k"),
      exclude[DirectMissingMethodProblem]("cats.effect.IO#Async.copy"),
      exclude[IncompatibleResultTypeProblem]("cats.effect.IO#Async.copy$default$1"),
      exclude[DirectMissingMethodProblem]("cats.effect.IO#Async.this"),
      exclude[DirectMissingMethodProblem]("cats.effect.IO#Bind.copy"),
      exclude[DirectMissingMethodProblem]("cats.effect.IO#Bind.this"),
      exclude[DirectMissingMethodProblem]("cats.effect.IO#Map.index"),
      exclude[IncompatibleMethTypeProblem]("cats.effect.IO#Map.copy"),
      exclude[IncompatibleResultTypeProblem]("cats.effect.IO#Map.copy$default$3"),
      exclude[IncompatibleMethTypeProblem]("cats.effect.IO#Map.this"),
      exclude[IncompatibleMethTypeProblem]("cats.effect.IO#Map.apply"),
      // revise Deferred, MVarConcurrent, LinkedLongMap - https://github.com/typelevel/cats-effect/pull/918
      exclude[IncompatibleResultTypeProblem]("cats.effect.concurrent.Deferred#State#Unset.waiting"),
      exclude[DirectMissingMethodProblem]("cats.effect.concurrent.Deferred#State#Unset.copy"),
      exclude[IncompatibleResultTypeProblem]("cats.effect.concurrent.Deferred#State#Unset.copy$default$1"),
      exclude[DirectMissingMethodProblem]("cats.effect.concurrent.Deferred#State#Unset.this"),
      exclude[MissingClassProblem]("cats.effect.concurrent.Deferred$Id"),
      exclude[DirectMissingMethodProblem]("cats.effect.concurrent.Deferred#State#Unset.apply")
    )
  }
)

val lawsMimaSettings = mimaSettings ++ Seq(
  // We broke binary compatibility for laws in 2.0
  mimaPreviousArtifacts := Set(organization.value %% name.value % "2.0.0")
)

lazy val scalaJSSettings = Seq(
  // Use globally accessible (rather than local) source paths in JS source maps
  scalacOptions ++= {
    val hasVersion = git.gitCurrentTags.value.map(git.gitTagToVersionNumber.value).flatten.nonEmpty

    val maybeVersionOrHash =
      if (hasVersion)
        Some(s"v${version.value}")
      else
        git.gitHeadCommit.value

    maybeVersionOrHash match {
      case Some(versionOrHash) =>
        val l = (baseDirectory in LocalRootProject).value.toURI.toString
        val g = s"https://raw.githubusercontent.com/typelevel/cats-effect/$versionOrHash/"
        Seq(s"-P:scalajs:mapSourceURI:$l->$g")

      case None =>
        Seq()
    }
  },
  // Work around "dropping dependency on node with no phase object: mixin"
  scalacOptions in (Compile, doc) -= "-Xfatal-warnings"
)

lazy val root = project
  .in(file("."))
  .disablePlugins(MimaPlugin)
  .aggregate(coreJVM, coreJS, lawsJVM, lawsJS, runtimeTests)
  .settings(noPublishSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "cats-effect",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %%% "discipline-scalatest" % DisciplineScalatestVersion % Test
    ),
    libraryDependencies ++= Seq(
      compilerPlugin(("com.github.ghik" % "silencer-plugin" % SilencerVersion).cross(CrossVersion.full)),
      ("com.github.ghik" % "silencer-lib" % SilencerVersion % "provided").cross(CrossVersion.full),
      ("com.github.ghik" % "silencer-lib" % SilencerVersion % Test).cross(CrossVersion.full)
    )
  )
  .jvmSettings(mimaSettings)
  .jsSettings(scalaJSSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val laws = crossProject(JSPlatform, JVMPlatform)
  .in(file("laws"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings: _*)
  .settings(
    name := "cats-effect-laws",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % CatsVersion,
      "org.typelevel" %%% "discipline-scalatest" % DisciplineScalatestVersion % Test
    )
  )
  .jvmSettings(lawsMimaSettings)
  .jsSettings(scalaJSSettings)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val FullTracingTest = config("fulltracing").extend(Test)

lazy val runtimeTests = project
  .in(file("runtime-tests"))
  .dependsOn(coreJVM)
  .settings(commonSettings ++ noPublishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % CatsVersion,
      "org.typelevel" %%% "discipline-scalatest" % DisciplineScalatestVersion % Test
    )
  )
  .configs(FullTracingTest)
  .settings(inConfig(FullTracingTest)(Defaults.testSettings): _*)
  .settings(
    unmanagedSourceDirectories in FullTracingTest += {
      baseDirectory.value.getParentFile / "src" / "fulltracing" / "scala"
    },
    test in Test := (test in Test).dependsOn(test in FullTracingTest).value,
    fork in Test := true,
    fork in FullTracingTest := true,
    javaOptions in Test ++= Seq(
      "-Dcats.effect.tracing=true",
      "-Dcats.effect.stackTracingMode=cached"
    ),
    javaOptions in FullTracingTest ++= Seq(
      "-Dcats.effect.tracing=true",
      "-Dcats.effect.stackTracingMode=full"
    )
  )

lazy val benchmarksPrev = project
  .in(file("benchmarks/vPrev"))
  .settings(commonSettings ++ noPublishSettings)
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0")
  .settings(scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains)))
  .enablePlugins(JmhPlugin)

lazy val benchmarksNext = project
  .in(file("benchmarks/vNext"))
  .dependsOn(coreJVM)
  .settings(commonSettings ++ noPublishSettings)
  .settings(scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains)))
  .enablePlugins(JmhPlugin)

lazy val docsMappingsAPIDir =
  settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val siteSettings = Seq(
  micrositeName := "Cats Effect",
  micrositeDescription := "The IO Monad for Scala",
  micrositeAuthor := "Cats Effect contributors",
  micrositeGithubOwner := "typelevel",
  micrositeGithubRepo := "cats-effect",
  micrositeBaseUrl := "/cats-effect",
  micrositeTwitterCreator := "@typelevel",
  micrositeDocumentationUrl := "https://typelevel.org/cats-effect/api/",
  micrositeFooterText := None,
  micrositeHighlightTheme := "atom-one-light",
  micrositePalette := Map(
    "brand-primary" -> "#3e5b95",
    "brand-secondary" -> "#294066",
    "brand-tertiary" -> "#2d5799",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"
  ),
  micrositeExtraMdFiles := Map(
    file("README.md") -> ExtraMdFileConfig(
      "index.md",
      "home",
      Map("permalink" -> "/", "title" -> "Home", "section" -> "home", "position" -> "0")
    )
  ),
  micrositeConfigYaml := ConfigYml(
    yamlPath = Some((resourceDirectory in Compile).value / "microsite" / "_config.yml")
  ),
  micrositeCompilingDocsTool := WithMdoc,
  mdocIn := (sourceDirectory in Compile).value / "mdoc",
  fork in mdoc := true,
  Compile / scalacOptions ~= (_.filterNot(
    Set(
      "-Xfatal-warnings",
      "-Werror",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:imports",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:patvars",
      "-Ywarn-unused:privates",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Xlint:-missing-interpolator,_"
    ).contains
  )),
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(mappings in packageDoc in Compile in coreJVM, docsMappingsAPIDir)
)

lazy val microsite = project
  .in(file("site"))
  .enablePlugins(MicrositesPlugin, SiteScaladocPlugin, MdocPlugin)
  .settings(commonSettings ++ noPublishSettings)
  .settings(siteSettings)
  .dependsOn(coreJVM, lawsJVM)

git.gitCurrentTags := Try("git tag --contains HEAD".!!.trim.split("\\s+").toList).toOption.toList.flatten
