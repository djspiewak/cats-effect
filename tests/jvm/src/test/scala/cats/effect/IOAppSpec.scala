/*
 * Copyright 2020-2021 Typelevel
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

package cats.effect

import cats.syntax.all._

import org.specs2.mutable.Specification

import scala.io.Source
import scala.sys.process.{BasicIO, Process}

import java.io.File

class IOAppSpec extends Specification {

  val JavaHome = System.getProperty("java.home")
  val ClassPath = System.getProperty("sbt.classpath")

  "IOApp (jvm)" should {
    import examples._

    "evaluate and print hello world" in {
      val h = java(HelloWorld, Nil)
      h.awaitStatus() mustEqual 0
      h.stdout() mustEqual s"Hello, World!${System.lineSeparator()}"
    }

    "pass all arguments to child" in {
      val expected = List("the", "quick", "brown", "fox jumped", "over")
      val h = java(Arguments, expected)
      h.awaitStatus() mustEqual 0
      h.stdout() mustEqual expected.mkString("", System.lineSeparator(), System.lineSeparator())
    }

    if (System.getProperty("os.name").toLowerCase.contains("windows")) {
      // The jvm cannot gracefully terminate processes on Windows, so this
      // test cannot be carried out properly. Same for testing IOApp in sbt.
      "run finalizers on TERM" in skipped(
        "cannot observe graceful process termination on Windows")
      "exit on fatal error" in skipped("cannot observe graceful process termination on Windows")
      "exit on fatal error with other unsafe runs" in skipped(
        "cannot observe graceful process termination on Windows")
    } else {
      "run finalizers on TERM" in {
        if (System.getProperty("os.name").toLowerCase.contains("windows")) {
          // The jvm cannot gracefully terminate processes on Windows, so this
          // test cannot be carried out properly. Same for testing IOApp in sbt.
          ok
        } else {
          import _root_.java.io.{BufferedReader, FileReader}

          // we have to resort to this convoluted approach because Process#destroy kills listeners before killing the process
          val test = File.createTempFile("cats-effect", "finalizer-test")
          def readTest(): String = {
            val reader = new BufferedReader(new FileReader(test))
            try {
              reader.readLine()
            } finally {
              reader.close()
            }
          }

          val h = java(Finalizers, test.getAbsolutePath() :: Nil)

          var i = 0
          while (!h.stdout().contains("Started") && i < 100) {
            Thread.sleep(100)
            i += 1
          }

          Thread.sleep(
            100
          ) // give thread scheduling just a sec to catch up and get us into the latch.await()

          h.term()
          h.awaitStatus() mustEqual 143

          i = 0
          while (readTest() == null && i < 100) {
            i += 1
          }
          readTest() must contain("canceled")
        }
      }

      "exit on fatal error" in {
        val h = java(FatalError, List.empty)
        h.awaitStatus() mustEqual 1
        h.stderr() must contain("Boom!")
      }

      "exit on fatal error with other unsafe runs" in {
        val h = java(FatalErrorUnsafeRun, List.empty)
        h.awaitStatus() mustEqual 1
        h.stderr() must contain("Boom!")
      }

      "exit on canceled" in {
        val h = java(Canceled, List.empty)
        h.awaitStatus() mustEqual 1
        h.stderr() must contain("canceled")
      }
    }
  }

  def java(proto: IOApp, args: List[String]): Handle = {
    val stdoutBuffer = new StringBuffer()
    val stderrBuffer = new StringBuffer()
    val builder = Process(
      s"${JavaHome}/bin/java",
      List("-cp", ClassPath, proto.getClass.getName.replaceAll("\\$$", "")) ::: args)
    val p = builder.run(BasicIO(false, stdoutBuffer, None).withError { in =>
      val err = Source.fromInputStream(in).getLines().mkString(System.lineSeparator())
      stderrBuffer.append(err)
      ()
    })

    new Handle {
      def awaitStatus() = p.exitValue()
      def term() = p.destroy() // TODO probably doesn't work
      def stderr() = stderrBuffer.toString
      def stdout() = stdoutBuffer.toString
    }
  }

  trait Handle {
    def awaitStatus(): Int
    def term(): Unit
    def stderr(): String
    def stdout(): String
  }
}

package examples {

  object HelloWorld extends IOApp.Simple {
    def run: IO[Unit] =
      IO(println("Hello, World!"))
  }

  object Arguments extends IOApp {
    def run(args: List[String]): IO[ExitCode] =
      args.traverse_(s => IO(println(s))).as(ExitCode.Success)
  }

  object Finalizers extends IOApp {
    import java.io.FileWriter

    def writeToFile(string: String, file: File): IO[Unit] =
      IO(new FileWriter(file)).bracket { writer => IO(writer.write(string)) }(writer =>
        IO(writer.close()))

    def run(args: List[String]): IO[ExitCode] =
      (IO(println("Started")) >> IO.never)
        .onCancel(writeToFile("canceled", new File(args.head)))
        .as(ExitCode.Success)
  }

  object FatalError extends IOApp {
    def run(args: List[String]): IO[ExitCode] =
      IO(throw new OutOfMemoryError("Boom!")).as(ExitCode.Success)
  }

  object FatalErrorUnsafeRun extends IOApp {
    import cats.effect.unsafe.IORuntime.Implicits.global

    def run(args: List[String]): IO[ExitCode] =
      for {
        _ <- (0 until 100).toList.traverse(_ => IO.blocking(IO.never.unsafeRunSync()).start)
        _ <- IO.blocking(IO(throw new OutOfMemoryError("Boom!")).start.unsafeRunSync())
        _ <- IO.never[Unit]
      } yield ExitCode.Success
  }

  object Canceled extends IOApp {
    def run(args: List[String]): IO[ExitCode] =
      IO.canceled.as(ExitCode.Success)
  }
}
