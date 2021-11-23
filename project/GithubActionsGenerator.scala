import io.circe.Json
import io.circe.syntax._

object GithubActionsGenerator {
  object Action {
    val Checkout = "actions/checkout@v2.4.0"
    val CoursierCache = "coursier/cache-action@v6.3"
    val SetupJava = "actions/setup-java@v2.3.1"
  }

  object Step {
    val SetupJava = Json.obj(
      "name" := "Setup Java JDK",
      "uses" := Action.SetupJava,
      "with" := Json.obj(
        "distribution" := "temurin",
        "java-version" := "11"
      )
    )

    val Checkout = Json.obj(
      "name" := "Checkout",
      "uses" := Action.Checkout
    )

    val Cache = Json.obj(
      "name" := "Cache",
      "uses" := Action.CoursierCache
    )
  }

  object Job {
    val Lint: Json = Json.obj(
      "name" := "⚠️ Fatal warnings and code formatting",
      "runs-on" := "ubuntu-latest",
      "steps" := List(
        Step.SetupJava,
        Step.Checkout,
        Step.Cache,
        Json.obj(
          "name" := "Workflows",
          "run" := "sbt blowoutCheck"
        ),
        Json.obj(
          "name" := "Code formatting",
          "run" := "sbt scalafmtCheckAll"
        ),
        Json.obj(
          "name" := "Fatal warnings",
          "run" := "sbt -Dmode=strict +Test/compile"
        )
      )
    )

    val Test: Json = Json.obj(
      "name" := "✅ Unit & integration tests",
      "runs-on" := "ubuntu-latest",
      "steps" := List(
        Step.SetupJava,
        Step.Checkout,
        Step.Cache,
        Json.obj(
          "name" := "Tests",
          "run" := "sbt +test"
        )
      )
    )
  }

  val main: Json = Json.obj(
    "name" := "CI & CD",
    "on" := Json.obj(
      "push" := Json.obj(
        "branches" := List("main"),
        "tags" := List("*.*.*")
      )
    ),
    "jobs" := Json.obj(
      "lint" := Job.Lint,
      "test" := Job.Test,
      "deploy" := Json.obj(
        "name" := "🚀 Deploy",
        "runs-on" := "ubuntu-latest",
        "needs" := List("test", "lint"),
        "steps" := List(
          Step.SetupJava,
          Step.Checkout,
          Step.Cache,
          Json.obj(
            "name" := "Release",
            "run" := "sbt ci-release",
            "env" := Json.obj(
              "PGP_PASSPHRASE" := "${{secrets.PGP_PASSPHRASE}}",
              "PGP_SECRET" := "${{secrets.PGP_SECRET}}",
              "SONATYPE_PASSWORD" := "${{secrets.SONATYPE_PASSWORD}}",
              "SONATYPE_USERNAME" := "${{secrets.SONATYPE_USERNAME}}"
            )
          )
        )
      )
    )
  )

  def branches: Json = Json.obj(
    "name" := "CI",
    "on" := Json.obj(
      "pull_request" := Json.obj(
        "branches" := Json.arr("main".asJson)
      )
    ),
    "jobs" := Json.obj(
      "lint" := Job.Lint,
      "test" := Job.Test
    )
  )
}
