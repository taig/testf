object GithubActionsGenerator {
  object Action {
    val Checkout = "actions/checkout@v2.4.0"
    val CoursierCache = "coursier/cache-action@v6.3"
    val SetupJava = "actions/setup-java@v2.3.1"
  }

  object Job {
    val Lint: String =
      s"""  lint:
         |    name: ⚠️ Fatal warnings and code formatting
         |    runs-on: ubuntu-latest
         |    steps:
         |      - name: Setup Java JDK
         |        uses: ${Action.SetupJava}
         |      - name: Checkout
         |        uses: ${Action.Checkout}
         |      - name: Cache
         |        uses: ${Action.CoursierCache}
         |      - name: Workflows
         |        run: sbt githubWorkflowsCheck
         |      - name: Code formatting
         |        run: sbt scalafmtCheckAll
         |      - name: Fatal warnings
         |        run: sbt -Dmode=strict +Test/compile""".stripMargin

    val Test: String =
      s"""  test:
         |    name: ✅ Unit & integration tests
         |    runs-on: ubuntu-latest
         |    steps:
         |      - name: Setup Java JDK
         |        uses: ${Action.SetupJava}
         |      - name: Checkout
         |        uses: ${Action.Checkout}
         |      - name: Cache
         |        uses: ${Action.CoursierCache}
         |      - name: Tests
         |        run: sbt +test""".stripMargin
  }

  val main: String =
    s"""name: CI & CD
       |
       |on:
       |  push:
       |    branches:
       |      - 'main'
       |    tags:
       |      - '*.*.*'
       |
       |jobs:
       |${Job.Test}
       |${Job.Lint}
       |  deploy:
       |    name: 🚀 Deploy
       |    needs: [ test, lint ]
       |    runs-on: ubuntu-latest
       |    steps:
       |      - name: Setup Java JDK
       |        uses: ${Action.SetupJava}
       |      - name: Checkout
       |        uses: ${Action.Checkout}
       |      - name: Cache
       |        uses: ${Action.CoursierCache}
       |      - name: Release
       |        run: sbt ci-release
       |        env:
       |          PGP_PASSPHRASE: $${{secrets.PGP_PASSPHRASE}}
       |          PGP_SECRET: $${{secrets.PGP_SECRET}}
       |          SONATYPE_PASSWORD: $${{secrets.SONATYPE_PASSWORD}}
       |          SONATYPE_USERNAME: $${{secrets.SONATYPE_USERNAME}}""".stripMargin

  val branches: String =
    s"""name: CI
       |
       |on:
       |  pull_request:
       |    branches:
       |      - 'main'
       |
       |jobs:
       |${Job.Test}
       |${Job.Lint}""".stripMargin
}
