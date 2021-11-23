package io.taig.testf

trait PureTestApp extends TestApp:
  def runner: Runner[Identity, Pure] = Runner.pure

  def spec: Spec[Pure]

  override def main(logger: String => Unit, callback: Option[Report] => Unit): Unit =
    val report = runner.run(spec)
    logger(Formatter(report, colors = true))
    callback(Some(report))
