package io.taig.testf

trait PureTestApp extends TestApp:
  def runner: Runner[Identity, Pure] = Runner.pure

  def suite: Test[Pure]

  override def main(logger: String => Unit, callback: Option[Report] => Unit): Unit =
    val report = runner.run(suite)
    logger(Formatter(report))
    callback(Some(report))
