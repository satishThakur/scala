package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal( b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      if(delta() < 0) Set()
      else{
        val sqDelta = Math.sqrt(delta())
        val s1 = (-b() + sqDelta)/(2 * a())
        val s2 = (-b() - sqDelta)/(2 * a())
        Set(s1, s2)
      }
    }
  }
}
