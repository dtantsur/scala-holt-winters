package net.divius.predictions

object HoltWinters {
	def apply(initialValues: List[Double], L: Int, alpha: Double = 0.5, beta: Double = 0.5, gamma: Double = 0.5): DataSink = {
	  class HoltWinters(val R: Double, val G: Double, val S: List[Double]) extends DataSink {
		  def update(newValue: Double) = {
		    val newR = alpha * newValue / S(L - 1) + (1.0 - alpha) * (R + G)
		    val newG = beta * (newR - R) + (1 - beta) * G
		    val newS = (gamma * newValue / newR + (1 - gamma) * S(L - 1)) :: S
		    new HoltWinters(newR, newG, newS)
		  }

		  def prediction: Stream[Double] = {
		    def loop(T: Int): Stream[Double] = if (T <= L) ((R + G) * S(L - T)) #:: loop(T + 1) else Stream.empty
		    loop(1)
		  }

		  override def toString = s"HoltWinters<R = $R, G = $G, S = ${S take 2}...>"
	  }

	  if (initialValues.length < 2 * L)
	    throw new RuntimeException(s"Expected at least 2 * $L = ${2 * L} data points")

	  val firstL = initialValues take L
	  val R0 = firstL.sum / L
	  val G0 = (R0 -  (initialValues drop L take L).sum / L) / L
	  val S0 = for ((y, k) <- firstL zipWithIndex) yield (y - k * G0 / 2) / R0
	  
	  new HoltWinters(R0, G0, S0)
	}
}