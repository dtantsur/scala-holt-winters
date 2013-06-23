package net.divius.predictions

trait DataSink {
	def update(value: Double): DataSink
	def prediction: Stream[Double]
}