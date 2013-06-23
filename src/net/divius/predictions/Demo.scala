package net.divius.predictions

object Demo extends App {
    val L = 5
	val initialValues = (1 to 2 flatMap (_ => (1 to L).toList ::: (2 until L).reverse.toList) map (1.0 * _)).toList
	println(initialValues)
	
	val hw = HoltWinters(initialValues, 2 * L - 2)
	println(hw)
	val pred = hw.prediction
	println(pred.toList)
    
    val testValues = (1 to 20 flatMap (_ => initialValues)).toList
    println(testValues)
    
    val hw2 = (hw /: testValues)(_.update(_))
    println(hw2)
    println(hw2.prediction.toList)
}