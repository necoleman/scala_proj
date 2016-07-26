/******************************************************************************
 *  (Start of) Implementation of a neural net in scala, in a functional style.
 *
 *  Things I'd like to look at/understand:
 *	- Implementing power series with streams
 *	- Whether there's a better way of computing the gradient
 *	- Implementing a stream of training data until convergence
 * 	- Using affine rep [A|b]x\oplus 1 instead of afifne-linear Ax + b
 *	- Whether there's a better way to structure the backpropagation
 *
 *****************************************************************************/

sealed trait NeuralNet


object NeuralNetApp {

type Point = Array[Double]
type Matrix = Array[Array[Double]]

/*
Stolen from Matt
*/
object LinAlgRoutines {

    def nRow(X: Matrix): Int = X.length
    def nCol(X: Matrix): Int = X(0).length

    def zeros(n: Int): Array[Double] = Array.fill[Double](n)(0) 

    def subtract(v1: Point, v2: Point): Point =
        v1.zip(v2).map { case (x: Double, y: Double) => x - y }

    def add(v1: Point, v2: Point): Point =
        v1.zip(v2).map { case (x: Double, y: Double) => x + y }

		def hadamultiply(v1: Point, v2: Point): Point = 
				v1.zip(v2).map { case (x: Double, y: Double) => x*y }

    def scalarMultiply(lambda: Double, v: Point): Point =
        v.map( (x: Double) => x * lambda )

    def dot(v1: Point, v2: Point): Double =
        v1.zip(v2).map { case (x: Double, y: Double) => x * y }.sum

    def transpose(X: Matrix): Matrix = Array.tabulate(nCol(X), nRow(X))((i, j) => X(j)(i))

    def matrixMultiply(X: Matrix, Y: Matrix): Matrix = {
        //var P = Array.tabulate(X.nRow, Y.nCol)( (x, y) => 0.0 )
        for (Xrow <- X) yield
            for (Ycol <- transpose(Y)) yield
                dot(Xrow, Ycol)
    }

    def matrixVectorMultiply(X: Matrix, y: Point): Point = {
        for(Xrow <- X) yield dot(Xrow, y)
    }

}

object MathRoutines {
	def exp_coeff(x: Double, n: Int): Double = (x,n) match{
		case (x,0) => 1
		case _ => (x/n)*exp_coeff(x,n-1)
	}
	
	def exp(x: Double): Double = {
		1 + x + x*x/2 + x*x*x/6
	}
	
	def sigmoid(x: Double): Double = {
		1/(1+exp(-x))
	}

}

/* 
	conceptualize a neural net as a vector of vectors of matrices
	each matrix is the 
*/
object NeuralRoutines{
	
	def activate_layer(l: Matrix, i: Point): Point = {
		LinAlgRoutines.matrixVectorMultiply(l,i).map(x => MathRoutines.sigmoid(x)):+1
	}
	
	def feed_forward(neural_net: List[Matrix], input: Point): Point 
			= (neural_net, input) match 
	{
		case (Nil, input) => input
		case (_, input) => feed_forward(neural_net.tail, activate_layer(neural_net.head, input))
	}

	/* return the gradient of the weights */
	def back_propagate(neural_net: List[Matrix], input: Point, true: Point): 
		List[Matrix] = (net, inp, error) match
	{
		
	}
	
}

object GenerateData{

	def generate_number(n: Int): Point = n match{
		case 0 => Array(1, 1, 1, 
										1, 0, 1, 
										1, 0, 1, 
										1, 0, 1, 
										1, 1, 1)

		case 1 => Array(0, 1, 0, 
										0, 1, 0, 
										0, 1, 0, 
										0, 1, 0,
										0, 1, 0)

		case 2 => Array(1, 1, 1,
										0, 0, 1,
										1, 1, 1,
										1, 0, 0,
										1, 1, 1)

		case 3 => Array(1, 1, 1,
										0, 0, 1,
										0, 1, 1,
										0, 0, 1,
										1, 1, 1)

		case 4 => Array(1, 0, 1,
										1, 0, 1,
										1, 1, 1,
										0, 0, 1,
										0, 0, 1)

		case 5 => Array(1, 1, 1,
										1, 0, 0,
										1, 1, 1,
										0, 0, 1,
										1, 1, 1)

		case 6 => Array(1, 0, 0,
										1, 0, 0,
										1, 1, 1,
										1, 0, 1,
										1, 1, 1)

		case 7 => Array(1, 1, 1,
										0, 0, 1,
										0, 0, 1,
										0, 0, 1,
										0, 0, 1)
		
		case 8 => Array(1, 1, 1,
										1, 0, 1,
										1, 1, 1,
										1, 0, 1,
										1, 1, 1)
		
		case 9 => Array(1, 1, 1,
										1, 0, 1,
										1, 1, 1,
										0, 0, 1,
										1, 1, 1)
	}

}

def main(args: Array[String]): Unit = {
    
	import scala.runtime.ScalaRunTime._
	def printArray[A](a: Array[A]): Unit = println(stringOf(a))

	def printHolder: Unit = {
		println("Someday I will have a brain here!")
	}

	printHolder

}

}
