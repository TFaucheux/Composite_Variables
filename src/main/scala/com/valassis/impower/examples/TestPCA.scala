package com.valassis.impower.examples

import breeze.math._
import breeze.stats.distributions._
import breeze.stats.{corrcoeff}
import breeze.linalg.{NumericOps, DenseMatrix, DenseVector, SparseVector, lowerTriangular, diag, svd, cov}
import breeze.math.Complex

import scala.io.Source
import scala.collection.immutable.Map
// import breeze.util.ArrayUtil

object TestPca extends App {

//Initiate and seed a pseudo random number generator:
val r = new scala.util.Random(1537)

//The number of observations (cases/ instances):
val N = 10

//The number of features (inputs/ variables):
val p = 4

  def line2Data(line: String): Array[Double] = {
    line
      .split("\\,+").drop(1) // split on commas, dropping 1st column
      // .split("\\s+").drop(1) // split on spaces, droping 1st column
      .filter(_.length > 0)
      .map(_.toDouble)
  }

  //import data
  // val data = Source.fromFile("datasets/index_values2.data")
  val data = Source.fromFile("datasets/mtcars2.csv")
    .getLines().drop(1) // getLines and drop Header
    .map(x => line2Data(x))
    .toArray

  //convert to breeze matrix
  val dm = DenseMatrix(data: _*)

  //the inputs are all but the last column.  Outputs are last column
  // val X = dm(::, 1 to 12)
  val A = dm(::, 0 to 10)
println("A: (a DenseMatrix of mtcars Data)\n" + A)

//Create a variance-covariance matrix describing the multivariate covariance structure:
// val A: DenseMatrix[Double] = lowerTriangular(DenseMatrix((1.1,2.2,0.4), (3.9,4.333,0.3242),(-0.43443,9.4242,-21324.3)))
// val A: DenseMatrix[Double] = lowerTriangular(DenseMatrix.rand(p,p))
// println("A: (a variance-covariance matrix describing the multivariate covariance structure)\n" + A)

//Create a symmetric matrix by mirroring the lower triangular matrix.
// val B = A + A.t
// println("B: (a symmetric matrix by mirroring the lower triangular matrix)\n" + B)

// val C = DenseMatrix.ones[Double](p,p)
// println("C: (ones)\n" + C)

// println("The off-diagonal elements are all between 0 and 1, so we have to substract a matrix with 0.5 and double the values afterwards.")
// val D = 2.0 * (B - 0.5 * C)
// println("set diagonal to 1 (correlation matrix with equal variance of inputs)")
// diag(D) := 1.0
// println("D:\n" + D)

// val D = cov(A);
val D = corrcoeff(A)
// println("! The covariance matrix\n" + D)
println("The correlation matrix\n" + D.toString(5,Int.MaxValue))

println("Based on the correlation matrix we are already able to run a principal component analysis based on Singular Value Decomposition:")
//https://github.com/scalanlp/breeze/blob/master/math/src/main/scala/breeze/linalg/functions/svd.scala
val svd.SVD(u,s,v) = svd(D)
println("The singular value decomposition of matrix \"random\":")
println("The left singular vectors:")
println(u)
println("These are the eigenvectors in Rn!")


println("Let's have a look at the dimensionality of this matrix:")
println("Get the first column of matrix u (the first eigenvector):")
val u1 = u(::, 1)
println("Number of columns: "+u1.length)
println("get the first row of matrix u:")
val u_1 = u.t(::,1)
println("Number of rows: "+u_1.length)
println("Conclusion: The eigenvectors in Rn should have dimensionality N!")
println("This is not the case because we base our analysis directly on the covariance matrix instead of working with the original data matrix.")

println("The right singular vectors:")
println(v)
println("These are the eigenvectors in Rp!")
println("get the dominant eigenvector")
val v1 = v(::, 1)
println("Number of columns: "+v1.length)
val v_1 = v.t(::,1)
println("Number of rows: "+v_1.length)
println("")

println("The singular values:")
println(s)
println("The singular values are the square root of the eigenvalues.")
println("The eigenvalues:")
s.map(v=>v*v).map(v=>println(v))
println("")
}
