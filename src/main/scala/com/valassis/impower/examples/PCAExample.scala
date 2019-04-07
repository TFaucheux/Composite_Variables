package com.valassis.impower.examples

import com.valassis.impower.unsupervised.PrincipleComponentsAnalysis
import breeze.stats.{corrcoeff}
import breeze.linalg.{DenseMatrix, DenseVector, cov}
import breeze.linalg._, eigSym.EigSym

import scala.util.{Try, Success, Failure}
import scala.io.Source

object PCAExample extends App{

  def line2Data(line: String): Array[Double] = {
    line
      .split("\\s+")
      .filter(_.length > 0)
      .map(_.toDouble)
  }

  //import data
  // val data = Source.fromFile("datasets/index_values2.data")
  // val data = Source.fromFile("datasets/pca_gsp.csv")
  val data = Source.fromFile("datasets/mtcars.csv")
    .getLines().drop(1)
    .map(x => line2Data(x))
    .toArray

  //convert to breeze matrix
  val dm = DenseMatrix(data: _*)

  //the inputs are all but the last column.  Outputs are last column
  // val X = dm(::, 1 to 13)
  val X = dm(::, 1 to 11)
  val covMat = cov(X)
  println("\ncovMat:" + covMat);
  val corMat = corrcoeff(X)
  println("\ncorMat:" + corMat);

  // val pca = new PrincipleComponentsAnalysis(X,covMat)
  val pca = new PCA(X,corMat)
  println(pca)
  println("observations:" + pca.nobs)
  println("center:" + pca.center)
  println("sdev:" + pca.sdev)
  println("loadings:" + pca.loadings)
  println("eigenvalues:" + pca.eigenvalues)
  println("propvar:" + pca.propvar)
  println("cumuvar:" + pca.cumuvar)
  println("scores:" + pca.scores)

  // println("\ncovMat:");
  // println(pca.covMat)

  println("\ncomponentVariance:");
  // println(pca.componentVariance)

  val A = corMat
  val EigSym(lambda, evs) = eigSym(A)

  val es = eigSym(A)
  val lambda2 = es.eigenvalues
  val evs2 = es.eigenvectors

  println("data\n"+ X)
  println("covMat\n"+ A)
  println("\nes:" + es)

  println("\n")
  println("eigenvalues:" + lambda2)
  println("eigenvectors:" + evs2)
  println("\n")

  println("\ncumlativeVariance:");
  // println(pca.cumlativeVariance)

  println("\ntransformedData:");
  // println(pca.transformedData)
}


