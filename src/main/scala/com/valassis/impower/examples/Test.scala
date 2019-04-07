package com.valassis.impower.examples

import com.valassis.impower.unsupervised.PrincipleComponentsAnalysis
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.linalg._, eigSym.EigSym
import breeze.stats._

object Test extends App {

/*
    def Pca(mat: DenseMatrix[Double]) {
    val xBar = mean(mat(::,*)).t
    val x = mat(*,::) - xBar
    val SVD = svd.reduced(x)
    val loadings = SVD.Vt.t
    val sdev = SVD.S / math.sqrt(x.rows - 1)
    lazy val scores = x * loadings
    }

    val X = DenseMatrix((1.0,1.5),(1.5,2.0),(2.0,1.5))
    val pca = Pca(X)
*/
    // printLn(pca.sdev)
    // printLn(pca.loadings)
    // printLn(pca.scores)

}
