package com.valassis.impower.unsupervised

import breeze.linalg.{*, DenseMatrix, PCA, cov}
import breeze.stats.mean


class PrincipleComponentsAnalysis(data: DenseMatrix[Double], covMat: DenseMatrix[Double]) {
/*
  val covMat = {
    val empMean: DenseMatrix[Double] = mean(data(::, *))

    var covariance = DenseMatrix.zeros[Double](data.cols, data.cols)
    (0 to data.rows - 1).foreach { dpId =>
      val dp = data(dpId, ::).t
      val dpMinusMu = dp - empMean.toDenseVector

      covariance += dpMinusMu * dpMinusMu.t
    }

    covariance.map(_ / (data.rows - 1))
  }
*/

  val pca = new PCA(data, covMat)

  val cumlativeVariance = pca.cumuvar
  val componentVariance = pca.propvar
  val transformedData = pca.scores
}
