package Kmeans

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Kmeans {

  def loadDataSet(fileName: String) = {
    var dataMat = ArrayBuffer[Array[Double]]()
    val fr = Source.fromFile(fileName)
    for (line <- fr.getLines()) {
      val fltLine = line.trim().split("\t").map(_.toDouble)
      dataMat.append(fltLine)
    }
    dataMat.toArray
  }

  def distEclud(vecA: Array[Double], vecB: Array[Double]) = {
    vecA.zip(vecB).map(x => math.pow(x._1 - x._2, 2)).sum
  }

  def randCent(dataSet: Array[Array[Double]], k: Int) = {
    val n = dataSet(0).length
    var centroids = Array.fill(k)(Array.fill(n)(0.0))
    for (j <- 0 to n - 1) {
      val minJ = dataSet.map(_(j)).min
      val rangeJ = dataSet.map(_(j) - minJ).max
      for (i <- 0 to k - 1) {
        Random.setSeed(i)
        centroids(i)(j) = minJ + rangeJ * Random.nextDouble()
      }
    }
    centroids
  }

  def MykMeans(dataSet: Array[Array[Double]],
               k: Int,
               distMeas: (Array[Double], Array[Double]) => Double = distEclud,
               createCent: (Array[Array[Double]], Int) => Array[Array[Double]] = randCent) = {
    val m = dataSet.length
    // 存储聚类结果 （质点，离该质点的距离）
    var clusterAssment = Array.fill(m)((0, 0.0))
    // 随机创建k个质点
    var centroids = createCent(dataSet, k)
    // 用于判断所有的数据点所在的簇是否不再改变
    var clusterChanged = true
    while (clusterChanged) {
      clusterChanged = false
      // 对每个数据点
      for (i <- 0 to m - 1) {
        var minDist = Double.MaxValue
        var minIndex = -1
        // 求与之最近的质点
        for (j <- 0 to k - 1) {
          val distJI = distMeas(centroids(j), dataSet(i))
          if (distJI < minDist) {
            minDist = distJI
            minIndex = j
          }
        }
        if (clusterAssment(i)._1 != minIndex)
          clusterChanged = true
        // 笔者认为这里应该的开方 而不不是平方
        clusterAssment(i) = (minIndex, math.sqrt(minDist))
      }
      centroids.foreach(x => println(x.mkString(" ")))
      println("——————————————————————————————————————————————")
      // 更新质点
      for (cent <- 0 to k - 1) {
        val centDataSet = dataSet.zip(clusterAssment).filter(_._2._1 == cent)
        val len = centDataSet.length
        // 保证簇中有数据点，才更新质点
        if (len > 0)
          centroids(cent) = centDataSet.map(_._1).reduce((a, b) => Array(a(0) + b(0), a(1) + b(1))).map(_ / len)
      }
    }
    (centroids, clusterAssment)
  }

  def main(args: Array[String]) {
    val dataSet = loadDataSet("testSet.txt")
    MykMeans(dataSet, 4)
  }
}