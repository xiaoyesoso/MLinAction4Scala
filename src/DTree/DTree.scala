package DTree

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

class MyTree(a: String, b: Map[Int, MyTree]) {
  var nodes = b
  val value = a
}

object DTree {
  def createDataSet() = {
    val dataSet = Array((Array(1, 1), "yes"), (Array(1, 1), "yes"),
      (Array(1, 0), "no"), (Array(0, 1), "no"), (Array(0, 1), "no"))
    // 这应该称为属性 而不是标签
    val TreeAttributes = Array("no surfacing", "flippers")
    (dataSet, TreeAttributes)
  }

  def calShannonEnt(dataSet: Array[Tuple2[Array[Int], String]]) = {
    val numEntries = dataSet.length
    var labelCounts: Map[String, Int] = Map.empty
    for (featVec <- dataSet) {
      val currentLabel = featVec._2
      labelCounts(currentLabel) = labelCounts.getOrElse(currentLabel, 0) + 1
    }
    var shannoEnt = 0.0
    for (value <- labelCounts.values) {
      val prob = value.toDouble / numEntries
      shannoEnt -= prob * math.log(prob) / math.log(2)
    }
    shannoEnt
  }

  def splitDataSet(dataSet: Array[Tuple2[Array[Int], String]], axis: Int, value: Int) = {
    var retDataSet: ArrayBuffer[Tuple2[Array[Int], String]] = ArrayBuffer.empty
    for (featVec <- dataSet) {
      if (featVec._1(axis) == value) {
        val reducedFeatvec = featVec._1.zipWithIndex.filter(_._2 != axis).map(_._1)
        retDataSet.+=((reducedFeatvec, featVec._2))
      }
    }
    retDataSet.toArray
  }

  def chooseBestFeatureToSplit(dataSet: Array[Tuple2[Array[Int], String]]) = {
    val baseEntropy = calShannonEnt(dataSet)
    var bestInfoGain = 0.0
    var bestFeature = -1
    for (i <- 0 to dataSet(0)._1.length - 1) {
      val uniqueVals = dataSet.map(_._1(i)).toSet
      var newEntropy = 0.0
      for (value <- uniqueVals) {
        val subDataSet = splitDataSet(dataSet, i, value)
        newEntropy += subDataSet.length.toDouble / dataSet.length * calShannonEnt(subDataSet)
      }
      val infoGain = baseEntropy - newEntropy
      if (infoGain > bestInfoGain) {
        bestInfoGain = infoGain
        bestFeature = i
      }
    }
    bestFeature
  }

  def creatTree(dataSet: Array[Tuple2[Array[Int], String]], attribute: Array[String]): MyTree = {
    val classList = dataSet.map(_._2);
    if (classList.count(_ == classList(0)) == classList.length) {
      new MyTree(classList(0), Map.empty)
    } else if (dataSet.length == 1) {
      val str = classList.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.map(_._2).reduce((x, y) => x + y))).toList.maxBy(_._2)._1
      new MyTree(str, Map.empty)
    } else {
      val bestFeat = chooseBestFeatureToSplit(dataSet)
      val bestFeatAttribute = attribute(bestFeat)
      var myTree = new MyTree(bestFeatAttribute, Map.empty)
      var Vattribute = attribute
      Vattribute = Vattribute.filter(_ != bestFeatAttribute)
      val uniqueVals = dataSet.map(_._1(bestFeat)).distinct
      for (value <- uniqueVals) {
        myTree.nodes.+=((value -> creatTree(splitDataSet(dataSet, bestFeat, value), Vattribute)))
      }
      myTree
    }
  }

  def classify(inputTree: MyTree, attribute: Array[String], testVec: Array[Int]): String = {
    var classLabel = ""
    val firstStr = inputTree.value
    val secondTrees = inputTree.nodes
    val featIndex = attribute.zipWithIndex.filter(_._1 == firstStr)(0)._2
    for (key <- secondTrees.keySet) {
      if (testVec(featIndex) == key) {
        if (secondTrees(key).nodes.isEmpty)
          return secondTrees(key).value
        else classLabel = classify(secondTrees(key), attribute, testVec)
      }
    }
    classLabel
  }

  def main(agrs: Array[String]): Unit = {
    val name = 1
    val DataSet = createDataSet()
    val dataSet = DataSet._1
    val attribute = DataSet._2
    val inputTree = creatTree(dataSet, attribute)
    println(classify(inputTree, attribute, Array(1, 1)))
  }
}