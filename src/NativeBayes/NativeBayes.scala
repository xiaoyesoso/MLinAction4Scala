package NativeBayes

import scala.collection.mutable.ArrayBuffer

object NativeBayes {

  def loadDataSet() = {
    val postingList = Array(Array("my", "dog", "has", "flea", "problems", "help", "please"),
      Array("maybe", "not", "take", "him", "to", "dog", "park", "stupid"),
      Array("my", "dalmation", "is", "so", "cute", "I", "love", "him"),
      Array("stop", "posting", "stupid", "worthless", "garbage"),
      Array("mr", "licks", "ate", "my", "steak", "how", "to", "stop", "him"),
      Array("quit", "buying", "worthless", "dog", "food", "stupid"))
    //1 代表不良信息, 反之为 0 
    val classVec = Array(0, 1, 0, 1, 0, 1)
    (postingList, classVec)
  }

  def setOfWords2Vec(vocabList: Array[String], inputSet: Array[String]) = {
    val returnVec = new Array[Int](vocabList.length)
    val vocabListWithIndex = vocabList.zipWithIndex
    for (word <- inputSet) {
      if (vocabList.contains(word))
        returnVec(vocabListWithIndex.filter(_._1 == word)(0)._2) = 1
      else printf("the word: %s is not in my Vocabulary!\n", word)
    }
    returnVec
  }

  def trainNB0(trainMatrix: Array[Array[Int]], trainCategory: Array[Int]) = {
    val numTrainDocs = trainMatrix.length
    val numWords = trainMatrix(0).length
    val pAbusive = trainCategory.sum / numTrainDocs.toDouble
    var p0Num = Array.fill(numWords)(1)
    var p1Num = Array.fill(numWords)(1)
    var p0Denom = 2.0
    var p1Denom = 2.0
    for (i <- 0 to numTrainDocs - 1) {
      if (trainCategory(i) == 1) {
        var cnt = 0
        p1Num = p1Num.map { x =>
          val v = x + trainMatrix(i)(cnt)
          cnt += 1
          v
        }
        p1Denom += trainMatrix(i).sum
      } else {
        var cnt = 0
        p0Num = p0Num.map { x =>
          val v = x + trainMatrix(i)(cnt)
          cnt += 1
          v
        }
        p0Denom += trainMatrix(i).sum
      }
    }
    (p1Num.map(x => math.log(x / p1Denom)), p0Num.map(x => Math.log(x / p0Denom)), pAbusive)
  }

  def classifyNB(vec2Classify: Array[Int], p0Vec: Array[Double], p1Vec: Array[Double], pClass1: Double) = {
    var cnt = 0
    val p1 = vec2Classify.map { x =>
      val v = x * p1Vec(cnt)
      cnt += 1
      v
    }.sum + math.log(pClass1)
    cnt = 0
    val p0 = vec2Classify.map { x =>
      val v = x * p0Vec(cnt)
      cnt += 1
      v
    }.sum + math.log(1.0 - pClass1)

    if (p1 > p0) 1 else 0
  }

  def main(args: Array[String]): Unit = {
    val DataSet = loadDataSet()
    val listOPosts = DataSet._1
    val listClasses = DataSet._2
    val myVocabList = listOPosts.reduce((a1, a2) => a1.++:(a2)).distinct
    var trainMat = new ArrayBuffer[Array[Int]](listOPosts.length)
    listOPosts.foreach(postinDoc => trainMat.append(setOfWords2Vec(myVocabList, postinDoc)))

    val p = trainNB0(trainMat.toArray, listClasses)
    val p0V = p._2
    val p1V = p._1
    val pAb = p._3
    val testEntry = Array("love", "my", "dalmation")
    val thisDoc = setOfWords2Vec(myVocabList, testEntry)
    println(testEntry.mkString(",") + " classified as: " + classifyNB(thisDoc, p0V, p1V, pAb))
    val testEntry2 = Array("stupid", "garbage")
    val thisDoc2 = setOfWords2Vec(myVocabList, testEntry2)
    println(testEntry2.mkString(",") + " classified as: " + classifyNB(thisDoc2, p0V, p1V, pAb))
  }
}