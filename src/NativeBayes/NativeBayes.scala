package NativeBayes

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

  def main(args: Array[String]): Unit = {
    val DataSet = loadDataSet()
    val listOPosts = DataSet._1
    val listClasses = DataSet._2
    val myVocabList = listOPosts.reduce((a1, a2) => a1.++:(a2)).distinct
    setOfWords2Vec(myVocabList, listOPosts(1)).foreach { x => print("[" + x + "] ") }
  }
}