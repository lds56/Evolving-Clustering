package whatever {

  class FeatureVector(val v: Map[String, Double]) {//raw: List[(Double, String)]) {

    def cos(that: FeatureVector): Double = {
      this.v.map {case(k, v) => that.v.getOrElse(k, 0.0) * v} sum
    }
  }

  object FeatureVector {

    def apply(content: String): FeatureVector =
      apply(new KeywordExtractor(content) extract)

    def apply(raw: List[(Double, String)]): FeatureVector =
      new FeatureVector(rawToFV(raw))

    def rawToFV(raw: List[(Double, String)]): Map[String, Double] = {
      val raw2 = raw //filter(_._1 >= 1)
      val sumOfSquare = raw2.map(el => el._1 * el._1).sum
      raw2.map {case (rank, name) => (name, rank / math.sqrt(sumOfSquare))} toMap
    }
    
  }

}
