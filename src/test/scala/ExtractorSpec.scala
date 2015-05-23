//package whatever

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers

import whatever._

class ExtractorSpec extends FunSpec with GivenWhenThen with Matchers {

  describe("An extractor") {

    it("should generate a hash table") {

      given("a list of words")
      val lw = List("aa", "b", "aa", "ccc", "d")

      when("generating a hash table")
      val ext = new Extractor
      val ht = ext.genHash(lw)

      then("the distinct words will in the result hash table")
      val res = Map("aa" -> 0, "b" -> 1, "ccc" -> 2, "d" -> 3)
      ht should be (res)

    }


    it("should filter stop words") {

      given("a line of text")
      val testContent = "compatibility of systems of linear constraints " +
        "over the set of natural numbers."

      when("filtering is applied")
      val ext = new Extractor
      val filteredList = ext.filterStop(ext.splitWords(testContent))

      then("the result list should have no stop words")
      val testList = List("compatibility", "systems", "linear", "constraints",
        "set", "natural", "numbers")

      filteredList should be (testList)

    }
  }

  describe("A keyword Extractor") {

    it("should generate a distance matrix") {

      given("a passage")
      val ext = new KeywordExtractor("test2")

      when("calculating a distance matrix")
      val dm = ext.calcWordDist(2)

      then("the distance matrix should be identical to this")
      /*val result = List(
        List(0, 1, 0, 0, 0, 0),
        List(1, 0, 1, 1, 0, 0),
        List(0, 1, 0, 0, 0, 0),
        List(0, 1, 0, 0, 1, 0),
        List(0, 0, 0, 1, 0, 1),
        List(0, 0, 0, 0, 1, 0)
       )*/
      val result = Map(0 -> List(1), 1 -> List(0, 2, 3), 2 -> List(1),
        3 -> List(1, 4), 4 -> List(3, 5), 5 -> List(4))
      dm should be (result)
    }

    it("should calculate the rank of words") {

      given("a passage of words")
      val ext = new KeywordExtractor("keyword_extractor_test")

      when("applying the textrank algorithmn")
      val rankM = ext.extract

      then("""the word "system" should have highest rank""")
      rankM(0)._2 should be ("minimal")
      //val res = rankM.zipWithIndex reduceLeft((t1, t2) => if (t1._1 < t2._1) t2 else t1)
      //(ext.indexHash apply res._2) should be ("minimal")

    }

    it ("should handle news") {

      given("a list of news")
      val news1ext = new KeywordExtractor("news1")
      val news2ext = new KeywordExtractor("news2")
      val news3ext = new KeywordExtractor("news3")
      val news4ext = new KeywordExtractor("news4")
      val news5ext = new KeywordExtractor("news5")
      val news6ext = new KeywordExtractor("news6")

      when("extracting their keywords")
      val ky1 = news1ext.extract
      val ky2 = news2ext.extract
      val ky3 = news3ext.extract
      val ky4 = news4ext.extract
      val ky5 = news5ext.extract
      val ky6 = news6ext.extract

      then("ky")
      // ky1 foreach println

    }

    it("should handle raw news") {

      given(" a raw news")
      val news = Preprocessor parseJson "resources/test_news/news0"

      when("extracting keywords from the news")
      val ky = new KeywordExtractor(news.Text) extract

      then("the keywords should contain 'Apple'")
      ky(0)._2 should (be ("retina") or be ("imac") or be ("apple"))
      //ky foreach println

    }
  }

  describe("A summary extractor") {

    it("should calculate a correct similarity with two sentences") {

      given("two sentences")
      val s1 = ("Compatibility of system of linear constraints " +
        "over the set of natural numbers") toLowerCase
      val s2 = ("Criteria of compatibility of a system of linear " +
        "Diophantine equations, strict inequations, " +
        "and nonstrict inequations are considered" ) toLowerCase

      when("calculating the similarity of these two sentences")
      val ext = new SummaryExtractor("summary_extractor_test")
      val sim = ext.calcSimilarity(ext.filterStop(ext.splitWords(s1)),
        ext.filterStop(ext.splitWords(s2)))

      then("the similarity should be this")
      assert(sim - 0.69 < 0.01)

    }

    it ("should extract the summary of a passage") {

      given("a passage of text")
      val ext = new SummaryExtractor("summary_extractor_test")

      when("calculating the ranks of sentences")
      val rankM = ext.extract
      // rankM foreach println

      then("the summary (aka the sentence with the highest rank) should be this")
      rankM(0)._2 should contain ("minimal")

    }

    it("should generate a distance matrix") {

      given("a passage of text")
      val ext = new SummaryExtractor("summary_extractor_test")

      when("calculating a distance matrix")
      val dm = ext.dists
      //dm.foreach(println)

      then("the distance matrix should be symmetry")
      dm(0)(1) should be (dm(1)(0))
      //dm(0)(1) should be (1.0)
      
    } 
  }

}
