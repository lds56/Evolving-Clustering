
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers

import whatever._

class PreprocessorSpec extends FunSpec with GivenWhenThen with Matchers {

  describe("a preprocesseor") {

    it("should handle with some test files") {

      given("A tiny test set")

      when("parsing the test file into an valid object")
      val news0 = Preprocessor.parseJson("resources/test_news/news0")

      then("the time attribute should be parsed correctly")
      news0.Time should be ("2015-05-20 12:23:00")

      and("the text attribute should be parsed coorectly")
      news0.Text should startWith ("Apple recently announced that")

    }

  }

}
