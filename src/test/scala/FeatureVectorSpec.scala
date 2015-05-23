import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers

import whatever._

class FeatureVectorSpec extends FunSpec with GivenWhenThen with Matchers {

  describe("A feature vector") {

    it("should be normalized") {

      given("a normalized feature vector")
      val fv = FeatureVector(List((3.0, "a"), (4.0, "b")))

      when("normalized")

      then("its norm should be 1")
      fv.v.map(t => t._2 * t._2).sum should be (1.0)

    }

    it("should has distance function") {

      given("three feature vectors, fv1, fv2 and fv3")
      val fv1 = FeatureVector(List((3.0, "a"), (4.0, "b")))
      val fv2 = FeatureVector(List((9.0, "a"), (2.0, "b")))
      val fv3 = FeatureVector(List((3.0, "a"), (8.0, "b")))

      when("comparing the distance from fv1 to fv2 and fv3 respectively")
      val d12 = fv1 cos fv2
      val d13 = fv1 cos fv3

      then("fv3 should be closer to fv1 than fv2")
      d12 should be < d13

    }

  }

}
