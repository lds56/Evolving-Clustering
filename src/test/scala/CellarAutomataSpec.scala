
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers

import whatever._

class CellarAutomataSpec extends FunSpec with GivenWhenThen with Matchers {

/*  describe("The cell") {

    it("can compare the strength with another's") {

      given("A cell with strength ")

    }

 }*/

  describe("A cell kingdom") {

    it("should evolve without any immigration") {

      given("A kingdom with four cells")
      val fv1 = FeatureVector(List((3.0, "a"), (4.0, "b")))
      val fv2 = FeatureVector(List((9.0, "a"), (2.0, "b")))
      val fv3 = FeatureVector(List((6.0, "a"), (8.5, "b")))
      val fv4 = FeatureVector(List((2.0, "a"), (3.0, "b")))

      val cell1 = Cell(1, fv1)(Cell.ATTACKER)
      val cell2 = Cell(3, fv2)(Cell.ATTACKER)       
      val cell3 = Cell(5, fv3)(Cell.RESISTANT)      
      val cell4 = Cell(6, fv4)(Cell.RESISTANT)      

      when("evolving in iteration")
      val ck = new CellKingdom(List(cell1, cell2, cell3, cell4), 6)
      val res = ck.evolve.cells

      res map(_.toString + "||") foreach print
      println

      then("the second cell and the third one should belong to the same cluster")
      res(2).label should be (res(3).label)
      
    }

    it("should evolve with immigration") {

      given("a evolved cell kingdom")
      val fv1 = FeatureVector(List((3.0, "a"), (4.0, "b")))
      val fv2 = FeatureVector(List((9.0, "a"), (2.0, "b")))
      val fv3 = FeatureVector(List((6.0, "a"), (8.5, "b")))
      val fv4 = FeatureVector(List((10.0, "a"), (3.0, "b")))

      val cell1 = Cell(1, fv1)(Cell.ATTACKER)
      val cell2 = Cell(3, fv2)(Cell.RESISTANT)
      val cell3 = Cell(5, fv3)(Cell.RESISTANT)
      val cell4 = Cell(6, fv4)(Cell.RESISTANT)

      val ck = new CellKingdom(cell4 :: cell3 :: cell2 :: List(cell1), 6).evolve

      when("some new cells immigirate in")
      val fv5 = FeatureVector(List((8.0, "a"), (1.5, "b")))
      val fv6 = FeatureVector(List((17.0, "a"), (3.5, "b")))

      val cell5f = Cell(8, fv5)(_)
      val cell6f = Cell(8, fv6)(_)

      val newck = ck evolve(List(cell5f, cell6f))
      val res = newck.cells

      then("The 5th and 6th cells should belong to the cluster 2nd belongs to")
      //assert(res(1).label == res(4).label && res(1).label == res(5).label)
      (res(6-5).label, res(6-6).label) should equal (res(6-2).label, res(6-2).label)
    }

  }

}
