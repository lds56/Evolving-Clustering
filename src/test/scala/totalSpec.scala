import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import scala.io.Source
import scala.util.Random

import whatever._

class TotalSpec extends FunSpec with GivenWhenThen with Matchers {

  val listOfNews = (0 to 36) map(id => Preprocessor parseJson
    ("resources/test_news/news" + id))


  def grades(n: Int): Double = {

    val theCellfs = listOfNews.take(n).map(ns => Cell.apply(ns)_) toList

    val ck = CellKingdom.offlineEvolve(theCellfs)

    val ln = listOfNews.take(n)

    val cs = ck.cells.reverse

    val sim = ln.map(t1 => ln.map(t2 =>
      if (t1.label == t2.label) 1 else 0))

      println("--------------")
      sim.foreach(t1 => {t1.foreach(t2 => print(t2 + " "));println})
      println("--------------")

    val res = cs.map(t1 => cs.map(t2 =>
      if (t1.label == t2.label) 1 else 0))


    println("--------------")
    res.foreach(t1 => {t1.foreach(t2 => print(t2 + " "));println})
    println("--------------")

    val FNTN = cs.groupBy(ns => ns.label).toList.combinations(2)
      .map(t => t.head._2.size * t.last._2.size).sum

    println("FNTN: " + FNTN)

    val TN = (0 to n-1).combinations(2).filter(l =>
      sim(l.head)(l.last) == 0 && res(l.head)(l.last) == 0).size

    println("TN: " + TN)

    val FN = FNTN - TN

    val TPFP  = cs.groupBy(ns => ns.label).toList.map{case(lb, lc) =>
      lc.size * lc.size}.sum

    println("TPFP: " + TPFP)

    val TP = (0 to n-1).combinations(2).filter(l =>
      sim(l.head)(l.last) == 1 && res(l.head)(l.last) == 1).size

    println("TP: " + TP)

    val FP = TPFP - TP

    val R = TP / (TP + FN).toDouble

    val P = TP / (TP + FP).toDouble

    2 * P * R / (P + R)

    //val grade = (sim zip res).map(t => (t._1 zip t._2)
      //.map(tt => if (tt._1 == tt._2) 1 else 0))

    //grade.map(_.sum).sum / (n.toDouble*n.toDouble)

  }

  describe("A clustering based on AC") {

    it("should generate some clusters") {

      //val n = 37

      given("a tiny test set") //36 later

      
      val fvs = listOfNews.take(10).map(c => FeatureVector(c.Text))
      val fvM = fvs.map(t1 => fvs.map(t2 => t1 cos t2))
      println("=============")
      fvM.foreach(t1 => {t1.foreach(t2 => print(f"$t2%.2f" + " "));println})
      println("==============")
      


      when("stating evolving")

      then("Boom!!!")

      //val listOfNews = (0 to 49) map(id => Preprocessor parseJson
        //("resources/news5/news" + id))

      
      //val theCellfs = listOfNews.zipWithIndex.map(ns => Cell.apply(ns._1)_) toList

      //val ck = CellKingdom.offlineEvolve(theCellfs)

      //println(ck.cells.groupBy(g => g.label).size)
        //foreach(c => println())

      val cc = (5 to 36).map(t => grades(t))
      //val rd = new Random()
      val cc2 = cc.map(t => Cell.newStrength(1018) * t)
      cc2 foreach println

 //     for 

      //val ck = new CellKingdom(List())
      //when()

    }

  }


}
