import scala.math
import scala.util.Random

package whatever {
 
  object TypeDefiner {

    val eps = 0.00000000001

    type Cells = List[Cell]

    implicit class PowerInt(val i:Double) extends AnyVal {
      def ** (exp:Double):Double = math.pow(i,exp)
    }

  }

  import TypeDefiner._

  class Cell(val strength: Double, val label: Int,
    val born: Int, val fv: FeatureVector, val news: News){

    def isHowFarFrom(that: Cell) = Cell.distanceFunc(this, that)

    /* born factor?
     * born times distance?
     */

    def compareStrength(that: Cell): Cell = {
      val newStrength = Cell.monoDecFunc(this isHowFarFrom that) * that.strength

      //println("that :" + that.toString + ", strength: " + newStrength)

      if (newStrength > this.strength)
        new Cell(newStrength, that.label, this.born, this.fv, this.news)
      else this
    }

    def age(deltaTime: Int): Cell = {
      require(deltaTime > 0, "delta time should be positive")
      new Cell(strength * (Cell.agingRate ** deltaTime), label, born, fv, news)
    }

    def applyRule(neighbors: Cells): Cell = {
      //println("Apply in " + this.toString)
      neighbors.foldLeft(this)(_ compareStrength _)
    }

    def isOldAt(currentTime: Int): Boolean = Cell.isOld(this, currentTime)

    def diff(that: Cell): Double = Cell.diff(this, that)

    override def toString: String = "Cell(#" + born + ", ^" +
      label + ", "+ "%.3f".format(strength) + ")"

  }

  object Cell {

    val agingRate = 1.00
    val elderStrength = 0.1
    val elderAge = 24 * 60 * 10.0
    val bornGap = 24 * 60 * 2.0

    val resistanceStrength = 0.3
    val attackStrength = 1.0
    val maxStrength = 15.0
    val RESISTANT = "resistant"
    val ATTACKER = "attacker"

    val linearDecFunc = (x: Double) => x
    val sigmoidDecFun = (t: Double) => 1 / ( 1 + math.exp(-t))
    val sigmoidWidth = 12.0
    val effectiveWidth = 1.0

    val testNews = News("test_time", "test_text", "test_labe")

    val rad = new Random(19941018)

    def newCharacter: String = if (rad nextBoolean) RESISTANT else ATTACKER

    def newLabel(seed: Int): Int = seed

    def newStrength(seed: Int): Double =
      (new Random(seed).nextGaussian + maxStrength/1.0) / maxStrength match {
        case rad if rad > 2 => 2.0
        case rad if rad < 0 => 0.0
        case rad => rad
      }

    def apply(news: News)(character: String): Cell = (
      apply(Preprocessor convertTime news.Time, FeatureVector(news.Text), news)
        (character)
    )

    def apply(bornTime: Int, fv: FeatureVector, news: News = testNews)
      (character: String): Cell =

      character match {
        case ATTACKER =>
          new Cell(attackStrength, newLabel(bornTime), bornTime, fv, news)
        case RESISTANT =>
          new Cell(resistanceStrength, newLabel(bornTime), bornTime, fv, news)
      }

    def monoDecFunc(x: Double): Double = linearDecFunc(x)
      //sigmoidDecFun(sigmoidWidth / effectiveWidth * (-x) + sigmoidWidth / 2)

    def distanceFunc(thisCell: Cell, thatCell: Cell): Double = {
      math.abs(thisCell.fv cos thatCell.fv)
    } ensuring(res => res <= 1 + eps && res >= 0 - eps,
      "The distance should vary between 0 and 1.")

    def isOld(theCell: Cell, currentTime: Int): Boolean =
      currentTime - theCell.born > elderAge && theCell.strength < elderStrength

    def diff(thisCell: Cell, thatCell: Cell): Double =
      math.abs(thisCell.strength - thatCell.strength)

  }

  class CellKingdom(val cells: Cells, val GreenWichTime: Int) {

    /* Here we assume that the time of immigration 
     * equals to the born time of the immigrator.
     */

    def neighborsOf(theCell: Cell): Cells = // find forward
      CellKingdom.neighborsOf(this.cells, theCell)

    def evolve: CellKingdom =
      new CellKingdom(CellKingdom.iterEvolve(cells, 0), GreenWichTime)

    def evolve(currentTime: Int): CellKingdom = 
      this eliminateElder(currentTime) evolve

    def evolve(newCell: Cell): CellKingdom = {
      this eliminateElder(newCell.born) immigrate(newCell) evolve
    }

    def evolve(newCellf: String => Cell): CellKingdom = {

      def sumStr(cs: Cells): Double = cs.map(_.strength).sum

      val attackerEvo = this evolve newCellf(Cell.ATTACKER)
      val resistantEvo = this evolve newCellf(Cell.RESISTANT)

      if (sumStr(attackerEvo.cells) - (sumStr(this.cells) + Cell.attackStrength) <
        sumStr(resistantEvo.cells) - (sumStr(this.cells) + Cell.resistanceStrength))
        resistantEvo
      else attackerEvo
    }

    def evolve(newCellfs: List[String => Cell]): CellKingdom =
      newCellfs.foldLeft(this)(_ evolve _)

    def eliminateElder(currentTime: Int): CellKingdom = {

      // println(this.cells)

      (currentTime  - this.GreenWichTime) match {
        case dt if dt == 0 => this
        case dt if dt >  0 => new CellKingdom(
          cells.map(_ age dt).filterNot(_ isOldAt currentTime), currentTime)
        case _ => throw new Exception(
          "EliminationException: " + currentTime +" is earlier than " + this.GreenWichTime)
      }
    }

    def isConquered(theCell: Cell): Boolean =
      theCell == (theCell applyRule (this neighborsOf theCell))

    /* // choose character by testing if conquered
    def chooseCharacter(cf: String => Cell): Cell =
      if (isConquered(cf apply Cell.RESISTANT)) cf apply Cell.ATTACKER
      else cf apply Cell.RESISTANT
     */

    def immigrate(newCell: Cell): CellKingdom =
      new CellKingdom(newCell :: cells, newCell.born)

    /* // general migration function
     def migrate[T](combine: (Cells, T) => Cells)
      (newCell: T, currentTime: Int): CellKingdom = {
      new CellKingdom(combine(cells, newCell), currentTime)
    }
     */

  }

  object CellKingdom {

    val iterThreshold = 0.0001
    val iterMaxTime = 1000

    //def init(theCells: Cells): CellKingdom =

    def apply(characterFuncs: List[String => Cell], lastTime: Int): CellKingdom = 
      new CellKingdom(characterFuncs map(_ apply(Cell.newCharacter)), lastTime)

    def offlineEvolve(characterFuncs: List[String => Cell]): CellKingdom = {

      def applyPioneer(pioneerf: String => Cell): CellKingdom =
        new CellKingdom(List(pioneerf(Cell.ATTACKER)), pioneerf(Cell.ATTACKER).born)

      characterFuncs match {
        case pioneer :: Nil => applyPioneer(pioneer)
        case pioneer :: migrants => applyPioneer(pioneer).evolve(migrants)
        case _ => throw new Exception("No cells!!!")
      }
    }

    def neighborsOf(theCells: Cells, theCell: Cell): Cells = 
      theCells filter(c => c != theCell &&
        math.abs(c.born - theCell.born) < Cell.bornGap)

    def iterEvolve(theCells: Cells, iterTime: Int): Cells = {

      //theCells.reverse map(_.toString + "|") foreach print
      //println

      assert(iterTime < iterMaxTime, "exccesive iteration")

      val newCells = theCells map(c => c applyRule neighborsOf(theCells, c))
      if ((newCells zip theCells).map(t => t._1 diff t._2).sum > iterThreshold)
        iterEvolve(newCells, iterTime + 1)
      else theCells
    }

  }

}
