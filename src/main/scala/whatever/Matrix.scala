import Matrix._

case class RichMatrix(m: Matrix) {
  def T = transpose(m)

  def *(that:RichMatrix) = mXm( this.m, that.m )

  def apply(i:Int, j:Int) = m(i)(j)
  def rowCount = m.length
  def colCount = m.head.length

  def toStr = "\n" + m.map{
    _.map{"\t" + _}.reduceLeft(_ + _)+"\n"
  }.reduceLeft(_ + _)

  implicit def pimp(m:Matrix) = new RichMatrix(m)
}

object Matrix{

  type Row = List[Int]
  type Matrix = List[Row]

/*  def bigProd[T](t1: List[T], t2: List[T]) =
    (t1 zip t2).map{ t: (T, T) => t._1 * t._2 }.reduceLeft(_ + _) */

  def dotProd(v1: Row, v2: Row) =
    (v1 zip v2).map{ t: (Int, Int) => t._1 * t._2 }.reduceLeft(_ + _)

  def transpose(m: Matrix): Matrix =
    if (m.head.isEmpty) Nil
    else m.map(_.head) :: transpose(m.map(_.tail))

  def mXm(m1: Matrix, m2: Matrix): Matrix =
    for( row <- m1 ) yield
      for( col <- transpose(m2) ) yield
        dotProd( row, col )

  def apply(rowCount:Int, colCount:Int)(f:(Int, Int) => Int) =
    (for(i <- 1 to rowCount) yield
      (for(j <- 1 to colCount) yield f(i,j)).toList
    ).toList

}
