object Leet631 extends App {

  import scala.collection.mutable

  class Excel(_height: Int, _width: Char) {

    //TreeMap of Y to TreeMap of X to value
    val data = mutable.TreeMap.empty[Int, mutable.TreeMap[Char, ExcelValue]]

    def set(row: Int, column: Char, `val`: Int) {
      data.get(row) match {
        case Some(rowData) => rowData(column) = Static(`val`)
        case None => data += (row -> mutable.TreeMap(column -> Static(`val`)))
      }
    }

    def get(row: Int, column: Char): Int = {
      data.get(row).flatMap(_.get(column)) match {
        case None => 0
        case Some(Static(value)) => value
        case Some(FormulaArr(formulaArr)) =>
          formulaArr.map {
            case Formula(fromRow, toRow, fromCol, toCol) =>
              data.range(fromRow, toRow + 1).map {
                case (y, rowData) => rowData.range(fromCol, (toCol + 1).toChar).map {
                  case (x, _) =>
                    get(y, x)
                }.sum
              }.sum
          }.sum
      }
    }

    def sum(row: Int, column: Char, numbers: Array[String]): Int = {
      val formulaArr = numbers.map {
        case str if str.contains(':') =>
          str.split(':') match {
            case Array(from, to) =>
              Formula(fromRow = from.tail.toInt, toRow = to.tail.toInt, fromCol = from.head, toCol = to.head)
          }
        case str => Formula(fromRow = str.tail.toInt, toRow = str.tail.toInt, fromCol = str.head, toCol = str.head)
      }
      data.get(row) match {
        case Some(rowData) => rowData(column) = FormulaArr(formulaArr)
        case None => data += (row -> mutable.TreeMap(column -> FormulaArr(formulaArr)))
      }
      get(row, column)
    }

    sealed trait ExcelValue

    case class Formula(fromRow: Int, toRow: Int, fromCol: Char, toCol: Char)

    case class FormulaArr(formulas: Array[Formula]) extends ExcelValue

    case class Static(value: Int) extends ExcelValue

  }

  /**
   * Your Excel object will be instantiated and called as such:
   * var obj = new Excel(height, width)
   * obj.set(row,column,`val`)
   * var param_2 = obj.get(row,column)
   * var param_3 = obj.sum(row,column,numbers)
   */

}
