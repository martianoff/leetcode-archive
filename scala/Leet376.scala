object Leet376 extends App {

  object Solution {
    def wiggleMaxLength(nums: Array[Int]): Int = {
      nums.foldLeft(UpDownTrends(up = 0, down = 0, prevNumber = None)) {
        // init first number
        case (UpDownTrends(_, _, None), number) =>
          UpDownTrends(up = 1, down = 1, prevNumber = Some(number))
        case (UpDownTrends(lastUp, lastDown, Some(prevNumber)), number) if number > prevNumber =>
          UpDownTrends(up = lastUp max (lastDown + 1), down = lastDown, prevNumber = Some(number))
        case (UpDownTrends(lastUp, lastDown, Some(prevNumber)), number) if number < prevNumber =>
          UpDownTrends(up = lastUp, down = lastDown max (lastUp + 1), prevNumber = Some(number))
        case (UpDownTrends(lastUp, lastDown, Some(prevNumber)), number) if number == prevNumber =>
          UpDownTrends(up = lastUp, down = lastDown, prevNumber = Some(number))
      } match {
        case UpDownTrends(lastUp, lastDown, _) => lastUp max lastDown
      }
    }

    case class UpDownTrends(up: Int, down: Int, prevNumber: Option[Int])
  }
}
