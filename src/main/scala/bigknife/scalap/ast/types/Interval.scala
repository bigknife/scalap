package bigknife.scalap.ast.types

case class Interval(first: Int, second: Int) {
  def bothZero: Boolean = first == 0 && second == 0

  def setBoth(i: Int): Interval = copy(first = i, second = i)
  def shiftLeft(i: Int): Interval = copy(first = second, second = i)
}
