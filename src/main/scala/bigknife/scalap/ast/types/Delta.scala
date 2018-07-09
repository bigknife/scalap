package bigknife.scalap.ast.types

/**
  * represent a changed data
  * @param data latest data
  * @param changed if the data changed since before
  * @tparam A data type
  */
case class Delta[A](data: A, changed: Boolean) {
  def unchanged: Boolean = !changed
}

object Delta {
  def changed[A](data: A): Delta[A] = Delta(data, changed = true)
  def unchanged[A](data: A): Delta[A] = Delta(data, changed = false)

  trait Syntax {
    implicit final class DeltaSyntax[A](a: A) {
      def changed: Delta[A] = Delta.changed(a)
      def unchanged: Delta[A] = Delta.unchanged(a)
    }
  }
}
