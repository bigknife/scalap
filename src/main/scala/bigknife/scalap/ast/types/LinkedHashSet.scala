package bigknife.scalap.ast.types

/**
  * a set-like data structure, but keep the inserting-order.
  */
sealed trait LinkedHashSet[A] {
  def +(a: A): LinkedHashSet[A] = LinkedHashSet.insert(this, a)
  def -(a: A): LinkedHashSet[A] = LinkedHashSet.remove(this, a)

  def tag: String
  override def toString: String = LinkedHashSet.toString(this)

  def toVector: Vector[A]

  def contain(a: A): Boolean
  def notContain(a: A): Boolean = !contain(a)
}

object LinkedHashSet {
  private case class SimpleLHS[A](data: Vector[A], tag: String) extends LinkedHashSet[A] {
    override def toVector: Vector[A] = data

    override def contain(a: A): Boolean = data.contains(a)
  }

  def empty[A](tag: String): LinkedHashSet[A] = SimpleLHS(Vector.empty, tag)

  def insert[A](lhs: LinkedHashSet[A], a: A): LinkedHashSet[A] = lhs match {
    case x @ SimpleLHS(data, tag) => if (data.contains(a)) x else SimpleLHS(data :+ a, tag)
  }

  def remove[A](lhs: LinkedHashSet[A], a: A): LinkedHashSet[A] = lhs match {
    case _ @ (SimpleLHS(data, tag)) => SimpleLHS(data.filter(_ != a), tag)
  }

  def toString[A](lhs: LinkedHashSet[A]): String = lhs match {
    case SimpleLHS(data, tag) => s"$tag(" + data.mkString(",") + ")"
  }
}
