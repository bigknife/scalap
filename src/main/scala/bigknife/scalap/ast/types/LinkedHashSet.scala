package bigknife.scalap.ast.types

/**
  * a set-like data structure, but keep the inserting-order.
  */
sealed trait LinkedHashSet[A] {
  def +(a: A): LinkedHashSet[A] = LinkedHashSet.insert(this, a)
  def -(a: A): LinkedHashSet[A] = LinkedHashSet.remove(this, a)

  def ++(that: LinkedHashSet[A]): LinkedHashSet[A]

  def tag: String
  override def toString: String = LinkedHashSet.toString(this)

  def toVector: Vector[A]

  def contain(a: A): Boolean
  def notContain(a: A): Boolean = !contain(a)

  def filter(p: A => Boolean): LinkedHashSet[A]

  def foldLeft[B](z: B)(f: (B, A) => B): B

  def isEmpty: Boolean

  def hasGrownFrom(that: LinkedHashSet[A]): Boolean = LinkedHashSet.hasGrown(that, this)

  def unsafeHeadValue(): A

  def sliding(count: Int): Iterator[Seq[A]]
}

object LinkedHashSet {
  private case class SimpleLHS[A](data: Vector[A], tag: String) extends LinkedHashSet[A] {
    override def toVector: Vector[A] = data

    override def contain(a: A): Boolean = data.contains(a)

    override def filter(p: A => Boolean): LinkedHashSet[A] = SimpleLHS(data.filter(p), tag)

    override def foldLeft[B](z: B)(f: (B, A) => B): B = data.foldLeft(z)(f)

    override def ++(that: LinkedHashSet[A]): LinkedHashSet[A] = that match {
      case SimpleLHS(thatData, _) => SimpleLHS(data ++ thatData.filter(x => !data.contains(x)), tag)
      case _ => this // impossible
    }

    override def isEmpty: Boolean = data.isEmpty

    override def unsafeHeadValue(): A = data.head

    override def sliding(count: Int): Iterator[Seq[A]] = data.sliding(count)
  }

  def empty[A](tag: String): LinkedHashSet[A] = SimpleLHS(Vector.empty, tag)

  def insert[A](lhs: LinkedHashSet[A], a: A): LinkedHashSet[A] = lhs match {
    case x @ SimpleLHS(data, tag) => if (data.contains(a)) x else SimpleLHS(data :+ a, tag)
  }

  def remove[A](lhs: LinkedHashSet[A], a: A): LinkedHashSet[A] = lhs match {
    case _ @ SimpleLHS(data, tag) => SimpleLHS(data.filter(_ != a), tag)
  }

  /**
    * x2 has grown from x1
    * @return
    */
  def hasGrown[A](x1: LinkedHashSet[A], x2: LinkedHashSet[A]): Boolean = (x1, x2) match {
    case (SimpleLHS(d1, _), SimpleLHS(d2, _)) =>
      d2.length > d1.length && d2.containsSlice(d1)
  }

  def toString[A](lhs: LinkedHashSet[A]): String = lhs match {
    case SimpleLHS(data, tag) => s"$tag(" + data.mkString(",") + ")"
  }
}
