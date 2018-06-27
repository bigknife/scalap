package bigknife.scalap.ast.types

case class QuorumSet(
    threshold: Int, // at least threshold members should say yes.
    validators: Vector[Node.ID], // a quorum slice to convince a node to believe something.
    innerSets: Vector[QuorumSet] = Vector.empty // nesting inner quorum sets. (only allows 2 levels of nesting)
) {

  def withThreshold(threshold: Int): QuorumSet               = copy(threshold = threshold)
  def withValidators(validators: Vector[Node.ID]): QuorumSet = copy(validators = validators)
  def withInnerSets(innerSets: Vector[QuorumSet]): QuorumSet = copy(innerSets = innerSets)

  def addValidator(validator: Node.ID): QuorumSet  = copy(validators = this.validators :+ validator)
  def addInnerSet(quorumSet: QuorumSet): QuorumSet = copy(innerSets = this.innerSets :+ quorumSet)

  override def toString: String = {
    val validatorsStr =
      validators.map(x => x.value.take(3).map("%02x" format _).mkString("")).mkString(",")

    if (innerSets.isEmpty) {
      s"QS($threshold,Validators($validatorsStr)"
    } else {
      val innerSetsStr = innerSets.map(_.toString).mkString(",")
      s"QS($threshold,Validators($validatorsStr),Inner($innerSetsStr))"
    }
  }

  /*
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: QuorumSet =>
      this.threshold == that.threshold &&
      this.validators == that.validators &&
      this.innerSets == that.innerSets
    case _ => false
  }*/
}

object QuorumSet {
  val Empty: QuorumSet = QuorumSet(
    0,
    Vector.empty,
    Vector.empty
  )
}
