package bigknife.scalap.ast.types

case class QuorumSet(
    threshold: Int, // at least threshold members should say yes.
    validators: Vector[Node.ID], // a quorum slice to convince a node to believe something.
    innerSets: Vector[QuorumSet] // nesting inner quorum sets. (only allows 2 levels of nesting)
)
