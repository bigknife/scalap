package bigknife.scalap.ast.types

case class Value[A: Ordered[A]](value: A)
