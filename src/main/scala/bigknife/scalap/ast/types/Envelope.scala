package bigknife.scalap.ast.types

case class Envelope[M <: Message](message: M, signature: Signature)
