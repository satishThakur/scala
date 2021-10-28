package extn

trait TwosCompliment[N]:
  def equalsMin(n : N): Boolean
  def abs(n : N) : N

object TwosCompliment:
  given TwosCompliment[Int] with
    override def abs(n: Int): Int = n.abs
    override def equalsMin(n: Int): Boolean = n == Int.MinValue

  given TwosCompliment[Byte] with
    override def abs(n: Byte): Byte = n.abs
    override def equalsMin(n: Byte): Boolean = n == Byte.MinValue

extension[N](n: N)(using tc: TwosCompliment[N])
  def isMinValue: Boolean = tc.equalsMin(n)
  def abs: Option[N] = if isMinValue then None else Some(tc.abs(n))

object Compliment extends App:
  println(Int.MinValue.isMinValue)
  println(Int.MinValue.abs)
