package validation
import cats.data.Validated
// A => F[E,B]
//check is more of transformation than validation.
sealed trait Check[E,A,B]:
  import Check.Map
  def apply(a: A) : Validated[E,B]

  //if I can transform A to B, given B => C, Can I transform A to C
  def map[C](f: B => C): Check[E,A,C]  = Map(this, f)

object Check:
  case class Map[E,A,B, C](in : Check[E,A,B], f : B => C) extends Check[E,A,C]:
    override def apply(a: A): Validated[E, C] = in(a).map(f)
    

object CheckApp extends App:
  println("~~~~Start~~~~~")