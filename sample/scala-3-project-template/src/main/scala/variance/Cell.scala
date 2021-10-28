package variance

class Cell[T](v : T):
  private var value : T = v

  def get: T = value

  def set(nv: T): Unit =
    value = nv

/**
 * Why set(nv : T) is not covariant position
 * f = Cell[Orange]
 * g : Cell[Fruit] = f
 * g.set(apple)
 * orange = f.get // boom!!!
 */



object Main extends App:
  val card = new Cell[Int](4)
  println(card.get)

  card.set(100)
  println(card.get)