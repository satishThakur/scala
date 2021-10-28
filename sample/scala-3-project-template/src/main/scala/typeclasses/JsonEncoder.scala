package typeclasses

trait JsonEncoder[T]:
  def encode(t : T): String

  extension(t: T)
    def toJson: String = encode(t)

object JsonEncoder:
  given intEncoder: JsonEncoder[Int] with
    override def encode(t: Int): String = t.toString

  given stringEncoder: JsonEncoder[String] with
    override def encode(t: String): String = s"\"$t\""

  given boolEncoder: JsonEncoder[Boolean] with
    override def encode(t: Boolean): String = t.toString

  given listEncoder[T](using JsonEncoder[T]): JsonEncoder[List[T]] with
    override def encode(ts: List[T]): String = ts.map(_.toJson).mkString("[", ",", "]")

object ToJsonMethods:
  extension[T](t: T)(using jer: JsonEncoder[T])
    def toJson: String = jer.encode(t)

object EncoderApp extends App:
  import ToJsonMethods.toJson
  println(22.toJson)
  println("abcddd".toJson)

  println(List("hello", "there").toJson)
