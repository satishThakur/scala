package chap9

trait JSON

object JSON {


  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err,Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))


    def array : Parser[JSON] = surround("[", "]"){
      value sep "," map (vs => JArray(vs.toIndexedSeq))
    }

    def obj: Parser[JSON] = surround("{", "}"){
      keyval sep "," map (kvs => JObject(kvs.toMap))
    }

    def keyval = escapedQuoted ** (":" *> value)

    def value = literal | array

    def literal =
      "null".as(JNull) |
      "true".as(JBool(true)) |
      "false".as(JBool(false)) |
      escapedQuoted.map(JString(_)) |
      double.map(JNumber(_))

    root(whitespace *> (obj | array))

  }
}