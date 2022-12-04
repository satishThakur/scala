package fpsession.syntax

trait Printable[A]: //<>
  def print(a: A) : String

class StringPrintable extends Printable[String]:
  override def print(a: String): String = s"[$a]"


object Helper:
  def sum(ls : List[Int]): Int = ls match{
    case x :: xs => x + sum(xs)
    case Nil => 0
  }


