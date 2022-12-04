package practise

import java.text.{FieldPosition, ParsePosition}
import java.util.Date

sealed trait Validation[+E, +A]

case class Success[A](value : A) extends Validation[Nothing, A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

object Validation:
  given validationApplicative[E]: Applicative[Validation[E, _]] with
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    extension[A](fa: Validation[E,A])
      override def map2[B, C](fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(v1), Success(v2)) => Success(f(v1,v2))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (Vector(h2) ++ t2))
        case (Success(_), f @ Failure(_, _)) => f
        case (f @ Failure(_, _), Success(_)) => f
      }


object ValidationApp extends App:
  case class WebForm(name: String, birthday: Date, phoneNumer: String)

  def validateName(name: String): Validation[String, String] =
    if name.isEmpty then Failure("name is empty") else Success(name)

  def validatePhoneNumber(number: String): Validation[String, String] =
    if number.isEmpty || !number.matches("[0-9]{10}") then Failure("number should be 10 digit") else Success(number)

  def validateBirthday(bday: String): Validation[String, Date] =
    import java.text.SimpleDateFormat
    try {
      Success(new SimpleDateFormat("yyyy-mm-dd").parse(bday))
    }catch{
      case _ => Failure("not valid date")
    }

  import Validation.given
  println(validateName("").map3(
    validateBirthday("asd1983-12-25"),
    validatePhoneNumber("dd9901033055"))(WebForm(_,_,_)))

