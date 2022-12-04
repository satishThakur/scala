package practise

import java.time.LocalDateTime

object EitherValidation extends App:

  def validateName(name : String) : Either[String, String] = if name.startsWith("sec") then Right(name) else Left("no name")
  def validBirthdate(date: LocalDateTime): Either[String, LocalDateTime] = Right(date)
  def validatePhoneNumer(number: Int) : Either[String, Int] = if number > 100 then Right(number) else Left("wrong  number!!")

  case class Person(name: String, bday: LocalDateTime, phoneNumber: Int)
  
  case class WebForm(name: String, bday: LocalDateTime, phoneNumber: Int)

  type EitherStringOr[T] = Either[String, T]

  def validatePerson(p : Person): EitherStringOr[WebForm] =
    validateName(p.name).flatMap(s =>
      validBirthdate(p.bday).flatMap(b => validatePhoneNumer(p.phoneNumber).map(WebForm(s,b,_))))
