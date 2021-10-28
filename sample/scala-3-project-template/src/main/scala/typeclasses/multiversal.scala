package typeclasses

import typeclasses.multiversal.Apple


object multiversal extends App:
  //import CanEqual.derived
  class Apple derives CanEqual
  class RedApple extends Apple
  //object Apple
    //given eq: CanEqual[Apple, Apple] = CanEqual.derived
    //given eq1: CanEqual[Apple, Color] = CanEqual.derived
  class Color
  //println(Apple() == Color())
  println(Apple() == Apple())
  println(Apple() == RedApple())
  println(RedApple() == Apple())
  //println("some" == Some("some"))
  //println(Apple() == Some(Apple()))