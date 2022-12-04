package com.satish.examples.effect.basics

import com.satish.examples.effect.basics.MyIO

case class MyIO[A](runUnsafe: () => A):
  def pure(a: => A): MyIO[A] = MyIO(() => a)
  def map[B](f: A => B): MyIO[B] = MyIO(() => f(this.runUnsafe()))
  def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(this.runUnsafe()).runUnsafe())

object Console:
  def write(s: => String): MyIO[Unit] =
    MyIO(() => print(s))



object IOMain extends App:

  val printName: MyIO[Unit] = Console.write("satish")
  val printPraise: MyIO[Unit] = Console.write(" is nice!!\n")

  val biggerMessage : MyIO[Unit] = for{
    _ <- printName
    _ <- printPraise
    _ <- printPraise
  } yield ()

  println("***** start ****")
  biggerMessage.runUnsafe()

