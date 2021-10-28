package fbound

//Self bounded F type polymorphism.
class Animal[A <: Animal[A]]:
  self: A =>
  def breed(): List[A] = ???

class Cat extends Animal[Cat]:
  override def breed(): List[Cat] = ???

class Dog extends Animal[Dog]:
  override def breed(): List[Dog] = ???

class Breed extends App
