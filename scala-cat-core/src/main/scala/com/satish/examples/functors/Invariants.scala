package com.satish.examples.functors

object Invariants:
  trait Codec[A]:
    self =>
    def encode(a: A): String
    def decode(s: String): A
    def imap[B](enc: B => A, dec: A => B): Codec[B] = new Codec[B] {
      override def encode(a: B): String = self.encode(enc(a))
      override def decode(s: String): B = dec(self.decode(s))
    }
