package traits

class Animal
trait Furry extends Animal
trait Legged extends Animal
trait FourLegged extends Legged

class Cat extends Animal with Furry with FourLegged


object MultiInheritence extends App
// Linearization below.
// Animal == Animal --> AnyRef --> Ref
// Furry == Furry --> Animal --> AnyRef --> Ref
// FourLegged == FourLegged --> Legged --> Animal --> AnyRef --> Ref
// Cat == Cat --> FourLegged --> Legged --> Furry --> Animal --> Anyref --> Ref