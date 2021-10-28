package variance

class InvariantParking[T](things: List[T]):
  def park(vehicle: T): InvariantParking[T] = ???
  def impound(vehilces: List[T]): InvariantParking[T] = ???
  def check(cond: String): List[T] = ???
  def flatMap[U](f: T => InvariantParking[U]): InvariantParking[U] = ???

class CovariantParking[+T](things: List[T]):
  def park[U >: T](vehicle: U): CovariantParking[T] = ???
  def impound[U >: T](vehilces: List[U]): CovariantParking[T] = ???
  def check(cond: String): List[T] = ???
  def flatMap[U](f: T => CovariantParking[U]): CovariantParking[U] = ???

class ContravariantParking[-T](things: List[T]):
  def park(vehicle: T): ContravariantParking[T] = ???
  def impound(vehilces: List[T]): ContravariantParking[T] = ???
  def check[U <: T](cond: String): List[U] = ???
  def flatMap[S <: T, U](f: S => ContravariantParking[U]): ContravariantParking[U] = ???


class IList[T]

class InvariantParkingi[T](things: IList[T]):
  def park(vehicle: T): Unit = ???
  def impound(vehilces: IList[T]): Unit = ???
  def check(cond: String): IList[T] = ???

class CovariantParkingi[+T](things: IList[T]):
  def park[U >: T](vehicle: U): Unit = ???
  def impound[U >: T](vehilces: IList[U]): Unit = ???
  def check[U >: T](cond: String): IList[U] = ???

class ContravariantParkingi[-T](things: IList[T]):
  def park(vehicle: T): Unit = ???
  def impound[S <: T](vehilces: IList[S]): Unit = ???
  def check[U <: T](cond: String): IList[U] = ???

