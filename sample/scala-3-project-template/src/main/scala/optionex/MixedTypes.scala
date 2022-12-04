package optionex

class DB1:
  def getPerson(id: Int): Option[Person] = ???
  def getDept(p : Person): Either[String, Dept] = ???
  def deptLocation(d: Dept): Option[Location] = ???

object MixedTypes extends App:
  def getLocation(db: DB1, personId: Int): Location =
    (for{
      person <- db.getPerson(personId).toRight("error in getting person")
      dept <- db.getDept(person)
      loc <- db.deptLocation(dept).toRight("some error")

    }yield loc).toOption.getOrElse(Location.defLoc)
