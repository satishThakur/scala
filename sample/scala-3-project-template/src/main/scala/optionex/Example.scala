package optionex

case class Person(id: Int, name: String)

case class Dept(name: String)

case class Location(lat: String, lng: String)

object Location:
  def defLoc: Location = Location("0","0")

class DB:
  def getPerson(id: Int): Option[Person] = ???
  def getDept(p : Person): Option[Dept] = ???
  def deptLocation(d: Dept): Option[Location] = ???



object Example extends App:
  def getLocation(db: DB, personId: Int): Location =
    val deptLocation = for{
      p <- db.getPerson(personId)
      d <- db.getDept(p)
      l <- db.deptLocation(d)
    } yield l

    deptLocation.getOrElse(Location.defLoc)

