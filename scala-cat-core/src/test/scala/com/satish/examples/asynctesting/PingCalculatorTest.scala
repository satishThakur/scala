package com.satish.examples.asynctesting
import cats.Id
class PingCalculatorTest extends munit.FunSuite {

  test("pingservice adds the pings"){
    class TestClient(data : Map[String, Int]) extends UptimeClient[Id]:
      override def uptime(host: String): Id[Int] = data.getOrElse(host, 0)

    val client = new TestClient(Map("host1" -> 10, "host2" -> 12, "host3" -> 15))

    val service = new UptimeService[Id](client)

    val pings : List[Int] = service.hostsUptime(List("host1", "host3"))

    assertEquals(pings, List(10, 15))
  }


  test("hello") {
    val obtained = 43
    val expected = 43
    assertEquals(obtained, expected)
  }
}

