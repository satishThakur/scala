package com.satish.process

import java.io.File

import com.github.tototoshi.csv.CSVReader

object Parser {

  def main(args: Array[String]): Unit = {

    //read CSV file
   val reader = CSVReader.open(new File("/tmp/data-18.csv"))

    val dataWithHeader: List[List[String]] = reader.all()

    val data = dataWithHeader.drop(1)


    val dataByOderable: Map[String,List[List[String]]] = data.groupBy(row => row(8))

    val dist: Map[String,Double] = dataByOderable map {case (k,v) => (k,(v.size * 1.0)/data.size)}

    val sortedDist = dist.toList.sortBy(k => k._2).reverse

    println(sortedDist)

    val citiIds: List[(String,List[Int])] = dataByOderable.toList.map(x => (x._1, x._2.take(2).map(eventCityId)))

    val cityCount: List[(String,Map[Int, List[Int]])] = citiIds.map(x => (x._1, x._2.groupBy(i => i)))

    val counts: List[(String,Map[Int, Int])]= cityCount.map(x => (x._1, x._2 map {case (k,v:List[Int]) => (k,v.size)}))

    println(counts)
    //println(latlongToCityId("22.542506","88.357603"))


  }


  def eventCityId(event: List[String]): Int ={
    latlongToCityId(event(4),event(6))
  }

  def latlongToCityId(lat: String, long: String): Int = {
    println("reverse lookup for ", lat, long)
    Thread.sleep(100)
    val r = requests.post("http://localhost:10000/api/presentation/context/geo",
      headers = Map("Content-Type" -> "application/json"),
      data = ujson.Obj("latitude" -> lat, "longitude" -> long))
    val parsed = ujson.read(r.text())
    parsed.obj("data").obj("cityId").num.toInt
  }

}
