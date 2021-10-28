package com.satish.process

import java.io.File
import CSVData.{Col, _}
import com.github.tototoshi.csv.CSVReader
import scala.util.Random

object CSVData{
  type Col = String
  type Row = List[Col]
  type Rows = List[Row]
}

class CSVData(val data: Rows){

  //Get distribution by a column - sorted by desc
  def colDistribution(col: Int): List[(Col,Double)]  =
    data.groupBy(_(col)).toList.map{ case (c,rows) => (c, (rows.size * 1.0)/data.size)}.sortBy(_._2).reverse

  //Group CSV data to smaller data grouped by every possible value of column - sorted by column Distribution
  def rowsGroupByCol(col: Int): List[(Col,CSVData)] = {
    val groupedData = data.groupBy(_ (col)).toList
    val dist: Map[Col, Double] = colDistribution(col).toMap
    groupedData.sortWith((t1, t2) => dist(t1._1) < dist(t2._1)).reverse.map(x => (x._1, new CSVData(x._2)))
  }

  //Draw a random sample of Size n
  def sample(n: Int): CSVData = {
    new CSVData(Random.shuffle(data).take(math.min(n,data.size)))
  }

  //Aggregate by given function - aggregation simply do count here - can be made more generic?
  def aggregateBy[U](f: Row => U) : List[(U,Int)] = {
    val d:List[(Row,U)] = data.map(r => (r, f(r)))
    val g : Map[U, Int] = d.groupBy(x => x._2).map{ case (u: U, rs:List[(Row,U)]) => (u,rs.size)}
    g.toList.sortBy(_._2).reverse
  }
}

import GeoLocationMapper._

object CSVProcessor {

  def main(args: Array[String]): Unit = {
    val reader = CSVReader.open(new File("/tmp/data-18.csv"))
    val sampleSize = 100
    val dataWithHeader: List[List[String]] = reader.all()

    val data: Rows = dataWithHeader.drop(1)

    val cd = new CSVData(data)

    prettyPrintDist(cd.colDistribution(8))

    val subDatas: List[(Col, CSVData)] = cd.rowsGroupByCol(8)

    val y: List[(Col, List[(Int, Int)])] = subDatas.map {
      case (os, csvDatum) => (os, csvDatum.sample(sampleSize).aggregateBy(row => latlongToCityId(row(4), row(6))))
    }
    prettyPrint(y)
  }

  def prettyPrint(osCount: List[(Col, List[(Int, Int)])]): Unit = {
    println(osCount.foldLeft("")((st, osData) => {
      val (col, cityCount) = osData
      val total = cityCount.foldLeft(0)((s, e) => s + e._2)
      val header = st + "\n" + "Oderable State [" + col + "]" + "Sample Size [" + total + "]\n"
      header + cityCount.map(d => "\tCityId[" + d._1 + "] = " + d._2 + "(" + ((d._2 * 100.0) / total).toInt + "%)").mkString("\n")
    }))
  }

  def prettyPrintDist(dist: List[(Col, Double)]): Unit = {
    println(dist.map { case (os, d) => "" + os + " - " + (d * 100).toInt + "%" }.mkString("\t"))
  }
}

object GeoLocationMapper{

  def latlongToCityId(lat: String, long: String): Int = {
    Thread.sleep(100)
    val r = requests.post("http://localhost:10000/api/presentation/context/geo",
      headers = Map("Content-Type" -> "application/json"),
      data = ujson.Obj("latitude" -> lat, "longitude" -> long))
    val parsed = ujson.read(r.text())
    parsed.obj("data").obj("cityId").num.toInt
  }
}
