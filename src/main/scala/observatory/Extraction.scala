package observatory

import java.time.LocalDate
import scala.io.Source
/**
  * 1st milestone: data extraction
  */
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.spark.SparkContext
import org.apache.hadoop.fs.Stat

case class StationKey(stn: String, wban: String)

object Extraction extends ExtractionInterface:
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()
  val sc = spark.sparkContext
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */

  def getFromFile(file: String): RDD[Array[String]] = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream(file), "utf-8")
    .getLines()
    .toList
    sc.parallelize(lines).map(_.split(','))
  }

  def parseStations(stations: RDD[Array[String]]): RDD[(StationKey, Location)] = {
    stations.flatMap{
      case Array(stn, wban, lat, lon) => Some((StationKey(stn, wban), Location(lat.toDouble, lon.toDouble)))
      case _ => None 
    }
  }
  def parseTemperatures(temperatures: RDD[Array[String]], year: Year): RDD[(StationKey, (LocalDate, Temperature))] = {
    temperatures.flatMap{
      case Array(stn, wban, month, day, temp) => Some((StationKey(stn, wban), (LocalDate.of(year, month.toInt, day.toInt), (temp.toDouble - 32) / 1.8)))
      case _ => None 
    }
  }
  
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    locateTemperaturesSpark(year, stationsFile, temperaturesFile).collect.toSeq
  }

  def locateTemperaturesSpark(year: Year, stationsFile: String, temperaturesFile: String): RDD[(LocalDate, Location, Temperature)] = {
    val stations: RDD[(StationKey, Location)] = parseStations(getFromFile(stationsFile))
    val temperatures: RDD[(StationKey, (LocalDate, Temperature))] = parseTemperatures(getFromFile(temperaturesFile), year)
    val merged: RDD[(Location, (LocalDate, Temperature))] = stations.join(temperatures).values
    merged.map{case (loc, (date, temp)) => (date, loc, temp)}
  }
  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    sparkAverageRecords(sc.parallelize(records.toSeq)).collect().toSeq
  }

  def sparkAverageRecords(records: RDD[(LocalDate, Location, Temperature)]): RDD[(Location, Temperature)] = {
    records.groupBy(_._2)
           .map{case (loc, iter) => (loc, iter.map(_._3))}
           .map{case (loc, iter) => (loc, iter.sum / iter.size)}
  }
  

