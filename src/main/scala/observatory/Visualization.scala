package observatory
import scala.math.{asin, sin, cos, abs, pow, toRadians}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given
import scala.collection.parallel.CollectionConverters.given

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  
  def hav(x: Double) = pow(sin(x), 2)
  def distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {

    2*asin(pow(hav((lat1-lat2)/2) + (1 - hav((lat1-lat2)/2) - hav((lat1+lat2)/2))*hav((lon1-lon2)/2), 0.5))
  }
  def distance(loc1: Location, loc2: Location): Double = {
    val R: Double = 6371.0
    if(loc1 == loc2) 0
    else {
      val (lat1, lon1) = (loc1.lat, loc1.lon)
      val (lat2, lon2) = (loc2.lat, loc2.lon)
      if(lat1 == -lat2 && Math.abs(lon2-lon1) == 180) Math.PI * R
      else {
        distance(toRadians(lat1), toRadians(lon1), toRadians(lat2), toRadians(lon2)) * R
      }
    }
  }
  def weight(dist: Double): Double = 1/Math.pow(dist, 5) 

  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    val distTemp = temperatures.map{case (loc, temp) => 
      val dist = abs(distance(loc, location))
      (dist, temp)  
    }
    val closest = distTemp.minBy(_._1)
    if (closest._1 <=1) closest._2
    else {
      val (num, denom) = distTemp
                          .map{case (dist, temp) => 
                            val w = weight(dist)
                            (w*temp, w)}
                          .foldLeft((0.0,0.0))((p1,p2) => (p1._1 + p2._1, p1._2 + p2._2))
      num / denom
    }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    points.find(_._1 == value) match {
      case Some(temp, color) => color
      case None => 
        points.partition(_._1 < value) match {
          case (Nil, bigger) => bigger.minBy(_._1)._2
          case (smaller, Nil) => smaller.maxBy(_._1)._2
          case (smaller, bigger) =>
            val sm: (Temperature, Color) = smaller.maxBy(_._1)
            val bm: (Temperature, Color) = bigger.minBy(_._1)
            val t = (value - sm._1)/(bm._1 - sm._1)
            def interp(s: Color, b: Color): Color = {
              val nRed = (b.red * t + s.red * (1-t)).round.toInt
              val nGreen = (b.green * t + s.green * (1-t)).round.toInt
              val nBlue = (b.blue * t + s.blue * (1-t)).round.toInt
              Color(nRed, nGreen, nBlue)
            }
            interp(sm._2, bm._2)
        }
        
    }
  }
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  val colors: Iterable[(Temperature, Color)] = {
    List((60, Color(255, 255, 255)),
         (32, Color(255, 0, 0)),
         (12, Color(255, 255, 0)),
         (0, Color(0,0,255)),
         (-15, Color(0,0,255)),
         (-27, Color(255, 0, 255)),
         (-50, Color(33, 0, 107)),
         (-60, Color(0,0,0)))
  }
  
  def pixelToLocation(x: Int, y: Int): Location = {
    Location(90 - y, x-180)
  }
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): ImmutableImage = {
    val width = 360
    val height = 180
    val pixels =
      for {
        y <- 0 until height
        x <- 0 until width
        location = pixelToLocation(x, y)
        color = interpolateColor(colors, predictTemperature(temperatures, location))
      } yield Pixel(x, y, color.red, color.green, color.blue, 127)
    ImmutableImage.wrapPixels(width, height, pixels.toArray, ImageMetadata.empty)
  }

  