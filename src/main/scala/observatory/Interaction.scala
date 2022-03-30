package observatory
import scala.math.{sinh, atan, pow}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given
import Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = pow(2,tile.zoom)
    val lon_deg = tile.x / n * 360.0 - 180.0
    val lat_rad = atan(sinh(Math.PI * (1 - 2 * tile.y / n)))
    val lat_deg = lat_rad * 180.0 / Math.PI
    Location(lat_deg, lon_deg)
  }
    

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage = {
    val width = 256
    val height = 256
    val offsetX = tile.x * 256
    val offsetY = tile.y * 256
    val pixels =
      for {
        y <- 0 until height
        x <- 0 until width
        t = Tile(x + offsetX, y+offsetY, tile.zoom + 8)
        location = tileLocation(t)
        color = interpolateColor(colors, predictTemperature(temperatures, location))
      } yield Pixel(x, y, color.red, color.green, color.blue, 127)
    ImmutableImage.wrapPixels(width, height, pixels.toArray, ImageMetadata.empty)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 until 4
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2,zoom).toInt
      tile = Tile(x,y,zoom)
    } generateImage(year, tile, data)
  }
  

