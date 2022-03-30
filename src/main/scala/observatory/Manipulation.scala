package observatory

import scala.collection.parallel.CollectionConverters.given

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface:
  
  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val grid: Map[GridLocation, Temperature] = 
      (for {
        lat <- -89 to 90
        long <- -180 to 179
      } yield (GridLocation(lat, long), Visualization.predictTemperature(temperatures, Location(lat, long)))).toMap
    (gridLoc: GridLocation) => grid(gridLoc)
    }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def avg(nums: scala.collection.parallel.ParIterable[Double]): Double = nums.sum / nums.size
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val grids = for(temperatures <- temperaturess.par) yield makeGrid(temperatures)
    val averages = (for {
      lat <- -89 to 90
      long <- -180 to 179
      gridLoc = GridLocation(lat, long)
    } yield (gridLoc, avg(grids.map(grid => grid(gridLoc))))).toMap
    (gridLoc: GridLocation) => averages(gridLoc)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid: Map[GridLocation, Temperature] = 
      (for {
        lat <- -89 to 90
        long <- -180 to 179
      } yield (GridLocation(lat, long), Visualization.predictTemperature(temperatures, Location(lat, long)))).toMap
    val deviations: Map[GridLocation, Temperature] = grid.map{case (gridLoc, temp) =>  (gridLoc, temp - normals(gridLoc))}
    (gridLoc: GridLocation) => deviations(gridLoc)
  }



