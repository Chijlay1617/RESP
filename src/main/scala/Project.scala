import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.{Source, StdIn}


trait RenewableEnergySource {
  protected var energy: Double
  def generateEnergy(): Double
  def setEnergy(newEnergy: Double): Unit
  def sourceName: String
  def checkForIssues(): Option[String]
}

class SolarPanel extends RenewableEnergySource {
  protected var energy = 100.0
  def sourceName: String = "Solar Energy"
  def generateEnergy(): Double = {
    energy
  }
  def checkForIssues(): Option[String] = {
    if (generateEnergy() < 50) Some("Low solar energy output detected.") else None
  }

  def setEnergy(newEnergy: Double): Unit = {
    energy = newEnergy
  }
}

class WindTurbine extends RenewableEnergySource {
  protected var energy = 200.0
  def sourceName: String = "Wind Energy"
  def generateEnergy(): Double = {
    energy
  }
  def checkForIssues(): Option[String] = {
    if (generateEnergy() < 100) Some("Low wind energy output detected.") else None
  }

  def setEnergy(newEnergy: Double): Unit = {
    energy = newEnergy
  }
}

class HydroPower extends RenewableEnergySource {
  protected var energy = 300.0
  def sourceName: String = "Hydro Energy"
  def generateEnergy(): Double = {
    energy
  }
  def checkForIssues(): Option[String] = {
    if (generateEnergy() < 200) Some("Low hydro energy output detected.") else None
  }

  def setEnergy(newEnergy: Double): Unit = {
    energy = newEnergy
  }
}

case class EnergyData(timestamp: LocalDateTime, sourceName: String, energy: Double)


class PowerPlant(sources: List[RenewableEnergySource]) {
  def getSources: List[RenewableEnergySource] = sources
  private val dataFile = "energy_data.txt"

  //write data to a file
  def updateData(): Unit = {
    val timestamp = LocalDateTime.now()
    val customDateTimeFormat = "yyyy-MM-dd HH:mm:ss.SSS"
    val formatter = DateTimeFormatter.ofPattern(customDateTimeFormat)

    val data = sources.map(s => EnergyData(timestamp, s.sourceName, s.generateEnergy()))
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(dataFile, true)))
    data.foreach(d => writer.write(s"${d.timestamp.format(formatter)}, ${d.sourceName}: ${d.energy} kW\n"))
    writer.close()
  }

  // read data from a file
  def readData(): List[EnergyData] = {
    val customDateTimeFormat = "yyyy-MM-dd HH:mm:ss.SSS"
    val formatter = DateTimeFormatter.ofPattern(customDateTimeFormat)

    Source.fromFile(dataFile).getLines().map { line =>
      val Array(timestampPart, sourceNameWithEnergy) = line.split(", ", 2)
      val Array(sourceName, energy) = sourceNameWithEnergy.split(": ")
      val parsedTimestamp = LocalDateTime.parse(timestampPart, formatter)
      EnergyData(parsedTimestamp, sourceName, energy.split(" ")(0).toDouble)
    }.toList
  }






  def filterData(from: LocalDateTime, to: LocalDateTime): List[EnergyData] = {
    readData().filter(d => !d.timestamp.isBefore(from) && !d.timestamp.isAfter(to))
  }
}


object Main {
  def mean(data: List[Double]): Double = data.sum / data.length

  // Calculate median
  def median(data: List[Double]): Double = {
    val sorted = data.sorted
    val n = sorted.length
    if (n % 2 == 0) (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0 else sorted(n / 2)
  }

  // Calculate mode
  def mode(data: List[Double]): Double = {
    data.groupBy(identity).mapValues(_.length).maxBy(_._2)._1
  }

  // Calculate range
  def range(data: List[Double]): Double = data.max - data.min

  // Calculate midrange
  def midrange(data: List[Double]): Double = (data.max + data.min) / 2.0
  def roundToTwoDecimals(value: Double): Double = {
    BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  // Main function
  def main(args: Array[String]): Unit = {
    val powerPlant = new PowerPlant(List(new SolarPanel, new WindTurbine, new HydroPower))

    var continueMenu = true
    //Handle error
    def parseIntOption(input: String): Option[Int] =
      try {
        Some(input.toInt)
      } catch {
        case _: NumberFormatException => None
      }
    def parseDoubleOption(input: String): Option[Double] =
      try {
        Some(input.toDouble)
      } catch {
        case _: NumberFormatException => None
      }
    while (continueMenu) {
      println("\n--- Menu ---")
      println("1. Monitor renewable energy sources")
      println("2. Control renewable energy sources")
      println("3. Collect and store energy data")
      println("4. View energy generation and storage capacity")
      println("5. Analyze energy data")
      println("6. Check for issues with renewable energy sources")
      println("7. Exit")

      print("Please choose an option (1-7): ")
      val input = StdIn.readLine()

      parseIntOption(input) match {
        case Some(choice) if choice >= 1 && choice <= 7 =>
          choice match {
            //Monitor data
            case 1 =>
              println("\nRenewable Energy Sources:")
              val sources = powerPlant.getSources
              sources.zipWithIndex.foreach { case (source, index) =>
                println(s"${index + 1}. ${source.getClass.getSimpleName}: ${source.generateEnergy()} kW")
              }
            //Control data
            case 2=>
              println("\nUpdate energy values for renewable sources:")

              var index = 0
              var continueInput = true
              while (index < powerPlant.getSources.length && continueInput) {
                val source = powerPlant.getSources(index)
                print(s"Enter new energy value for ${source.getClass.getSimpleName} (${index + 1}): ")
                val newEnergy = StdIn.readLine()
                parseDoubleOption(newEnergy) match {
                  case Some(value) =>
                    source.setEnergy(value)
                    index += 1
                  case _ =>
                    println("Invalid input, please try again.")
                    continueInput = false
                }
              }
            //Collect data into a file
            case 3 =>
              powerPlant.updateData()
              println("Energy data collected and stored.")
            //read data from file
            case 4 =>
              println("Energy Generation:")
              val energyData = powerPlant.readData()
              energyData.foreach(data => println(s"Timestamp: ${data.timestamp}, ${data.sourceName}: ${data.energy} kW"))
              println("Storage Capacity:")
              val list: List[Double] = List()
              val Solar_Energy: List[Double] = energyData.foldLeft(list)((list, data) => {
                if (data.sourceName.matches("Solar Energy")) list.appended(data.energy) else list
              })
              val Wind_Energy: List[Double] = energyData.foldLeft(list)((list, data) => {
                if (data.sourceName.matches("Wind Energy")) list.appended(data.energy) else list
              })
              val Hydro_Energy: List[Double] = energyData.foldLeft(list)((list, data) => {
                if (data.sourceName.matches("Hydro Energy")) list.appended(data.energy) else list
              })
              println(s"Storage Capacity of Solar Energy: ${Solar_Energy.sum}")
              println(s"Storage Capacity of Wind Energy: ${Wind_Energy.sum}")
              println(s"Storage Capacity of Hydro Energy: ${Hydro_Energy.sum}")

            //Data analysis
            case 5 =>
              println("\nData Analysis:")
              println("Choose the time range for data analysis:")
              println("1. Hourly")
              println("2. Daily")
              println("3. Weekly")
              println("4. Monthly")
              print("Please choose an option (1-4): ")
              val timeChoice = StdIn.readLine()

              val fromOption: Option[LocalDateTime] = parseIntOption(timeChoice) match {
                case Some(value) =>
                  value match {
                    case 1 => Some(LocalDateTime.now().minusHours(1))
                    case 2 => Some(LocalDateTime.now().minusDays(1))
                    case 3 => Some(LocalDateTime.now().minusWeeks(1))
                    case 4 => Some(LocalDateTime.now().minusMonths(1))
                    case _ =>
                      println("Invalid choice. Please choose a number between 1 and 4.")
                      None
                  }
                case None =>
                  println("Invalid input. Please enter a number between 1 and 4.")
                  None
              }

              fromOption match {
                case Some(fromValue) =>
                  val to = LocalDateTime.now()
                  val filteredData = powerPlant.filterData(fromValue, to)

                  if (filteredData.nonEmpty) {
                    val energyValues = filteredData.map(_.energy)
                    println(s"Mean: ${roundToTwoDecimals(mean(energyValues))} kW")
                    println(s"Median: ${roundToTwoDecimals(median(energyValues))} kW")
                    println(s"Mode: ${roundToTwoDecimals(mode(energyValues))} kW")
                    println(s"Range: ${roundToTwoDecimals(range(energyValues))} kW")
                    println(s"Midrange: ${roundToTwoDecimals(midrange(energyValues))} kW")
                  } else {
                    println("No data available for the selected time range.")
                  }
                case None =>
                // Do nothing, as there was an error with the user input.
              }



            //Check issues
            case 6 =>
              // Implement the logic for checking issues with renewable energy sources
              println("\nChecking for issues with renewable energy sources:")
              powerPlant.getSources.zipWithIndex.foreach { case (source, index) =>
                source.checkForIssues() match {
                  case Some(issue) =>
                    println(s"${index + 1}. ${source.getClass.getSimpleName}: ALERT - $issue")
                  case None =>
                    println(s"${index + 1}. ${source.getClass.getSimpleName}: No issues detected.")
                }
              }
            case 7 =>
              println("Exiting the program...")
              continueMenu = false
            case _ =>
              println("Invalid option. Please choose a number between 1 and 6.")
          }
        case _=>
          println("Invalid option, please try again.")
      }
    }
  }
}

