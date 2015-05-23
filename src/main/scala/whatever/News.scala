import scala.io.Source
import com.github.nscala_time.time.Imports._

package whatever {

  case class News(Time: String, Text: String, label: String)

  //case class News(Title: String, Time: String, Text: String,
    //Url:String, Location: String)

  object Preprocessor {

    import org.json4s._
    import org.json4s.jackson.JsonMethods._

    val timeParser = DateTimeFormat forPattern "YYYY-MM-DD HH:mm:ss"
    val timeStart = timeParser parseDateTime "2014-11-01 00:00:00"

    implicit val formats = DefaultFormats

    def parseJson(fileName: String): News =
      parse(Source fromFile fileName mkString).extract[News]

    def convertTime(endString: String): Int = {
      val timeEnd = timeParser parseDateTime endString
      (timeStart to timeEnd).duration.getStandardMinutes toInt
    }
      
  }

  //case class News {}

}
