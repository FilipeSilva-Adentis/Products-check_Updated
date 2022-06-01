import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.util
import java.util.Date
import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

class showIntervals(private var dateIni: String, private var dateEnd: String) {
  var dateI = new Date()
  var dateE = new Date()
  val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  format.format(new java.util.Date())
  dateI = format.parse(dateIni)
  dateE = format.parse(dateEnd)
  if (dateI.compareTo(dateE) > 0) {
    val dateAux = dateIni
    dateIni = dateEnd
    dateEnd = dateAux
  }

  def getIntervals(listOrders: ArrayBuffer[Order]): String = {

    dateIni = dateIni.split(" ")(0)
    dateEnd = dateEnd.split(" ")(0)

    var m1_3, m4_6, m7_12, m12: Int = 0
    listOrders.foreach(order => {
      val itemDate = dateFormat.format(order.getDate())

      val space = ChronoUnit.MONTHS.between(LocalDate.parse(dateIni).withDayOfMonth(1), LocalDate.parse(itemDate).withDayOfMonth(1)) + 1
      if (space <= 3) {
        m1_3 += 1
      } else if (space > 3 && space <= 6) {
        m4_6 += 1
      } else if (space > 6 && space <= 12) {
        m7_12 += 1
      } else {
        m12 += 1
      }
    })

    "1-3 months: " + m1_3 + " orders\n4-6 months: " + m4_6 + " orders\n7-12 months: " + m7_12 + " orders\n>12 months: " + m12 + " orders"
  }


  def getOrdersBySpecInterval(listOrders: ArrayBuffer[Order], intervalList: String): String = {

    val intervals = new util.ArrayList[Integer]

    val p = Pattern.compile("\\d+")
    val m = p.matcher(intervalList)

    while ( {
      m.find
    }) intervals.add(m.group.toInt)

    dateIni = dateIni.split(" ")(0)
    dateEnd = dateEnd.split(" ")(0)

    var str: String = new String()

    for (w <- 0 until intervals.size()) {
      if (w % 2 == 0) {
        var counter: Int = 0
        if (w + 1 != intervals.size) {
          listOrders.foreach(order => {
            val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
            val itemDate = dateFormat.format(order.getDate())

            val space = ChronoUnit.MONTHS.between(LocalDate.parse(dateIni).withDayOfMonth(1), LocalDate.parse(itemDate).withDayOfMonth(1)) + 1
            if (space >= intervals.get(w) && space <= intervals.get(w + 1)) {
              counter += 1
            }
          })

          str += intervals.get(w)
          str += '-'
          str += intervals.get(w + 1)
          str += ": "
          str += counter
          str += "\n"
        }
      }
    }
    str
  }
}