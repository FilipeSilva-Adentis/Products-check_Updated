import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.util
import java.util.regex.Pattern

class showIntervals(private val dateIniArg: String, private val dateEndArg: String) {
  val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  format.format(new java.util.Date())


  def getIntervals(listOrders: Seq[Order]): String = {


    val dateIni = dateIniArg.split(" ")(0)

    //
    sealed trait MonthsSinceNow
    case object LessThanOneMonth extends MonthsSinceNow
    case object OneToThreeMonths extends MonthsSinceNow
    case object FourToSixMonths extends MonthsSinceNow
    case object SevenToTwelveMonths extends MonthsSinceNow
    case object MoreThanTwelveMonths extends MonthsSinceNow

    object MonthsSinceNow {
      def totalMonths(dateInicial: String, order: Order): Int = {
        val itemDate = dateFormat.format(order.getDate())
        (ChronoUnit.MONTHS.between(LocalDate.parse(dateIni).withDayOfMonth(1), LocalDate.parse(itemDate).withDayOfMonth(1)) + 1).toInt
      }

      def apply(dataInicio: String, order: Order): MonthsSinceNow = {
        val delta = totalMonths(dataInicio, order)
        delta match {
          case delta if delta < 1 => LessThanOneMonth
          case delta if delta == 1 || delta <= 3 => OneToThreeMonths
          case delta if delta == 4 || delta <= 6 => FourToSixMonths
          case delta if delta == 7 || delta <= 12 => SevenToTwelveMonths
          case delta if delta > 12 => MoreThanTwelveMonths
        }
      }
    }

    val orders: Seq[Order] = listOrders

    val a: Seq[MonthsSinceNow] = orders.map(order =>
      MonthsSinceNow.apply(dateIni, order)
    )

    val b: Map[MonthsSinceNow, Seq[MonthsSinceNow]] = a.groupBy(identity)

    val c: Map[MonthsSinceNow, Int] = b.map { case (monthsSinceNow, orders) => (monthsSinceNow, orders.size) }
    //
    c.toString()
    /*    val str: String = c.foreach(obj => {
          obj._1 match {
            case LessThanOneMonth => "<1" + obj._2
            case OneToThreeMonths => "1-3" + obj._2
            case FourToSixMonths => "4-6" + obj._2
            case SevenToTwelveMonths => "7-12" + obj._2
            case MoreThanTwelveMonths => ">12" + obj._2
          }
        }
        ).toString
        str*/

    /*var m1_3, m4_6, m7_12, m12: Int = 0
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
    })*/

  }


  def getOrdersBySpecInterval(listOrders: Seq[Order], intervalList: String): String = {

    val intervals = new util.ArrayList[Integer]

    val p = Pattern.compile("\\d+")
    val m = p.matcher(intervalList)

    while ( {
      m.find
    }) intervals.add(m.group.toInt)

    val dateIni = dateIniArg.split(" ")(0)

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