import scala.collection.mutable.ArrayBuffer

class showOrdersInterval(private val dateIni: String, private val dateEnd: String) {

  def getOrders(listOrders: Seq[Order]): Seq[Order] = {

    val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    format.format(new java.util.Date())
    val dateI = format.parse(dateIni)
    val dateE = format.parse(dateEnd)

    val listOrdersFiltered = listOrders.filter(order => {
      order.getDate().compareTo(dateI) >= 0 && order.getDate().compareTo(dateE) <= 0
    })

    listOrdersFiltered
  }
}
