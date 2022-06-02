import java.util.Date

class Order(private val customerName: String,
            private val contact: Int,
            private val shippingAddress: String,
            private val date: String,
            private val items: List[Item]) {

  //Items/Order dates validation
  val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  format.format(new java.util.Date())
  val dateOrder = format.parse(date)

  items.foreach(item => {
    if (dateOrder.compareTo(item.getDate()) < 0) {
      throw new ArithmeticException("\n\nItem creation date must be before order date\n\n" + "Error with order: " + customerName + "\tDate: " + date + item.getProduct() + "\n")
    }
  })


  val total: Float = items.map(_.getCost()).sum

  def getDate(): Date = {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    format.format(new java.util.Date())

    format.parse(date)
  }

  def getItemsList(): List[Item] = {
    items
  }

  override def toString: String = "\nOrder: \n" + "Name: " + customerName + ";\nContact: " + contact + ";\nAddress: " + shippingAddress + ";\nTotal: " + total + ";\nDate: " + date + "\n" + items + "\n---"
}
