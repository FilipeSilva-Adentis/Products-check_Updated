import java.text.SimpleDateFormat
import scala.util.Try


class Product(private val name: String,
              private val category: String,
              private val weight: Float,
              private val price: Float,
              private val creationDate: String) {

  /*  val result: Try[Unit] = for {
      df : SimpleDateFormat <- Try(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"))
      try(df.setLenient(false))
    } yield {

    }*/


  //
  try {
    val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    df.setLenient(false)
    df.parse(creationDate)
  } catch {
    case _: Exception => throw new Exception("\n\nInvalid Date '" + creationDate + "' for product '" + name)
  }

  if (name.isEmpty || name.isBlank || category.isBlank || category.isBlank || weight <= 0 || price <= 0) {
    throw new ArithmeticException("\nWrong  fields input for product '" + name + "'")
  }

  def getPrice(): Float = {
    price
  }

  def getDate(): String = {
    creationDate
  }

  def getName(): String = {
    name
  }

  override def toString: String = "\nProduct:" + "\nName: " + name + "\nCategory:" + category + "\nWeight: " + weight + "\nPrice: " + price + "\nCreation Date: " + creationDate;

  def equals(obj: Product): Boolean = {
    if (this.name.equals(obj.name) && this.category.equals(obj.category) && this.weight.equals(obj.weight) && this.creationDate.equals(obj.creationDate) && this.price.equals(obj.price)) {
      return true
    }
    false
  }
}
