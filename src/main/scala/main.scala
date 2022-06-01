import scala.util.{Failure, Success, Try}

object main {
  def main(args: Array[String]): Unit = {

    val result: Try[Unit] = for {
      dateIni <- Try(args(0))
      dateEnd <- Try(args(1))
    } yield {
      val intervalList = Try(args(2)).toOption

      intervalList match {
        case None =>
          println(
            new showIntervals(dateIni, dateEnd).getIntervals(
              new showOrdersInterval(dateIni, dateEnd)
                .getOrders(new doOrders().makeOrders())
            )
          )
        case Some(intervalList) =>
          println(
            new showIntervals(dateIni, dateEnd)
              .getOrdersBySpecInterval(
                new doOrders().makeOrders(),
                intervalList
              )
          )
      }
    }

    result match {
      case Failure(exception) => println(s"Error: ${exception.getMessage}")
      case Success(value) => println(value)
    }
  }
}