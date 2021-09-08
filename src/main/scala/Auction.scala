

object Auction {

  /** Calculates the maximum number of shares sold. If there are more than one such, it returns their arithmetic mean.
   *
   * @param requests buy and sell requests
   * @return prices (or arithmetic mean of them) with maximum of iterations or "0 n/a" if there is no result
   */
  def calculate(requests: Vector[Request]): String = {
    val (buy, sell) =
      groupRequests(requests)
        .partition { t =>
          t.direction.id == RequestDirection.Buy.id
        }

    val result = sell.flatMap { sellRequest =>
      buy.filter(_.price >= sellRequest.price).map(_.count).sum match {
        case available if available > 0 =>
          Some(Result(
            count = if (available > sellRequest.count) sellRequest.count else available,
            price = sellRequest.price
          ))
        case _ => None
      }

    }

    result match {
      case res if res.nonEmpty =>

        val maxCounts = result.groupBy(_.count).maxBy(_._1)._2
        Result(
          count = maxCounts.head.count,
          price = maxCounts.map(_.price).sum / maxCounts.size
        ).toString

      case _ => "0 n/a"
    }

  }

  /** Group given requests for simplify calculations.
   *
   * @param requests buy and sell requests
   * @return buy and sell requests groped by price and direction
   */
  protected def groupRequests(requests: Iterable[Request]): Iterable[Request] = {
    requests
      .groupBy(elem => (elem.direction, elem.price))
      .map {
        item =>
          Request(
            direction = item._1._1,
            count = item._2.map(_.count).sum,
            price = item._1._2
          )
      }
  }

  /** Split given values, convert them into Requests, and calculate the maximum number of shared sold.
   *
   * @param string values to convert. Must match the specified format (direction: one letter B or S (Buy or Sell),
   *               count: natural number, price: floating point number, rubles) and must be
   *               separated by line breaks
   * @return buy and sell requests groped by price and direction
   */
  def stringToRequest(string: String): Option[Request] = {
    val pattern = "([BS]) [0-9]+ [0-9]*\\.[0-9]+".r
    string match {
      case value if pattern.matches(value.trim) =>
        val values = value.split(" ")
        Some(Request(
          direction = RequestDirection.withName(values(0)),
          count = values(1).toIntOption.getOrElse(0),
          price = BigDecimal.valueOf(values(2).toDoubleOption.getOrElse(0d))
        ))
      case _ => None
    }
  }

  /** Converts the given String into Request object.
   * Checks the value for accordance expected format.
   *
   * @param string value to convert. Must match the specified format (direction: one letter B or S (Buy or Sell),
   *               count: natural number, price: floating point number, rubles)
   * @return prices (or arithmetic mean of them) with maximum of iterations or "0 n/a" if there is no result
   */
  def processMultilineString(string: String): String = {
    val res = string
      .split("\n")
      .flatMap(stringToRequest)
      .toVector

    Auction.calculate(res)
  }

  /** Result of calculation the maximum number of shares sold.
   *
   *  @param count number of iterations with specified price
   *  @param price share's price
   */
  case class Result(count: Int, price: BigDecimal) {
    override def toString: String = s"$count $price"
  }

}

/** Request to buy or sell the share.
 *
 *  @param direction type of financial transaction
 *  @param count amount of shares for current transaction
 *  @param price share's price
 */
case class Request(direction: RequestDirection.Value, count: Int, price: BigDecimal) {

  override def toString: String = {
    s"${direction.toString} $count $price"
  }

}


object RequestDirection extends Enumeration {
  val Buy: RequestDirection.Value = Value("B")
  val Sell: RequestDirection.Value = Value("S")
}
