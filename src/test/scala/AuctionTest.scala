import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class AuctionTest extends org.scalatest.funsuite.AnyFunSuite {

  test("Parsing a empty String doesn't return any value") {

    assert(Auction.stringToRequest("").isEmpty)

  }

  test("Parsing a incorrect String doesn't return any value") {

    assert(Auction.stringToRequest("B S 10.00").isEmpty)

  }

  test("Parsing a correct String value") {

    assert(Auction.stringToRequest("B 100 10.00").contains(Request(RequestDirection.Buy, 100, 10.00)))

  }

  test("Calculate a case without any solutions") {

    assert(
      Auction.processMultilineString(
      """B 100 10.00
        |S 150 10.10
        |""".stripMargin)
        .equals("0 n/a"))
  }

  test("Calculate a case with solution") {

    assert(
      Auction.processMultilineString(
      """B 100 15.40
        |B 100 15.30
        |S 150 15.30
        |""".stripMargin)
        .equals("150 15.3"))
  }

  test("Calculate a case with multiply solutions") {

    assert(
    Auction.processMultilineString(
      """
        |B 100 12.3
        |S 100 12.1
        |S 100 1.5
        |""".stripMargin
    )
    .equals("100 6.8"))
  }

  test("Work with huge amount of data") {
    assert(
      Auction.processResult(mockupGenerator.generateRequest(1000000))
        .nonEmpty
    )
  }






}

object mockupGenerator {

  /** Generates a collection of random requests.
   *
   * @param count amount of request to generate
   * @return collection with randomly generated requests
   */
  def generateRequest(count: Int): Vector[Request] = {
    Vector.fill(count)(randomRequest())
  }

  /** Generates a random request. Used only for tests.
   *
   * @return single buy or sell request with random amount and price
   */
  protected def randomRequest(): Request = {
    Request(
      direction = RequestDirection.apply(Random.between(0, 2)),
      count = Random.between(1, 1000),
      price = BigDecimal(Random.between(1d, 100d)).setScale(2, RoundingMode.HALF_UP)
    )
  }
}

