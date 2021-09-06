import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class AuctionTest extends org.scalatest.funsuite.AnyFunSuite {

  object mockupGenerator {
    def generateTransactions(count: Int): Vector[Transaction] = {
      Vector.fill(count)(randomTransaction())
    }

    protected def randomTransaction(): Transaction = {
      Transaction(
        direction = TransactionDirection.apply(Random.between(0, 2)),
        count = Random.between(5, 10),
        price = BigDecimal(Random.between(5d, 10d)).setScale(0, RoundingMode.HALF_UP)
      )
    }
  }


  test("test") {

//    val res = Vector(
//      Transaction(TransactionDirection.Sell, 9, 5),
//      Transaction(TransactionDirection.Buy, 9, 8),
//      Transaction(TransactionDirection.Buy, 7, 5),
//      Transaction(TransactionDirection.Buy, 5, 7),
//      Transaction(TransactionDirection.Sell, 7, 8)
//    )

//    val res = Vector(
//      Transaction(TransactionDirection.Buy, 100, 15.4),
//      Transaction(TransactionDirection.Buy, 100, 15.3),
//      Transaction(TransactionDirection.Sell, 150, 15.3),
//    )
    println(Auction.getResult(mockupGenerator.generateTransactions(1000000)))
  }

}
