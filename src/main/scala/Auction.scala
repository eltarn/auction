import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Auction {

  def getResult(transactions: Vector[Transaction]): String = {
    val (buy, sell) = transactions.partition { t =>
      t.direction.id == TransactionDirection.Buy.id
    }

    var vector = Vector.empty[Transaction]
    val result = sell.map { s =>
      val toBuy = buy
        .filter(_.price >= s.price)
        .sortBy(_.price)
      val y = doSell(s, toBuy, Iterable.empty[Sold])
      vector = y.res
      y
    }

    val x = result.flatMap(_.sold)
    println(x)
    s"${x.map(_.count).sum} ${x.map(_.price).sum / x.size}"
  }

  case class Sold(count: Int, price: BigDecimal)
  case class Test(transaction: Transaction, vector: Vector[Transaction], sold: Int)
  case class Result(sold: Iterable[Sold], res: Vector[Transaction])

  @tailrec
  def doSell(sellTransaction: Transaction, vector: Vector[Transaction], sold: Iterable[Sold]): Result = {
    println(vector.size)
    val buyTransaction = vector.head
    val t = (sellTransaction, buyTransaction) match {
      case (s, b) if s.count >= b.count =>
        val soldCount = b.count
        Test(s.copy(count = s.count - soldCount), vector.tail, soldCount)
      case (s, b) if s.count < b.count =>
        val soldCount = s.count
        val nB = b.copy(count = b.count - soldCount)
        Test(s.copy(count = 0), vector.tail ++ Option(nB), soldCount)
    }
    println(t)
    if (t.transaction.count > 0 && t.vector.nonEmpty) {
      doSell(t.transaction, t.vector, sold ++ Iterable(Sold(t.sold, buyTransaction.price)))
    } else  {
      Result(sold ++ Iterable(Sold(t.sold, buyTransaction.price)), t.vector)
    }
  }


}

case class Transaction(direction: TransactionDirection.Value, count: Integer, price: BigDecimal) {

  override def toString: String = {
    s"${direction.toString} $count $price"
  }

}


object TransactionDirection extends Enumeration {
  val Buy: TransactionDirection.Value = Value("B")
  val Sell: TransactionDirection.Value = Value("S")
}
