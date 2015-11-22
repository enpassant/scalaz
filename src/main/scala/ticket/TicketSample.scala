package ticket

import scalaz._, Scalaz._, Free._

import language.higherKinds
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Try

object TicketSample
{
  case class Tickets(count: Int) extends AnyVal
  case class UserTicketsRequest(ticketCount: Int)

  sealed trait External[+A]
  case class InvokeTicketingService(count: Int)
    extends External[Tickets]

  def purchaseTickets(input: UserTicketsRequest): Free[External, Option[Tickets]] = {
    if (input.ticketCount > 0) {
      liftF[External, Tickets](
        InvokeTicketingService(input.ticketCount)).map(Some(_))
    } else {
      Free.pure(None)
    }
  }

  def bonusTickets(purchased: Option[Tickets]): Free[External, Option[Tickets]] = {
    if (purchased.exists(_.count > 10)) {
      liftF[External, Tickets](
        InvokeTicketingService(1)).map(Some(_))
    } else {
      pure(None)
    }
  }

  def formatResponse(purchased: Option[Tickets], bonus: Option[Tickets]): String =
    s"Purchased tickets: $purchased, bonus: $bonus"

  val externalToServiceInvoker = new (External ~> Future) {
    override def apply[A](e: External[A]): Future[A] = e match {
      case InvokeTicketingService(count) if count > 0 =>
        serviceInvoker.run(count, s"/tkts?count=$count")
    }
  }

  val externalToTest = new (External ~> Option) {
    override def apply[A](e: External[A]): Option[A] = e match {
      case InvokeTicketingService(count) if count > 0 =>
        Some(Tickets(count))
    }
  }

  object serviceInvoker {
    def run(count: Int, path: String) = {
      Future {
        Thread.sleep(2000)
        Tickets(count)
      }
    }
  }

  val input = UserTicketsRequest(11)

  val logic: Free[External, String] = for {
    purchased <- purchaseTickets(input)
    bonus <- bonusTickets(purchased)
  } yield formatResponse(purchased, bonus)

  def main(args: Array[String]): Unit = {
    val result = logic.foldMap(externalToServiceInvoker)
    val resultTest = logic.foldMap(externalToTest)
    println(s"Test: $resultTest")
    Await.result(result, 10.seconds)
    println("Production: " + result.value.getOrElse("done"))
  }
}

