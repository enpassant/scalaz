case class EmailAddress(address: String)
case class Email(content: String)
case class Customer(name: String, emailAddress: EmailAddress, unsubscribed: Boolean)

sealed trait AppEffect
case class SendEmail(addr: EmailAddress, content: Email) extends AppEffect
case class Log(level: String, message: String) extends AppEffect

object SpecialOffersService {
  def prepareSpecialOffersEmail(cust: Customer): Option[SendEmail] = {
    if (!cust.unsubscribed) {
      val email = Email(s"Hi, ${cust.name}! Boy, have we got a deal for you!")
      Some(SendEmail(cust.emailAddress, email))
    }
    else None
  }
}

trait EmailSender {
  def send(addr: EmailAddress, email: Email): Unit
}

trait Logger {
  def log(level: String, message: String): Unit
}

class EffectInterpreter(logger: Logger, sender: EmailSender) {
  def apply(effect: AppEffect): Unit = effect match {
    case SendEmail(addr, content) => sender.send(addr, content)
    case Log(level, msg) => logger.log(level, msg)
  }
}

//-------- App wiring somewhere else -----------
class App(interpret: EffectInterpreter) {
  def sendSpecialOffersEmail(cust: Customer): Unit =
    SpecialOffersService.prepareSpecialOffersEmail(cust) foreach { interpret(_) }
}
