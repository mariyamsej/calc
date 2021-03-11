import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

case object Start
case class SetRequest(expr: String)
case class GetRequest(res: String)
case class GetResponse(res: Double)

class MainCalc extends Actor with ActorLogging {
        implicit val timeout = Timeout(5 seconds)

        val calcActor = context.actorOf(Props(new Calculator), "Calc")

        override def preStart() {
            self ! Start
        }

        def receive = {
            case Start =>
                calcActor ! SetRequest("2-9+1=")
                val respF = calcActor ? GetRequest("Result")
                respF pipeTo self

            case r: GetResponse =>
                log.warning(s"Response: ${r.res}")
        }
}
