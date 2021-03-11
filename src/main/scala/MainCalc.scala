import Calculator._

import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.collection._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case object Start
case class SetRequest(expr: String)
case class GetRequest(res: Double)

object MainCalc extends Actor with ActorLogging {
        implicit val timeout = Timeout(5 seconds)
        val mapActor = context.actorOf(Props(new Calculator), Calculator.name)

        override def preStart() {
            self ! Start
        }

        def receive = {
            case Start =>
                mapActor ! "dummy request"
                mapActor ! SetRequest("key", "value")
                val respF = mapActor ? GetRequest("key")
                respF pipeTo self

            case r: GetResponse =>
                log.warning(s"Response: $r")
                context.system.shutdown()
        }

}
