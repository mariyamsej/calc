import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.collection._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {
  val system = ActorSystem("system")
  val mainActor = system.actorOf(Props(new MainCalc), "MainCalc")
}
