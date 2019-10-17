package mud
import akka.actor.ActorRef

class Player {}
object Player {
    case class PrintMessage(msg: String)
    case class TakeItem(oitem: Item)
    case class  TakeExit(oroom: Option[ActorRef])
}