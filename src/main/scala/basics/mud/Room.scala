package mud

import akka.actor.ActorRef
class Room{
    import Room._
    def receive = {
        case LinkExits(rooms)=>
            exits = exitKeys.map(key => rooms.get(key))
        case GetItem(itemName) => 
           sender ! Player.TakeItem(getItem(itemName))
        case GetDescription => 
            sender ! Player.PrintMessage(description())
        
        case m => println("unhandled message in Room: "+m)
       }
}
object Room{
    case class GetItem(itemName:String)
    case class DropItem(item:Item)
    
    case class LinkExits(rooms:Map[Strings,ActorRef])
}

