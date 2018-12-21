package arx.quintessence.game.entities

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/16/18
  * Time: 8:17 AM
  */

import arx.Prelude._
import arx.core.macros.GenerateCompanion
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData

@GenerateCompanion
class ShipTile extends TAuxData {
	var connections : List[Connection] = Nil
	var dimensions : Vec2i = Vec2i(4,4)
	var position : Vec2i = Vec2i(0,0)
}

sealed abstract class Edge
object Edge {
	case object Left extends Edge
	case object Right extends Edge
	case object Top extends Edge
	case object Bottom extends Edge
}

case class TileEdge(edge : Edge, slot : Int)

case class Connection(input : TileEdge, output : TileEdge)


class ShipComponent extends TAuxData {

}