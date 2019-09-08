
package arx.tyche.game.data

import arx.engine.entity.TGameEntity
import arx.tyche.core.GlobeCoord
import arx.Prelude._
import arx.engine.world.World

case class SpiritAction (actionType : SpiritActionType, patch : TGameEntity) {
	var progress = 0.seconds
}

class SpiritActionType {
	def range = 1
	def timeToAct = 5.seconds
}

trait PatchShape {
	def patchesIfAt(world : World, origin : TGameEntity) : List[TGameEntity]
}

object PatchShape {
	case class Circle(radius : Int) extends PatchShape {
		override def patchesIfAt(world: World, origin: TGameEntity): List[TGameEntity] = {
			origin :: origin[Patch].neighbors
		}
	}
}

case class TransformTerrain(targetTerrain : TerrainType, shape : PatchShape) extends SpiritActionType

case class CreateSource(description : String, sources : Map[TerrainType, SourceType]) extends SpiritActionType

case object DoNothing extends SpiritActionType
