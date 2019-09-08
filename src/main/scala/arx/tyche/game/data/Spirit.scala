package arx.tyche.game.data

import arx.core.vec.{ReadVec4f, Vec4f}
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.{GameEntity, TGameEntity}
import arx.engine.world.World
import arx.tyche.application.TycheApplication.world
import arx.tyche.core.GlobeCoord
import arx.tyche.game.data.TerrainType.Ocean

class Spirit extends TGameEntityAuxData {
	var position: GlobeCoord = GlobeCoord.fromEuler(0.0f, 0.0f, 1.0f)
	var destination: Option[GlobeCoord] = None
	var selectedActionType: Option[SpiritActionType] = None
	var intendedAction: Option[SpiritAction] = None
	var actionTypes: List[SpiritActionType] = Nil
	var color: ReadVec4f = Vec4f(0.3f, 0.4f, 0.9f, 1.0f)
	var speed: Float = 0.001f


	def canPerformAction(world: World, action: SpiritAction): Boolean = {
		position.angularDistanceTo(action.patch[Patch].center) < 0.01f
	}
}

object Spirit {
	protected def createSpirit(world: World)(spiritFunc: Spirit => Unit): TGameEntity = {
		val entity = new GameEntity()
		world.addEntity(entity)

		val spirit = entity[Spirit]
		spirit.position = world[Globe].patches.head.aux[Patch].center
		spiritFunc(spirit)
		entity
	}

	def oceanSpirit(world: World): TGameEntity = {
		createSpirit(world) { spirit =>
			spirit.actionTypes = List(
				TransformTerrain(Ocean, PatchShape.Circle(1)),
				CreateSource("animal", Map(TerrainType.Ocean -> SourceType.Mackerel))
			)
			spirit.selectedActionType = spirit.actionTypes.headOption
		}
	}
}