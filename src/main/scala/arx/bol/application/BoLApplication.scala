package arx.bol.application

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.bol.control.components.DungeonCrawlControlComponent
import arx.bol.control.components.InventoryControlComponent
import arx.bol.game.components.DungeonCrawlComponent
import arx.bol.game.components.EntityTypes.Creature
import arx.bol.game.entities.CreatureArchetype
import arx.bol.game.entities.PlayerCharacter
import arx.bol.game.entities.Weapons
import arx.bol.game.entities.data.BagOfLegendData
import arx.bol.game.entities.data.PhysicalData
import arx.bol.game.graphics.data.BagOfLegendGraphicsData
import arx.bol.game.graphics.data.SlotHighlight
import arx.bol.game.world.data.DungeonData
import arx.bol.graphics.components.DungeonCrawlGraphicsComponent
import arx.bol.graphics.components.InventoryGraphicsComponent
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.core.vec.Vec4f
import arx.engine.EngineCore
import arx.engine.advanced.Engine
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.WindowingControlComponent
import arx.engine.control.components.windowing.widgets.DimensionExpression
import arx.engine.control.data.WindowingData
import arx.engine.entity.GameEntity
import arx.engine.graphics.components.WindowingGraphicsComponent
import arx.engine.graphics.data.PovData
import arx.graphics.pov.TopDownCamera

import scalaxy.loops._

object BoLApplication extends Engine {
	EngineCore.windowWidth = 800
	EngineCore.windowHeight = 1080

	override def setUpEngine(): Unit = {
		val DD = world[DungeonData]

		for (i <- DD.activeDungeon.heightmap.indices) {
			DD.activeDungeon.heightmap(i) = 2
		}

		val PD = graphicsEngine.graphicsWorld[PovData]

		val pc = PlayerCharacter.create(CreatureArchetype.Human)
		world.addEntities(pc)

		for (_ <- 0 until 100) {
			val enemy : Creature = CreatureArchetype.Skeleton.create()
			val x = rand(5,DD.activeDungeon.length-1)
			enemy.location = Vec2f(x, DD.activeDungeon.heightmap(x))
			world.addEntities(enemy)
		}

		val sword = Weapons.BronzeSword.create()

		val inventory = new GameEntity("Bag of Legend")
		val ID = inventory[BagOfLegendData]
		ID.holdItem(sword, 1,1)


		val floorDagger = Weapons.BronzeDagger.create()
		floorDagger[PhysicalData].location = Vec2i(10,2)
		world.addEntities(floorDagger)


		world.addEntities(inventory)

		graphicsEngine.addComponent[DungeonCrawlGraphicsComponent]
		graphicsEngine.addComponent[InventoryGraphicsComponent]
		graphicsEngine.addComponent[WindowingGraphicsComponent]

		gameEngine.addComponent[DungeonCrawlComponent]

		controlEngine.addComponent[InventoryControlComponent]
		controlEngine.addComponent[DungeonCrawlControlComponent]
		controlEngine.addComponent[WindowingControlComponent]

		val WD = controlEngine.controlWorld[WindowingData]

		WD.desktop.drawing.drawBackground = false
		val widget = new Widget(WD.desktop)
		widget.width = DimensionExpression.Constant(400)
		widget.height = DimensionExpression.Constant(400)
	}
}
