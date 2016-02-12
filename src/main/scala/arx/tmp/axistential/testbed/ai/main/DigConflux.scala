package arx.axistential.testbed.ai.main

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/8/13
 * Time: 10:12 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.ai.AIGameComponent
import arx.axistential.game.archetypes.CommonMaterials
import arx.axistential.game.archetypes.Material
import arx.axistential.game.logic.ai.goals.DigGoal
import arx.axistential.testbed.ai.logic.DigWorld
import arx.axistential.testbed.ai.widgets.DigDisplayWidget
import arx.core.datastructures.Killable
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.game.GameEngine

class DigConflux extends Conflux {
	val windowing = new WindowingSystem2()

	val range = 30

	val world = new DigWorld
	world.terrain._setFromFunction( (x,y,z) => {
		if ( z < VoxelCoord.Center.z ) {
			CommonMaterials.Stone
		} else {
			Material.Sentinel
		}
	}, range)

	val engine = new GameEngine
	engine.environments ::= world
	engine.addComponent(new AIGameComponent)
	engine.initialize()
	engine.start()

	val widget = new DigDisplayWidget(world,null)
	widget.width = 100.0f
	widget.height = 100.0f
	windowing.addTopLevelWidget(widget)

//	world.colony.addGoal( SimpleMoveGoal( VoxelCoord.Center.plusX(20) ) )
	val toDig = (for (dx <- 5 until 10; dy <- 5 until 10 ; dz <- -2 to -1 ) yield { VoxelCoord.Center.plusX(dx).plusY(dy).plusZ(dz) }).toList
	world.colony.addGoal( DigGoal( toDig ) )

	override def drawGL() = {
		super.drawGL ()

		windowing.draw()
	}

	override def update(f: Float) = {
		super.update (f)

		windowing.update(f)

		engine.update(f)
	}


	override def handleEvent(event: Event) = windowing.handleEvent(event)

	override def quit() = {
		engine.exit()
		Killable.kill(Killable.ApplicationLevel)
	}
}
