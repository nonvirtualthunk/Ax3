package arx.axistential.testbed.ai.main

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/9/15
 * Time: 9:12 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.AIAgentData
import arx.axistential.ai.AIGameComponent
import arx.axistential.ai.AILogging
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.components.physics.BulletPhysicsComponent
import arx.axistential.game.components.physics.DebugData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.ai.goals.animals.ChaseGoal
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.ai.goals.MoveToRangeOfEffectGoal
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord

class MoveGraphingTestbed extends BareBonesTestbed {
	var xPoints = List[(Float,Float)]()
	var zPoints = List[(Float,Float)]()
	def data = List[Dataset](Dataset(xPoints,Color.Black), Dataset(zPoints,Color.Red), Dataset(DD.graphingData, Color.Green))

	def DD = gameEngine.world.aux[DebugData]

	var chase = false

	override def setUpGameEngine(): Unit = {
		AILogging.setLoggingLevel(Noto.Finest)
		val stone = Material.withName("stone")
		gameEngine.world.aux[TerrainData]._setFromFunction((x,y,z) => {
			if (z == VoxelCoord.Center.z-1 || x == z) {
				stone
			} else {
				Material.Sentinel
			}
		},32)

		val creature = AnimalSpecies.withName("wolf").createInstance
		creature.position = ObjectCoord(-0.5f,0.0f,creature.halfHeight)

		val hunted = AnimalSpecies.withName("horse").createInstance
		hunted.position = ObjectCoord(-25.0f,0.0f,hunted.halfHeight)

		val goal = if (chase) {
			new ChaseGoal(hunted, 40.voxels, 50.voxels, false, MoveGait.Run)
		} else {
			MoveToRangeOfEffectGoal(creature, VoxelCoord.fromRelative(25,0,25), MoveGait.Walk)
		}

		creature.aux[AIAgentData].passiveGoals ::= goal
		gameEngine.world.addEntity(creature)

		gameEngine addComponent new BulletPhysicsComponent
		gameEngine addComponent new AIGameComponent
		gameEngine addComponent new GameEngineComponent {
			override def update(time: UnitOfTime): Unit = {
				if ((!chase && creature.position.x < 24.0f) || (chase && creature.position.x > -20.0f)) {
					xPoints ::= gameEngine.time.inSeconds -> creature.position.x
					zPoints ::= gameEngine.time.inSeconds -> creature.position.z
//					xPoints ::= gameEngine.time.inSeconds -> creature.velocity.x.inVoxelsPerSecond
//					zPoints ::= gameEngine.time.inSeconds -> creature.velocity.z.inVoxelsPerSecond

//					world.aux[DebugData].graphingData ::= gameEngine.time.inSeconds -> tcreature.velocity.x.inVoxelsPerSecond
				}
			}
		}
	}
	override def setUpUI(): Unit = {
//		val grapher = new GraphWidget(Some(mainWidget),() => gameEngine.world.aux[DebugData].graphingData)
		val grapher = new GraphWidget(Some(mainWidget),() => data)
		grapher.width = mainWidget.clientWidth
		grapher.height = mainWidget.clientHeight
	}

	override def setUpGraphicsEngine(): Unit = {}
}
