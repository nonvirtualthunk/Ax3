package arx.eldr.application.testbeds

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.{Vec2f, Vec3f, Vec3i}
import arx.eldr.game.archetypes.Material
import arx.eldr.game.entity.data.CubeShape
import arx.eldr.game.entity.data.{AIAgentData, PhysicalData}
import arx.eldr.game.logic.ai.goals.MoveGoal
import arx.eldr.game.logic.ai.{AIGameComponent, AIGroupData}
import arx.eldr.game.logic.light.LightComponent
import arx.eldr.game.world.data.Terrain
import arx.eldr.graphics.entities.EntityGraphicsComponent
import arx.eldr.graphics.environment.{SkyboxGraphicsComponent, TerrainGraphicsComponent}
import arx.engine.advanced.Engine
import arx.engine.entity.GameEntity
import arx.engine.graphics.data.PovData
import arx.graphics.pov.EyeCamera

import scalaxy.loops._

object AITestbed extends Engine {

	override def setUpEngine(): Unit = {
		graphicsEngine.addComponent[TerrainGraphicsComponent]
		graphicsEngine.addComponent[SkyboxGraphicsComponent]
		graphicsEngine.addComponent[EntityGraphicsComponent]

		gameEngine.addComponent[LightComponent]
		gameEngine.addComponent[AIGameComponent]

		graphicsEngine.graphicsWorld[PovData].pov = {
			val cam = new EyeCamera(Vec3f(0,0,15),Vec3f.UnitX,Vec3f.UnitZ)
			cam.moveSpeed = Vec3f(0.35f)
			cam.turnSpeed = Vec2f(0.7f)
			cam.viewDistance = 200.0f
			cam
		}

		val terrain = world[Terrain]
		terrain.setMaterialsInRegion(VCR(-25,-25,-1) -> VCR(25,25,-1), Material.withName("grass"))
		terrain.setMaterialsInRegion(VCR(-10,-10,0) -> VCR(10,10,0), Material.withName("stone"))

		val group = new GameEntity("Testgroup")

		val creature = new GameEntity("Testman")
		creature[PhysicalData].position = VCR(-20,0,0)
		creature[PhysicalData].shape = CubeShape(Vec3i.One)
		creature[AIAgentData].groups += group
		creature[AIAgentData].goals += new MoveGoal(VoxelRegion(VCR(20,0,0)),MoveGait.Walk)

		group[AIGroupData].entities += creature

		world.addEntities(creature, group)
	}
}