package arx.rog2.game.application

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.ai.goap.GOAPSandbox.PhysicalData
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.EngineCore
import arx.engine.advanced.Engine
import arx.engine.control.components.windowing.WindowingControlComponent
import arx.engine.data.TimeData
import arx.engine.entity.GameEntity
import arx.engine.graphics.components.windowing.WindowingGraphicsComponent
import arx.engine.graphics.data.PovData
import arx.graphics.helpers.{Color, HSBA}
import arx.graphics.pov.TopDownCamera
import arx.rog2.control.AdvanceWorldEvent
import arx.rog2.control.RogCharacterControl
import arx.rog2.game.data.entity.BodySlotQualifier.{Left, Right}
import arx.rog2.game.data.entity.BodySlotType.{Hand, Head, Leg, Torso}
import arx.rog2.game.data.entity._
import arx.rog2.game.data.world.RogData
import arx.rog2.game.data.world.Terrain
import arx.rog2.game.data.world.TerrainFlag
import arx.rog2.game.engine._
import arx.rog2.game.entities.{Material, Weapons}
import arx.rog2.graphics.data.{Overlay, OverlayRegion}
import arx.rog2.graphics.{EntityGraphicsComponent, OverlayGraphicsComponent, TerrainGraphicsComponent}

import scalaxy.loops._

object RogTestApplication extends Engine {
	EngineCore.windowWidth = 1000
	EngineCore.windowHeight = 1000

	override def setUpEngine(): Unit = {
		graphicsEngine.parallelism = 1
		gameEngine.parallelism = 1
		serialGameEngine = true
		serialGraphicsEngine = true

		val terrain = world[Terrain]

		//createExampleTerrain()

		val dim = Vec2i(100, 100)

		val gen = new DungeonMapGenerator()
		gen.mazeCellSelector = (size) => rand((size - 20).max(0), size)
		gen.windingPercent = 15
		gen.roomCount = 600
		gen.roomSize = 15
		val dungeon = gen.generateDungeon(dim)
		dungeon.scale = 2

		for (xr <- 0 until dim.x * 2 optimized; yr <- 0 until dim.y * 2 optimized) {
			val xa = (xr - dim.x) + VoxelCoord.Center.x
			val ya = (yr - dim.y) + VoxelCoord.Center.y

			terrain.voxel(xa, ya, VoxelCoord.Center.z).set(Material.Wood)
			terrain.voxel(xa, ya, VoxelCoord.Center.z + 5).set(Material.Stone).setFlag(TerrainFlag.Ceiling)
			if (!dungeon.grid(xr / dungeon.scale, yr / dungeon.scale)) {
				for (za <- VoxelCoord.Center.z until VoxelCoord.Center.z + 5 optimized) {
					terrain.voxel(xa, ya, za).set(Material.Stone)
				}
			} else {
				val region = dungeon.regions(xr / dungeon.scale, yr / dungeon.scale)
				if (region == dungeon.passageRegion && rand(0,40) == 0) {
					world.addEntity({
						val torch = new GameEntity("Green Torch")
						torch[Physical].position = VCR((xr-dim.x), (yr-dim.y), 1)
						torch[Physical].dimensions = 1.voxel x 1.voxel x 1.5.voxels
						torch[Physical].drawInfo = SliceDrawInfo("standing_torch")
						torch[Physical].drawOrder = -1
						torch[LightSource].lightStrength = 13.voxels
						torch[LightSource].lightBrightness = 0.75f
						torch[LightSource].lightColor = Color(190, 255, 190)
						torch
					})
				}

				var adjQs = Set[Int]()
				for (q <- 0 until 4 optimized) {
					val xadj = (xr / dungeon.scale) + Cardinals.dirvec2d(q).x
					val yadj = (yr / dungeon.scale) + Cardinals.dirvec2d(q).y
					if (dungeon.grid.contains(xadj, yadj) && dungeon.grid(xadj, yadj)) {
						adjQs += q
					}
				}

				if (adjQs.size == 4) {
					terrain.voxel(xa, ya, VoxelCoord.Center.z).set(Material.DarkWood)
				}
			}
		}

		for (room <- dungeon.roomsByRegion.values) {
			val roomDesign = if (room.regionIndex <= 0) { 0 } else { rand(0, 4) }
			// torches
			if (roomDesign == 0 || roomDesign == 2) {
				for (dx <- 0 to 1; dy <- 0 to 1) {
					val xy = dungeon.toVC(room.rect.x + (room.rect.w) * dx, room.rect.y + (room.rect.h) * dy) + Vec2i(dx,dy)
					world.addEntity({
						val torch = new GameEntity("Torch")
						torch[Physical].position = VC(xy, VoxelCoord.Center.z + 2)
						torch[Physical].dimensions = 1.voxel x 1.voxel x 1.5.voxels
						torch[Physical].drawInfo = SliceDrawInfo("wall_torch", dy * 2)
						torch[Physical].drawOrder = -1
						torch[Physical].effectiveOpacity = 0.0f
						torch[LightSource].lightStrength = 15.voxels
						torch[LightSource].lightBrightness = 0.75f
						torch[LightSource].lightColor = Color(255, 235, 190)
						torch
					})
				}
			}


			if (roomDesign == 1) {
				world.addEntity({
					val torch = new GameEntity("Cookpot")
					torch[Physical].position = VC(dungeon.toVC(room.rect.x + room.rect.w/2, room.rect.y + room.rect.h/2), VoxelCoord.Center.z + 1)
					torch[Physical].dimensions = 2.voxel x 2.voxel x 2.5.voxels
					torch[Physical].drawInfo = SliceDrawInfo("cookpot")
					torch[Physical].drawOrder = -1
					torch[Physical].effectiveOpacity = 0.0f
					torch[LightSource].lightStrength = 6.voxels
					torch[LightSource].lightBrightness = 0.75f
					torch[LightSource].lightColor = Color(255, 220, 190)
					torch
				})
			} else if (roomDesign == 0) {
				world.addEntity({
					val torch = new GameEntity("Rug")
					torch[Physical].position = VC(dungeon.toVC(room.rect.x + room.rect.w/2, room.rect.y + room.rect.h/2) - Vec2i(5,2), VoxelCoord.Center.z + 1)
					torch[Physical].dimensions = 10.voxel x 5.voxel x 0.1.voxels
					torch[Physical].drawInfo = TextureDrawInfo("rug", DrawHeight.Floor)
					torch[Physical].drawOrder = -1
					torch[Physical].effectiveOpacity = 0.0f
					torch
				})

				world.addEntities({
					val shortSword = Weapons.shortSword

					val crate = new GameEntity("Crate")
					crate[Physical].withData { o =>
						o.position = VC(dungeon.toVC(room.rect.x + room.rect.w/2, room.rect.y + room.rect.h/2) + Vec2i(3,3), VoxelCoord.Center.z + 1)
						o.dimensions = 2.voxel x 2.voxel x 2.voxels
						o.drawInfo = CubeDrawInfo("crate")
						o.drawOrder = -1
						o.effectiveOpacity = 1.0f
					}

					crate[Furniture].withData { o =>
						o.flags += ObjectFlag.Container
					}

					crate[Inventory].withData { o =>
						o.hold(shortSword)
					}

					crate :: shortSword :: Nil
				})

				for (dx <- 0 to 1; dy <- 0 to 1; dz <- 0 to 2) {
					terrain.voxel(VCR(-5+dx,-5+dy,dz)).set(Material.FlagStones)
				}
			}
		}


		terrain.recordModifications = true

		world.addEntity({
			val bonfire = new GameEntity("Bonfire")
			bonfire[Physical].position = VCR(0, 0, 1)
			bonfire[Physical].dimensions = 2.voxels x 2.voxels x 1.5.voxels
			bonfire[Physical].drawInfo = SliceDrawInfo("bonfire")
			bonfire[LightSource].lightStrength = 18.voxels
			bonfire[LightSource].lightBrightness = 0.95f
			bonfire[LightSource].lightColor = Color(290, 223, 191)
			bonfire
		})

		val ent = new GameEntity("Player")
		ent[Physical].position = VCR(3, 0, 1)
		ent[Physical].dimensions = 1.voxel x 1.voxel x 2.voxels
		ent[Physical].drawInfo = TextureDrawInfo("player")
		ent[Creature].sightRange = 20.voxels
		ent[Equipper].withData(d => {
			d.slots += BodySlot(Hand, Left)
			d.slots += BodySlot(Hand, Right)
			d.slots += BodySlot(Head)
			d.slots += BodySlot(Torso)
			d.slots += BodySlot(Leg, Left)
			d.slots += BodySlot(Leg, Right)
		})

		world.addEntities(ent)
		world[RogData].player = ent

		world.addEntity({
			val enemy = new GameEntity("Enemy")
			enemy[Physical].position = VCR(0, 5, 1)
			enemy[Physical].drawInfo = TextureDrawInfo("enemy1")
			enemy[Creature].damageDealt = 1
			enemy
		})


		val pov = new TopDownCamera(20)
		pov.fovy = 70.0f
		graphicsWorld[PovData].pov = pov

		graphicsWorld[Overlay].overlaidRegions += "test" ->
			OverlayRegion(
				VoxelRegion(VoxelCoord.Center.plusZ(1)),
				"rog/ui/overlay/targetNegativeSolid.png",
				HSBA.White)

		gameEngine.addComponent[RogMainGameComponent]
		gameEngine.addComponent[RogPhysicsGameComponent]
		gameEngine.addComponent[RogLightingGameComponent]
		gameEngine.addComponent[RogVisionGameComponent]
		gameEngine.addComponent[RogLogbookGameComponent]

		graphicsEngine.addComponent[TerrainGraphicsComponent]
		graphicsEngine.addComponent[EntityGraphicsComponent]
		graphicsEngine.addComponent[WindowingGraphicsComponent]
		graphicsEngine.addComponent[OverlayGraphicsComponent]

		controlEngine.addComponent[RogCharacterControl]
		controlEngine.addComponent[WindowingControlComponent]

		gameEngine.eventBus.onEvent {
			case AdvanceWorldEvent(dt) =>
				synchronized {
					gameEngine.updateSerial(dt.inSeconds)
					world[TimeData].time += dt
				}
		}
	}

	override def update(deltaSeconds: Float): Unit = {
		controlEngine.update(deltaSeconds)
	}


	var lastDraw = -1.seconds

	override def draw(): Unit = {
		val now = curTime()
		val delta = if (lastDraw < 0.seconds) {
			0.01666666.seconds
		} else {
			now - lastDraw
		}
		graphicsEngine.updateSerial(delta.inSeconds)
		lastDraw = now
		super.draw()
	}

	def createExampleTerrain(terrain: Terrain): Unit = {
		for (x <- -30 to 30 optimized; y <- -30 to 30 optimized) {
			terrain.voxel(VCR(x, y, 0)).set(Material.Stone)
		}

		for (x <- -30 to 30 optimized; y <- -3 to 3 optimized) {
			terrain.voxel(VCR(x, y, 0)).set(Material.Wood)
		}

		for (x <- -30 to 30 optimized; y <- List(-4, 4)) {
			terrain.voxel(VCR(x, y, 0)).set(Material.Grass)
		}

		for (x <- -30 to 30 optimized; y <- List(-5, 5); z <- 1 to 2; if x.abs > 10) {
			terrain.voxel(VCR(x, y, z)).set(Material.Stone)
		}

		for (x <- List(-10, 10); y <- 5 to 20; z <- 1 to 2) {
			terrain.voxel(VCR(x, y, z)).set(Material.Stone)
		}
		for (x <- -9 to 9; y <- 5 to 19) {
			val z = if (x.abs > 3 || (y - 12).abs > 3) {
				0
			} else {
				-1
			}
			terrain.voxel(VCR(x, y, 0)).set(Material.Sentinel)
			terrain.voxel(VCR(x, y, z)).set(Material.FlagStones)
		}

		terrain.voxel(VCR(-7, 7, 1)).set(Material.FlagStones)
		terrain.voxel(VCR(-6, 8, 1)).set(Material.FlagStones)
	}
}
