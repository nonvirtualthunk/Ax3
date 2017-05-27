package arx.rog2.game.engine

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.query.ContinuousQueryListener
import arx.core.units.UnitOfTime
import arx.core.vec.Cardinals
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.entity.TGameEntity
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.modules.lighting.Shadowcaster
import arx.rog2.engine.RogComponent

import scalaxy.loops._
import arx.rog2.game.data.world._
import arx.rog2.game.data.entity._

import scala.collection.mutable

class RogLightingGameComponent(engine : GameEngine) extends GameComponent(engine) with RogComponent {

	var lastModificationCount = -1

	lazy val lightQuery = world.auxDataQuery[LightSource].withListener(new ContinuousQueryListener[TGameEntity] {
		override def queryResultAdded(t: TGameEntity): Unit = {}
		override def queryResultRemoved(t: TGameEntity): Unit = {
			world[Light].lightGrids.remove(t)
		}
	}, fireOnExistingResults = false)

	def recomputeLight(ent: TGameEntity) = {
		val PD = ent[Physical]
		val LD = ent[LightSource]
		val T = world[Terrain]
		val lightGrid = world[Light].lightGridFor(ent)
		val scale = LD.lightBrightness

		val occlusions = world[EntityOcclusionData].occlusionsRelativeTo(PD.position, LD.lightStrength.inVoxels.toInt, 0.0f)

		Shadowcaster.shadowcast(
			(dx,dy,dz) => if (T.voxel(PD.position + Vec3i(dx, dy, dz)).isSentinel) {
				occlusions(Vec3i(dx,dy,dz))
			} else {
				1.0f
			},
			(dx,dy,dz, lp) => lightGrid(dx,dy,dz) = lp * scale,
			world[Light].shadowGridFor(ent),
			LD.lightStrength.inVoxels.toInt,
			(pcntDist) => pcntDist * pcntDist,
			(dx,dy,dz) => true)

		LD.lastCalculated = Some(world.time)
	}

	var lastPlayerPos = VC(0,0,0)

	override protected def update(dt: UnitOfTime): Unit = {
		// two reasons we want to update,
		// 1) the world was modified within the range of the light and
		// 2) the light moved from being out of sight range to being in sight range
		// 3) the light just came into existence

		val recentMods = world[Terrain].modifications.takeWhile(m => m.idx >= lastModificationCount)
		val playerPos = player[Physical].position
		val playerVis = player[Creature].sightRange
		var lightsToRecalc = List[TGameEntity]()


		for (ent <- lightQuery) {
			if (!ent.hasAuxData[Physical]) {
				Noto.warn("Invalid: Light source with no physical existence")
			}

			val PD = ent[Physical]
			val LD = ent[LightSource]

			if (PD.heldIn.isEmpty && PD.position.distanceTo(playerPos) < LD.lightStrength + playerVis) {
				lazy val neverCalculated = LD.lastCalculated.isEmpty
				lazy val inRangeOfPlayer = PD.position.distanceTo(lastPlayerPos) >= LD.lightStrength + playerVis
				lazy val nearbyModification = recentMods.exists(m => PD.position.distanceTo(VoxelCoord(m.x,m.y,m.z)) < LD.lightStrength)
				if (neverCalculated || inRangeOfPlayer || nearbyModification) {
					lightsToRecalc ::= ent
				}
			}
			// if this is held in something, act as though it is uncalculated
			if (PD.heldIn.isDefined) {
				LD.lastCalculated = None
			}
		}

		lightsToRecalc.foreach(recomputeLight)

		lastModificationCount = world[Terrain].modificationCount
		lastPlayerPos = player[Physical].position
	}
}
