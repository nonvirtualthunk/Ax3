package arx.axistential.game.logic.ai.groupgoals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/5/15
 * Time: 8:25 AM
 */

import arx.Prelude._
import arx.axistential.ai._
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.helpers.FarmZoneData
import arx.axistential.game.entities.helpers.Zone
import arx.axistential.game.logic.ai.goals.SowSeedsGoal
import arx.core.units.UnitOfTime
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord

import scalaxy.loops._




class PlantFarmGoal (farmZone : Zone) extends AIGroupGoal {
	var lastSpecies : Option[PlantSpecies] = None
	var lastTerrainModifiedCount = 0

	override def createSubGoal(forGroup : TAIGroup) : Option[Goal] = {
		val FD = farmZone.aux[FarmZoneData]

		FD.speciesToPlant match {
			case Some(species) => {
				lastSpecies = FD.speciesToPlant
				if (checkAndUpdateIfTerrainChanged) {
					recomputePlantingLocations()
				}

				val properPlants = FD.sownPlants
				val properPlantLocs = properPlants.map(_.adjustedFootVoxelPos).toSet
				val notYetPlantedLocations = FD.plantAtLocations.filter(t => ! properPlantLocs.contains(t.toVoxelCoord))
				notYetPlantedLocations match {
					case Nil => None
					case _ => Some(new SowSeedsGoal(species,notYetPlantedLocations,farmZone))
				}
			}
			case None => None
		}
	}

	/** How often this should be checked for new work that needs to be done */
	override def checkInterval: UnitOfTime = 10.seconds

	def checkAndUpdateIfTerrainChanged = {
		val TD = farmZone.world.aux[TerrainData]
		val allT = TD.materialGrid.allTaleaPositionsInRegionInclusive(farmZone.region).map(v => TD.materialGrid.taleaForReadOnly(v.x,v.y,v.z))
		val ltmc = allT.isum(_.modifiedCount)
		val ret = ltmc > lastTerrainModifiedCount
		lastTerrainModifiedCount = ltmc
		ret
	}

	def recomputePlantingLocations (): Unit = {
		val FD = farmZone.aux[FarmZoneData]
		implicit val TD = farmZone.world.aux[TerrainData]

		val species = FD.speciesToPlant.get // precondition of this is that there is a species
		val fullGrownCS = species.collisionShapeForAgeCategory(species.speciesKind.matureAgeCategory)
		val fullGrownSize = fullGrownCS.boundingDimensions.inVoxels

		val xSpacing = (fullGrownSize.x * 0.5f).round.toInt
		val ySpacing = (fullGrownSize.y * 0.5f).round.toInt
		val zHeight = fullGrownSize.z.ceil.toInt

		val xyOffset = Vec3i(xSpacing,ySpacing,0)

		val reg = farmZone.region
		val cursor = MutableVoxelCoord(reg.min + Vec3i(xSpacing,ySpacing,0))

		val existingPlants = FD.sownPlants.map(_.adjustedFootVoxelPos)
		var plantAt = List[VoxelCoord]()

		// we use the bounding region to ensure that we don't miss anything when iterating, to prevent hitting
		// non included voxels, we filter by reg.contains in the inner loop
		val boundingReg = reg.boundingRegion
		val firstPossible = boundingReg.min
		val lastPossible = boundingReg.max

		val possibles = new ExternalizableDenseVoxelField[Byte](Vec3i(lastPossible - firstPossible + 1))
		def eliminate (x:Int,y:Int,z:Int) { possibles(x - firstPossible.x,y - firstPossible.y,z - firstPossible.z) = -1 }
		def isEliminated (x:Int,y:Int,z:Int) = { possibles(x - firstPossible.x,y - firstPossible.y,z - firstPossible.z) == -1 }

		for (existingPlant <- existingPlants) {
			for (dx <- -xSpacing to xSpacing optimized ; dy <- -ySpacing to ySpacing optimized; dz <- 0 until zHeight optimized) {
				eliminate(existingPlant.x + dx,existingPlant.y + dy,existingPlant.z + dz)
			}
		}

		for (x <- firstPossible.x to lastPossible.x optimized ; y <- firstPossible.y to lastPossible.y optimized) {
			var z = lastPossible.z
			while (z >= firstPossible.z) {
				if (! reg.contains(x,y,z)) {
					eliminate(x,y,z)
					z -= 1
				} else if (TD.isSolid(x,y,z)) {
					val startZ = z
					while (z >= firstPossible.z && z >= startZ - (zHeight - 1)) {
						eliminate(x,y,z)
						z -= 1
					}
				} else {
					z -= 1
				}
			}
		}

		for (x <- firstPossible.x + xSpacing to lastPossible.x - xSpacing optimized ;
			  y <- firstPossible.y + ySpacing to lastPossible.y - ySpacing optimized)
		{
			var z = lastPossible.z
			while (z >= firstPossible.z) {
				if (! isEliminated(x,y,z) && TD.isSolid(x,y,z-1)) {
					var obstructed = false
					for (dx <- -xSpacing to xSpacing optimized ; dy <- -ySpacing to ySpacing optimized) {
						if (! obstructed) {
							if (isEliminated(x + dx,y + dy,z)) {
								obstructed = true
							}
						}
					}

					if (!obstructed) {
						plantAt ::= VoxelCoord(x,y,z)
						for (dx <- -xSpacing to xSpacing optimized ; dy <- -ySpacing to ySpacing optimized; dz <- 0 until zHeight optimized) {
							eliminate(x + dx,y + dy,z + dz)
						}
						z = 0 // skip past any further
					}
				}

				z -= 1
			}
		}

		FD.plantAtLocations = plantAt
	}
}





