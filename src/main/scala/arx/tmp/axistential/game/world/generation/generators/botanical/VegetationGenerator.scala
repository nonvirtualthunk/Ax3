package arx.axistential.game.world.generation.generators.botanical

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/7/13
 * Time: 10:44 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes._
import arx.axistential.game.archetypes.species.PlantSpecies.LightUsage
import arx.axistential.game.archetypes.species.PlantSpecies.WaterUsage
import arx.axistential.game.archetypes.species.PlantAssociation
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.archetypes.species.RecurringLivingThingProduct
import arx.axistential.game.data.entity.PlantKind
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.data.entity.plants.PlantSeedContainerData
import arx.axistential.game.data.entity.plants.PlantSeedData
import arx.axistential.game.data.world._
import arx.axistential.game.logic.general.TerrainLogic
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.mathutil.WeightedDistribution
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.core.FibonacciHeap
import arx.core.THasSortKey
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.GridCell2D
import arx.tmp.game.logic.world.data.TimeData

import scala.collection.mutable
import scalaxy.loops._

class VegetationGenerator extends TWorldGenerator {

	case class SoilNode(v : VoxelCoord,dist : Int) extends THasSortKey {
		def sortKey: Float = dist
	}

	/**
	 * Perform this component's generation, storing the results in the provided world.
 *
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		if ( phase == WorldGenerator.Botanical ) {
			if ( stage == WorldGenerator.Voxel ) {
				generateGroundCover(world,progress)
			} else if ( stage == WorldGenerator.Entity ) {
				generatePlantEntities(world,progress)
			}
		}
		Nil
	}

	def speciesMatchesWorld(world:World)(species:PlantSpecies) = {
		species.biomeDescriptor.matches( world.aux[BiomeData].mainBiome )
	}

	def generatePlantEntities(world: World, progress: Progress) {
		val TD = world.aux[TerrainData]
		val PD = world.aux[TopologicalData]
		val SD = world.aux[SoilData]
		val FD = world.aux[FluidData]
		val TimeD = world.aux[TimeData]
		val reg = world.worldRegionAsSpatialRegion

		val allSpecies = PlantSpecies.allArchetypes.filter ( speciesMatchesWorld(world) ).filter( _.speciesKind != PlantKind.Groundcover )
		val allSpeciesArray = allSpecies.toArray
		val speciesWeightsArray = allSpeciesArray.map( _ => 1.0f )
		Noto.info("Num valid species : " + allSpecies.size)
		val reversedAssociations = new mutable.HashMap[PlantSpecies, List[PlantAssociation]]().withDefaultValue(Nil)
		for ( p <- allSpecies ; assoc <- p.associatedWith ) {
			val rAssoc = new PlantAssociation
			rAssoc.chance = assoc.chance
			rAssoc.distance = assoc.distance
			rAssoc.otherSpecies = p
			reversedAssociations(assoc.otherSpecies) = rAssoc :: reversedAssociations.getOrElse(assoc.otherSpecies,Nil)
		}

		var plantCount = 0
		val plantLimit = 1000 * ( (world.worldRegionAsSpatialRegion.dimensions.x * world.worldRegionAsSpatialRegion.dimensions.y) / (512.0f*512.0f) )

		val highAgeOfMaturityBasePoint = 1.0f //in turnings
		val locationChecks = 2

		def pickLocation () = {
			var location = VoxelCoord.Sentinel
			var checks = locationChecks
			while ( location.isSentinel && checks > 0 ) {
				val x = rand(reg.minX,reg.maxX)
				val y = rand(reg.minY,reg.maxY)

				location = TerrainLogic.oneAboveFirstTerrainInColumn(x,y,TD,PD)

				checks -= 1
			}
			location
		}

		def createPlant ( species : PlantSpecies , location : VoxelCoord, spawnReduction : Float = 1.0f ) {
			val plant = species.createInstance
			val PlantD = plant.aux[PlantData]
			// Give the plant a random age between just "born" and nearing death
			// TODO: Give this a more realistic distribution, favoring newly born over senescent
			PlantD.born = TimeD.time - randomGenerator.nextFloat() * species.ageCategoryStartTimes.values.max

			if ( location.notSentinel ) {
				//+====================+ Gather Up all the soil blocks in range +====================+
				val distLimit = species.rootRadius.inVoxels
				val soilBlockSet = new mutable.HashSet[VoxelCoord]()
				val soilQ = new FibonacciHeap[SoilNode]
				soilQ.enqueue(SoilNode(location.minusZ(1),0))

				while ( soilQ.nonEmpty ) {
					val n = soilQ.dequeue()

					val mat = TD.materialAt(n.v)
					if ( mat.materialTypes( Material.Soil ) ) {
						soilBlockSet.add(n.v)

						if ( n.dist < distLimit ) {
							for ( q <- 0 until 6 optimized ) {
								val av = n.v + dirvec(q)
								if ( ! soilBlockSet.contains(av) ) {
									soilQ.enqueue(SoilNode(av,n.dist+1))
								}
							}
						}
					}
				}

				if ( soilBlockSet.nonEmpty ) {
					val soilBlocks = soilBlockSet.toList
					//+====================+ Check for sufficient nutrients +====================+
					val totalNutrients = soilBlocks.map( SD.nutrientLevel ).sum

					//We work under the assumption that, on average, it will be a third of theoretical possible,
					//since obviously, the top half of the sphere is absent, and not all will be soil
					val rootVolume = (4.0f/3.0f) * pi * powf(species.rootRadius.inVoxels,3.0f) * 0.5f
					if ( totalNutrients >= species.nutrientUsage.perYear * rootVolume ) {
						//+====================+ Check for acceptable moisture levels +====================+
						def moistureAt ( v : VoxelCoord ) = {
							if ( FD.isFluidAt(v.x,v.y,v.z+1) ) {
								WaterUsage.Inundated.inCentimeters
							} else if ( FD.isFluidAt(v.x+1,v.y,v.z) || FD.isFluidAt(v.x,v.y+1,v.z) || FD.isFluidAt(v.x-1,v.y,v.z) || FD.isFluidAt(v.x,v.y-1,v.z) ) {
								math.min(SD.moistureLevel(v) + 40.0f, WaterUsage.VeryWet.inCentimeters)
							} else { SD.moistureLevel(v) }
						}
						val avgMoisture = soilBlocks.map( moistureAt ).sum / soilBlocks.size.toFloat

						if ( (species.waterUsage.waterPreference - avgMoisture.cm).abs <= species.waterUsage.tolerance ) {
							//+====================+ Check for sufficient light levels +====================+
							val lr = species.leafRadius.inVoxels.toInt
							val lr2 = lr * lr
							var count = 0
							var sum = 0
							for ( dx <- -lr to lr optimized ; dy <- -lr to lr optimized ) {
								if ( dx*dx + dy*dy <= lr2 ) {
									count += 1
									sum += LightUsage.FullSunlight - SD.shadeGrid(location.x + dx,location.y + dy)
								}
							}

							if ( (species.lightUsage.lightPreference - (sum/count.toFloat).toInt).abs <= species.lightUsage.tolerance ) {
								//+====================+ Consume nutrients and light +====================+
								val nutrientDelta = (species.nutrientUsage.perYear * rootVolume) / soilBlocks.size.toFloat

								for ( block <- soilBlocks ) {
									SD.deltaSoilNutrients(block) = (SD.deltaSoilNutrients(block) - nutrientDelta).toByte
								}
								for ( dx <- -lr to lr optimized ; dy <- -lr to lr optimized ) {
									if ( dx*dx + dy*dy <= lr2 ) {
										val cur = SD.shadeGrid(location.x + dx,location.y + dy)
										//This way things that are smaller than a whole voxel can shade less
										val pcnt = species.lightUsage.shadePcnt * math.min(1.0f, pi * powf(species.leafRadius.inVoxels,2.0f))
										val nex = math.min(cur + LightUsage.FullSunlight * pcnt, LightUsage.FullSunlight)
										SD.shadeGrid(location.x + dx,location.y + dy) = nex.toByte
									}
								}

								//+====================+ Add the actual plant to the world +====================+
								val basePosition = location.toObjectCoordFoot.plusZ(plant.boundingDimensions.z.inVoxels * 0.5f)
								plant.position = if ( plant.boundingDimensions.x.inVoxels < 1.0f && plant.boundingDimensions.y.inVoxels < 1.0f ) {
									basePosition.plusXY( rand(-0.35f,0.35f) , rand(-0.35f,0.35f) )
								} else { basePosition}


								world.addEntity(plant)
								plantCount += 1

								var chance = 0.0f
								var travelDistance = 1.meter
								for ( prod <- species.products ) {
									val mult = prod match {
										case r : RecurringLivingThingProduct => (1.0f / r.productionInterval.inCycles) * r.numberPerProduction * 6.5f
										case _ => prod.number.toFloat
									}

									prod.archetype.auxDataOpt[PlantSeedData] match {
										case Some(seed) => {
//											chance += mult

											travelDistance = seed.travelDistance
										}
										case _ =>
									}
									prod.archetype.auxDataOpt[PlantSeedContainerData] match {
										case Some(container) => {
											for ( part <- container.seedParts ){
												part.archetype.auxDataOpt[PlantSeedData] match {
													case Some(seed) => {
//														chance += mult
														travelDistance = seed.travelDistance
													}
													case _ =>
												}
											}
										}
										case _ =>
									}
								}
								chance = species.spreadChance

								chance *= spawnReduction
								while ( chance > 0.0f ) {
									if ( rand(0.0f,1.0f) < chance ) {
										for ( i <- 0 until species.spreadNumber ) {
											val baseDist = travelDistance.inVoxels
											val dist = ND( baseDist , baseDist * 0.2f )

											val newXY = location.xy + randVec2(dist).round
											val nextV = TerrainLogic.oneAboveFirstTerrainInColumn(newXY.x,newXY.y,TD,PD)

											val nextReduction = spawnReduction * ND(species.spreadFalloff,0.1f * species.spreadFalloff)
											createPlant(species,nextV,nextReduction)
										}
									}
									chance -= 1.0f
								}

								for ( assoc <- reversedAssociations(species) if rand(0.0f,1.0f) < assoc.chance ) {
									val d = assoc.distance.inVoxels
									val xyLoc = location.xy + randVec2(ND(d,d*0.2f)).round
									val loc = TerrainLogic.oneAboveFirstTerrainInColumn(xyLoc.x,xyLoc.y,TD,PD)
									createPlant( assoc.otherSpecies , loc , 1.0f )
								}
							}
						}
					}
				}
			}
		}

		while ( plantCount < plantLimit ) {
			val progressPcnt = plantCount / plantLimit.toFloat
			progress.percentDone = progressPcnt

			for ( i <- 0 until allSpeciesArray.length optimized ) {
				val species = allSpeciesArray(i)
				speciesWeightsArray(i) = math.max(0.0f,highAgeOfMaturityBasePoint - (species.ageOfMaturity.inTurnings * (progressPcnt - 0.3f)) * 1.5f)
			}

			val distribution = new WeightedDistribution( speciesWeightsArray , allSpeciesArray )
			val species = distribution.rand()

			createPlant(species,pickLocation())
			/*
			We want to generate plants for the various species, skewed in a couple ways
			- we want to generate longer lived plants first, in the long run ground cover adapts around trees, not vice versa
			- we want to generate in clusters, so each plant has some possibility of spawning one or more successor

			The second is pretty well established, but I'm less certain how to do the first, perhaps bucket the ageOfMaturity
			and take n from each? Or, give a variable weight distribution at each step that shifts as the percent complete does.
			 */
		}

		val entsByArch = world.entities.groupBy( _.archetype )
		for ( (arch,ents) <- entsByArch ) {
			if ( arch.isInstanceOf[PlantSpecies] ) { Noto.info(s"${arch.name} : ${ents.size}") }
		}
	}

	def generateGroundCover(world: World, progress: Progress) {
		val TD = world.aux[TerrainData]
		val terrain = TD.materialGrid
		val coverings = TD.coveringGrid
		val heightmap = world.aux[TopologicalData].heightmap
		val depthmap = world.aux[TopologicalData].depthmap
		val WorldScale = world.aux[ScaleData]
		val FD = world.aux[FluidData]
		val heightmapCells = heightmap.allInRegionInclusiveOrdered(world.worldRegionAsSpatialRegion)
		//			val rainfallCells = hydrologicalData.rainfall.allInRegionInclusiveOrdered(env.worldRegion)

		//			val lushGrass = env.coveringTypes.add(new Covering(LushGrass))
		val grass = TD.coveringTypes(new Covering(PlantSpecies.withName("grass")))
		//			val dryGrass = env.coveringTypes.add(new Covering(DryGrass))

		val dryCutoff = 150
		val lushCutoff = 225

		//			(heightmapCells zip rainfallCells).foreach {
		heightmapCells.foreach {
			//				case (heightmapCell, rainfallCell) => {
			case (heightmapCell) => {
				val heightmapWindow = heightmap.windowCenteredAt(heightmapCell.position)
				val depthmapWindow = depthmap.windowCenteredAt(heightmapCell.position)


				val offset = Vec3i(heightmapCell.position.x, heightmapCell.position.y, world.center.z)
				for ( x <- 0 until GridCell2D.dimension  optimized ; y <- 0 until GridCell2D.dimension ) {
					val baseDepth = depthmapWindow(x, y)
					val baseHeight = heightmapWindow(x, y)
					var effectiveHeight = baseHeight
					while ( terrain(offset.x + x, offset.y + y, offset.z + effectiveHeight - 1) == 0 && effectiveHeight > baseDepth ) {
						effectiveHeight -= 1
					}
					if (effectiveHeight > baseDepth && effectiveHeight < WorldScale.SnowLine.inVoxels ) {
						if ( ! FD.isFluidAt(offset.x + x,offset.y + y,offset.z + effectiveHeight) ) {
							//								val rainfall = rainfallCell(x, y)
							val rainfall = 200

							val slopes = Array(
								heightmapWindow(x + 1, y) - baseHeight,
								heightmapWindow(x - 1, y) - baseHeight,
								heightmapWindow(x, y + 1) - baseHeight,
								heightmapWindow(x, y - 1) - baseHeight
							)
							val steepness = (math.abs(slopes(0)) + math.abs(slopes(1)) + math.abs(slopes(2)) + math.abs(slopes(3))) / 4.0f

							if (steepness < 2.0f) {
								//									val grassType = if (rainfall <= dryCutoff) {
								//										dryGrass
								//									} else if (rainfall >= lushCutoff) {
								//										lushGrass
								//									} else {
								//							grass
								//									}
								var q = 0
								while (q < 6) {
									if (terrain(offset.x + x + Cardinals.cardinals(q).x, offset.y + y + Cardinals.cardinals(q).y, offset.z + effectiveHeight - 1 + Cardinals.cardinals(q).z) <= 0) {
										//											coverings(offset.x + x, offset.y + y, offset.z + baseHeight - 1, q) = grassType
										coverings(offset.x + x, offset.y + y, offset.z + effectiveHeight - 1, q) = grass
									}
									q += 1
								}
							}
						}
					}
				}
			}
		}
	}
}
