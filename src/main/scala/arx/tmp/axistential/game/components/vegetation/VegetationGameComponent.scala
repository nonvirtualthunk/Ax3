package arx.axistential.game.components.vegetation

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/30/14
 * Time: 3:54 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.archetypes.species.RecurringLivingThingProduct
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.data.world.AxistentialTimeData._
import arx.axistential.game.data.world.VegetationData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.query.ContinuousQueryListener
import arx.core.query.ContinuousQueryWindow
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.events.EntityChangedEvent
import arx.tmp.game.logic.world.data.TimeData

import scalaxy.loops._

class VegetationGameComponent extends AtIntervalGameEngineComponent with ContinuousQueryListener[GameEntity] {
	lazy val plants = world.createEntityArchetypeTypeQuery[PlantSpecies].withListener(this,fireOnExistingResults = true)
	lazy val animals = world.createEntityArchetypeTypeQuery[AnimalSpecies].withListener(this,fireOnExistingResults = true)
	// A moving window through all plants in the world, taken 10% at a time
	lazy val window = new ContinuousQueryWindow(plants,0.1f)



	override def queryResultAdded(t: GameEntity): Unit = {

		updatePlant(t.as[TPhysicalEntity], t.archetype.as[PlantSpecies], world.aux[TimeData])
	}
	override def queryResultRemoved(t: GameEntity): Unit = {}

	override def initialize(): Unit = {
		val VD = world.aux[VegetationData]
		val TMD = world.aux[TimeData]
		if (! VD.initialized) {
			VD.initialized = true

			for (plant <- plants ; species <- plant.archetype.ifIs[PlantSpecies]) {
				updatePlant(plant.as[TPhysicalEntity],species,TMD)
			}
		}
	}

	override def interval: UnitOfTime = 0.2.second

	def nextAgeCategoryStart (ac : AgeCategory, species : PlantSpecies) = {
		ac.followedBy match {
			case Some(nac) => species.ageCategoryStartTimes.getOrElse(nac,{Noto.warn(s"No start time given for age category $nac");10000.turning})
			case None => 10000.turning
		}
	}

	def updatePlant(plant: TPhysicalEntity, species: PlantSpecies, TMD: TimeData) = {
		val PD = plant.aux[PlantData]
		while (PD.age >= nextAgeCategoryStart(PD.ageCategory,species)) {
			PD.ageCategory.followedBy match {
				case Some(nextAge) =>
					PD.ageCategory = nextAge

					val oldHeight = plant.boundingDimensions.z.inVoxels
					plant.collisionShape = species.collisionShapeForAgeCategory(PD.ageCategory)
					val newHeight = plant.boundingDimensions.z.inVoxels
					plant.position = plant.position.plusZ((newHeight - oldHeight) * 0.5f)

					// On age category change, initialize all of the plant's products that are produced at the new age category
					// (that are not recurring), recurring products are potentially dependent on season, etc, and are produced separately
					for (prod <- species.products.notOfType[RecurringLivingThingProduct] if prod.atAgeCategory == PD.ageCategory) {

						for (i <- 0 until prod.number.toInt) {
							val inst = prod.archetype.createInstance
							PD.addPart(inst,prod.structural,prod.harvestableBy)
						}
					}

		//			plant.eventHappened()
					world.fireEvent(EntityChangedEvent(plant))
				case None =>
					// TODO, allow plants to die of old age
					Noto.warn("Plant ran out of age categories, it should die now")
			}
		}
		

		for (prod <- species.products.ofType[RecurringLivingThingProduct]) {
			val lastMade = PD.productsLastProduced.getOrElse(prod,PD.born)
			if (TMD.time - lastMade > prod.productionInterval && prod.seasons.contains(TMD.season) && prod.atAgeCategory == PD.ageCategory) {
				lazy val existingCount = PD.parts.count(p => p.entity.archetype == prod.archetype)
				if (prod.maximumCount.isEmpty || prod.maximumCount.get > existingCount) {
					val numberToProduce = prod.numberPerProduction.toInt

					for (n <- 0 until numberToProduce optimized) {
						val inst = prod.archetype.createInstance

						PD.addPart(inst,prod.structural,prod.harvestableBy)
					}

					PD.productsLastProduced += prod -> gameEngine.time
				}
			}
		}
	}

	override def onInterval(): Unit = {
		val TMD = world.aux[TimeData]
		for (plant <- window.next ; species <- plant.archetype.ifIs[PlantSpecies]) {
			updatePlant(plant.as[TPhysicalEntity],species,TMD)
		}
	}
}


