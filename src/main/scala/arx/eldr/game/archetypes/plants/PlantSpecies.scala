package arx.eldr.game.archetypes.plants

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.datastructures.MultiMap
import arx.core.traits.{ArxEnum, ArxEnumObject, TSentinel}
import arx.core.units.{UnitOfDistance, UnitOfTime}
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.archetypes._
import arx.eldr.game.archetypes.Reaction.{InputMap, OutputMap}
import arx.eldr.game.entity.data.CubeShape
import arx.eldr.game.entity.data.SubVoxelShape
import arx.eldr.game.entity.data.{ItemFlag, PhysicalData}
import arx.eldr.game.requirements._
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.{GameArchetype, GameEntity, TArchetypeKind, TGameEntity}
import arx.engine.requirement.EntityDescriptor

import scala.language.implicitConversions


class PlantSpecies(nomen: String) extends GameArchetype(nomen, PlantSpecies) {

	/**
	 * Create a new plant with no explicit progenitors, any genetics will be random
	 */
	def createPlant(): TGameEntity = createPlant(GameEntity.Sentinel, GameEntity.Sentinel)

	def createPlant(father: TGameEntity, mother: TGameEntity): TGameEntity = {
		val ret = new GameEntity(nomen)
		ret.archetype = this
		ret.copyDataFrom[PlantData](this)
		ret
	}
}

class PlantData extends TGameEntityAuxData {
	var sunlightNeeded = 1
	var waterNeeded = 1
	var nutrientConsumption = 1

	var growingStyle = GrowingStyle.Plant

	var fullHeight = 0.5.meter
	var ageCategoryStarts = Map[AgeCategory, UnitOfTime]()
	var age = 0.years

	var species : PlantSpecies = PlantSpecies.Sentinel

	// The oldest age category that this is at least the minimum age of
	def ageCategory = ageCategoryStarts.toList.filter(a => a._2 < age).sortBy(a => -a._2).head._1
}

class TreeData extends TGameEntityAuxData {
	var trunkMaterial = Material.Sentinel
	var sticks = 0.0f
	var stickGrowthPerYear = 2
	var trunkSize = 7.0f
	var trunkGrowthPerYear = 7
	var choppingToolRequirements = List[EntityDescriptor]()
}

class FruitData extends TGameEntityAuxData {

}

object PlantSpecies extends TArchetypeKind {
	def treeSpecies(name: String, growthDurationMultiplier: Float, height: UnitOfDistance) = {
		val ret = new PlantSpecies(name)
		val PD = ret[PlantData]
		PD.sunlightNeeded = 16
		PD.waterNeeded = 10
		PD.nutrientConsumption = 5
		PD.growingStyle = GrowingStyle.Tree
		PD.fullHeight = height
		PD.species = ret
		PD.ageCategoryStarts = Map(
			AgeCategory.Tree.Seedling -> 0.seconds,
			AgeCategory.Tree.Sapling -> 1.sowing,
			AgeCategory.Tree.Mature -> 1.turning,
			AgeCategory.Tree.Old -> 5.turning
		).mapValues(_ * growthDurationMultiplier)

		val TD = ret[TreeData]
		TD.trunkMaterial = Material.withName("wood") //TODO: Change to in-code definition of woods

		GatherSaplingReaction(ret)

		val PHD = ret[PhysicalData]
		PHD.shape = SubVoxelShape(Vec3f(0.1f))
		PHD.occupiesSpace = false

		ret
	}

	def bushSpecies(name : String, growthDurationMultiplier : Float, height : UnitOfDistance) = {
		val ret = new PlantSpecies(name)
		val PD = ret[PlantData]
		PD.sunlightNeeded = 10
		PD.waterNeeded = 6
		PD.nutrientConsumption = 3
		PD.growingStyle = GrowingStyle.Bush
		PD.fullHeight = height
		PD.species = ret
		PD.ageCategoryStarts = Map(
			AgeCategory.Bush.Shoot -> 0.seconds,
			AgeCategory.Bush.Blooming -> 0.5.sowing,
			AgeCategory.Bush.Mature -> 1.sowing
		).mapValues(_ * growthDurationMultiplier)

		GatherSaplingReaction(ret)
		ChopTreeReaction(ret)

		ret[PhysicalData].shape = SubVoxelShape(Vec3f(0.1f))

		ret
	}

	val OakTree = treeSpecies("Oak", 2.0f, 10.meters)

	val Sentinel = new PlantSpecies("Sentinel") with TSentinel
}











