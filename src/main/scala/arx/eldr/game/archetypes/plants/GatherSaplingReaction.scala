package arx.eldr.game.archetypes.plants

import arx.core.datastructures.MultiMap
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.archetypes._
import arx.eldr.game.archetypes.Reaction.InputMap
import arx.eldr.game.archetypes.Reaction.OutputMap
import arx.eldr.game.requirements.AuxDataDescriptor
import arx.eldr.game.requirements.EntityWithArchetypeDescriptor
import arx.engine.entity.TGameEntity

/**
  * TODO: Add javadoc
  */

case class GatherSaplingReaction protected[archetypes](species: PlantSpecies) extends Reaction {
	override def name: String = s"gather ${species.name} sapling"

	val target = InputKind.Target("Sapling")
	val outKind = OutputKind.Material("sticks")
	inputs = Map(target -> AuxDataDescriptor((PD: PlantData) => PD.ageCategory == AgeCategory.Tree.Sapling)
		.and(EntityWithArchetypeDescriptor(species)))
	outputs = Map(outKind -> ReactionOutput(species[TreeData].trunkMaterial,1))
	associatedSkills = List(Skill.PlantGathering)

	override def react(inputs: InputMap, tools: InputMap, location: VoxelCoord, actors: List[TGameEntity]): OutputMap = {
		val sapling = inputs.get(target).head

		val trunkMaterial = sapling[TreeData].trunkMaterial
		val wood = trunkMaterial.createMaterialBlock(1)

		MultiMap(outKind -> wood)
	}
}
