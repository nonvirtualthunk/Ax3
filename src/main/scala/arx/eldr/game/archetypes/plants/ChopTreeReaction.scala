package arx.eldr.game.archetypes.plants

import arx.core.datastructures.MultiMap
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.archetypes._
import arx.eldr.game.archetypes.Reaction.InputMap
import arx.eldr.game.archetypes.Reaction.OutputMap
import arx.eldr.game.entity.data.ItemFlag
import arx.eldr.game.requirements._
import arx.engine.entity.TGameEntity

/**
  * TODO: Add javadoc
  */

object ChopTreeReaction extends BaseReaction {
	override def name: String = s"chop down tree"
	val target = InputKind.Target("Tree")
	val outKind = OutputKind.Material("logs")
	val axeKind = InputKind.Tool("Axe")
	val axeReq : EntityDescriptor = ItemWithFlagDescriptor(ItemFlag.DurableEdge)

	val maturePlantReq = AuxDataDescriptor((PD: PlantData) => PD.ageCategory.isMature)
	val treeReq = AuxDataDescriptor[TreeData]
	// This can create a variant for any individual entity (not archetype) that is a mature plant and a tree
	variantRequirements = Map(target -> (treeReq and maturePlantReq and IndividualEntityDescriptor))
	override def variantFor(variantInputs: Map[InputKind, TGameEntity]): Reaction = {
		val targetTree = variantInputs(target)
		val speciesName = targetTree[PlantData].species.name
		// create a reaction for the specific tree in question
		val variant = createVariant(s"chop down ${speciesName} tree",specific = true)

		val TD = targetTree[TreeData]

		val baseAxe = variant.tools(axeKind)
		// add in any additional requirements for cutting down this particular tree
		val effectiveAxeReq = TD.choppingToolRequirements.foldLeft(axeReq)(_ and _)


		variant.tools += axeKind -> effectiveAxeReq
		variant.outputs += outKind -> ReactionOutput(TD.trunkMaterial, TD.sticks.toInt + TD.trunkSize.toInt)

		variant
	}
	def apply(ent : TGameEntity) : Reaction = this.variantFor(Map(target -> ent))

	inputs = Map(target -> (maturePlantReq and treeReq and IndividualEntityDescriptor))
	outputs = Map(outKind -> ReactionOutput(Material.withName("wood"), 1))
	associatedSkills = List(Skill.WoodCutting)
	tools = Map(axeKind -> axeReq)

	override def react(inputs: InputMap, tools: InputMap, location: VoxelCoord, actors: List[TGameEntity]): OutputMap = {
		val tree = inputs.get(target).head

		val trunkMaterial = tree[TreeData].trunkMaterial

		val outputs = new MultiMap[OutputKind,TGameEntity]
		for (i <- 0 until tree[TreeData].trunkSize.toInt + tree[TreeData].sticks.toInt) {
			val wood = trunkMaterial.createMaterialBlock()
			outputs.add(outKind,wood)
		}

		outputs
	}
}


// The modeling problem we have here is that we want to be able to say, down the line, tree X can only
// be cut down with a steel axe, because its wood is so strong, but the tools requirement is fixed, it
// doesn't depend on the inputs. Well, I think the thing to do is to represent that in the tree data
// itself, nope, remember, don't have the inputs. To do that we would need to have two separate reactions
// one for trees that don't have an extra requirement, and one for trees that do. That's achievable
// but feels messy. Ok, I think we do actually go back to the reaction-per-species, but we need a
// concept that ties them all together, they're all basically the same. Ah. What if an individual tree
// gets modified, say, growing in a weird magical zone that turns its wood to stone or whatever, how
// is that handled? It's no longer a question of the species, it's a question of the individual entity
// in question. So. Secondary requirements may be dependent on the choice of inputs. But choice of
// inputs is not necessarily known ahead of time!
// Aside, how often and how universally to we expect this to come up? I can definitely see various
// plants that can only be gathered with certain tools, which vary from plant to plant, particularly
// for the more exotic ones (moonberries can only be harvested with a silver shears on a full moon).
// Item creation, probably less so, though it will still come into play. Crafting a mithril sword
// might well require a different location and tools than a tin one, would those be separate reactions?
// Part of what we were going for is that they would not be.
// Pretending that we could implement the AI side without too much trouble (i.e. first choose inputs
// then choose tools/location/etc) we could probably make it work, _tools_ would take in the InputMap
// same as react, and would return the requirements for those inputs, likewise actors, location, etc.
// This runs into problems as well though, you really need to know the actor requirements before you
// even assign the goal to someone, if you dictate that a sword should be made, and the untrained smith
// goes and grabs a bar of unobtainium, he shouldn't even be able to try that. Or a certain tree can
// only be harvested by good aligned creatures, you dictate a general Gather goal, etc.
// Proposed alternative. Defaults that apply in the common case. Here that would be something like saying
// that the input is `(mature tree) and (no additional harvest restrictions)`. Then we provide the
// ability to make variants on an existing reaction. Those would tweak the inputs/tools/etc, but leave
// the reaction logic intact. For extremely complex cases you're really talking about an entirely
// separate reaction, but with a conceptual relation, for that we should include a field indicating
// sub-reaction of X
// OK, I think we may still end up using the variant thing at some point, but for this it'll just be
// one reaction per
// +=================================================+
// Upon further, further consideration, it seems that the reaction will need to know about the individual
// entity inputs it has in order to support the full range of possibilities. In this case, that would
// mean, potentially, a different reaction for every tree, _or_ an entire system capable of handling
// tools/locations/agents as a function of the inputs chosen. I think it makes the most sense in that
// case to not provide a general ChopTreeReaction that's available in the reaction dictionary. Instead
// reactions can be produced on demand as a result of specifying individual trees/plants for harvesting.
// The reactions in the dictionary will be those that do not depend on the individual entity in question.
// If we go that route, if we have, say, craft sword and we want mithral to only be forgeable in a
// specific kind of forge, we will need a separate reaction for mithral swords than we use for others.
// BAH. It would be so much more flexible if the inputs were known when determining the tools and
// such. To do that though, we will either need another step in the AI process, where it re-checks to
// determine if other prerequisites are needed. If we did prereqs lazily (like planning), it could work.
// The reaction goal would create three subgoals, acquire inputs, acquire tools, move to location. The
// latter two goals would only be sub-goal'd and planned after inputs were successfully acquired. The
// danger in this approach is that you might gather all the materials, then realize that you (the agent)
// lack sufficient skill to work them.
// Major option N: require the goal to have all inputs chosen at start. That way determining which
// agent would be valid is possible. This requires more UI intervention, no partially applied recipes,
// one cannot simply say: "Make a sword" without risking failure. If the goal chooses mithral, and none
// of your dwarves have the skill to craft mithral, it will fail and, unless some cleverness is used,
// it won't try again with different materials. It's a challenge if the details are not known ahead
// of time. But, if we assume make a sword is not a hugely valuable functionality, which it probably
// isn't (you generally want to know what you're making it out of) then that's all pretty much ok.
// We just make a different reaction for "Forge a Mithral Sword" and "Forge an Iron Sword". What if
// we codified that as a _sigh_ ReactionFactory sword of thing. Give it an entity or an archetype
// of the appropriate kind, and it will give you a reaction.