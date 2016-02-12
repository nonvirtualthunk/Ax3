package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/8/13
 * Time: 12:43 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.Recipe.Output
import arx.axistential.game.archetypes._
import arx.axistential.game.archetypes.item.ConfiguredItemArchetype
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.item.SimpleItemArchetype
import arx.axistential.game.archetypes.sml.ConfiguredRecipe
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.data.entity.plants.PlantSeedContainerData
import arx.axistential.game.data.entity.plants.PlantSeedData
import arx.axistential.game.data.entity.FoodData
import arx.axistential.game.data.entity.PlantKind
import arx.axistential.game.data.world.BiomeDescriptor
import arx.axistential.game.data.world.Season
import arx.axistential.game.entities.traits.SimplePhysicalEntity
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.core.function.SimpleExpression
import arx.core.representation._
import arx.core.units.UnitOfMeasure
import arx.tmp.game.logic.descriptors.EntityWithFlagDescriptor
import arx.tmp.game.logic.descriptors.EntityWithoutFlagDescriptor
import arx.tmp.game.logic.entities.core.TSanityCheckable.Assert
import arx.tmp.game.logic.entities.data.EntityFlag
import arx.tmp.game.logic.entities.data.FlagData
import arx.requirements.NoRequirement
import arx.requirements.OneEntityRequirement

class ConfiguredPlantSpecies(providedName : String, initialSML:ConfigValue) extends PlantSpecies with TConfiguredSpecies {
	def this (iniSML : ConfigValue) { this(iniSML.name.strOrElse("unknown name"),iniSML) }
	name = providedName
	var sml = initialSML
	val seedlingCollisionShape = new CubeCollisionShape(10.cm x 10.cm x 40.cm)
	

	def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		if (overwrite) { _auxData.clear() }
		applyConfigurableAuxData(sml,this)

		name = sml.name.strOrElse(name)

		lightUsage = PlantSpecies.LightUsage.fromSML( sml )
		waterUsage = PlantSpecies.WaterUsage.fromSML( sml )
		nutrientUsage = PlantSpecies.NutrientUsage.fromSML( sml )

		leafRadius = UnitOfMeasure.parseUnitOfDistance(sml.leafRadius.strOrElse("0.5 m"))
		rootRadius = UnitOfMeasure.parseUnitOfDistance(sml.rootRadius.strOrElse("0.5 m"))

		biomeDescriptor = BiomeDescriptor.fromSML( sml.biome )

		if ( sml.spread.nonEmpty ) {
			spreadChance = sml.spread.chance.floatOrElse(1.0f)
			spreadFalloff = sml.spread.falloff.floatOrElse(0.5f)
			spreadNumber = sml.spread.number.intOrElse(1)
		}

		for ( assocSML <- sml.associatedWith.arr ) {
			val assoc = new PlantAssociation
			assoc.setFromSML(assocSML)
			associatedWith ::= assoc
		}

		for ( product <- sml.products.arr ) {
			val arch = if ( product.item.nonEmpty ) {
				if ( product.item.isObj ) {
					ItemArchetype.registerArchetype(product.item.name.str, new ConfiguredItemArchetype(product.item))
				} else {
					ItemArchetype.archetypeWithName(product.item.str)
				}
			} else if ( product.material.nonEmpty ) {
				if ( product.material.isObj ) {
					Noto.warn("We don't want to deal with inline material definitions right this second")
					Material.Sentinel
				} else {
					Material.withName(product.material.str)
				}
			} else {
				ItemArchetype.registerArchetype(product.name.str, new ConfiguredItemArchetype(product))
			}

			products ::= LivingThingProduct.fromSML(product,this,Some(arch))
		}

		plantKind = PlantKind(sml.kind.strOrElse("plant"))
		plantKind match {
			case PlantKind.Tree => {
				val woodSML = sml.wood
				val woodName = woodSML.name.strOrElse(name + " wood")
				val woodMaterial = Material.getOrElseCreate(woodName, {
					val wood = CommonMaterials.Wood.copy
					wood.name = woodName //i.e. Oak Wood
					wood.strength = wood.strength + woodSML.strength.floatOrElse(0.0f)

					wood
				})

				// Create the twig archetype
				val twigArchetype = SimpleItemArchetype(name + " twig",4.cm x 1.cm x 1.cm,woodMaterial,Set(),Some("twig"))
				twigArchetype.aux[FlagData].flags += EntityFlag("Twig")
				twigArchetype.aux[FoodData].calories = 100.0f
				ItemArchetype.registerArchetype(twigArchetype.name, twigArchetype)

				// Create the leaf archetype
				val leafArchetype = SimpleItemArchetype(name + " leaf",8.cm x 4.cm x 4.cm,Material.withName("plant flesh"),Set(),Some("leaf"))
				leafArchetype.aux[FlagData].flags += EntityFlag("Leaf")
				leafArchetype.aux[FoodData].calories = 100.0f
				ItemArchetype.registerArchetype(twigArchetype.name, leafArchetype)

				// Create products
				val woodGrowthRate = woodSML.growthRate.floatOrElse(1.0f).max(0.0001f)
				val baseGrowthInterval = 1.turning
				val trunkGrowthInterval = baseGrowthInterval * (1.0f / woodGrowthRate)
				
				val trunkTool = OneEntityRequirement(EntityWithFlagDescriptor(Set(EntityFlag.DurableEdge)))
				val branchTool = OneEntityRequirement(EntityWithFlagDescriptor(Set(EntityFlag.Edge)))
				
				// trunk, initial
				// TODO: Make amount of trunk at each age category variable, dependent on size, species, etc
				val matureIndex = plantKind.matureAgeCategoryIndex
				val preMaturePostPlantingCategories = plantKind.preMatureAgeCategories.drop(1)


				val deciduous = sml.deciduous.boolOrElse(orElse = true)
				if (deciduous) { this.aux[FlagData].flags += EntityFlag("Deciduous") }
				val growthSeasons = if(!deciduous) { Season.AllYear } else { Set(Season.Spring,Season.Summer) }

				// leaf/twig growth, increases up until maturity, then slows down
				for ((ac,index) <- plantKind.ageCategories.zipWithIndex.drop(1)) {
					val amount : Float = if (index <= matureIndex) { index }
												else { (matureIndex - (index - matureIndex)).max(1) }

					// twigs
					products ::= new RecurringLivingThingProduct(twigArchetype,growthSeasons,ac,amount,1.cycle,false,NoRequirement,Some(10))
					// leaves
					products ::= new RecurringLivingThingProduct(leafArchetype,growthSeasons,ac,amount,1.cycle,false,NoRequirement,Some(10))
					if (index >= matureIndex) {
						// trunk
						products ::= new RecurringLivingThingProduct(woodMaterial,Season.AllYear,ac,1.0f,trunkGrowthInterval,true,trunkTool)
						// branches, less wood but not structural and easier to harvest
						products ::= new RecurringLivingThingProduct(woodMaterial,Season.AllYear,ac,1.0f,trunkGrowthInterval * 2.0f,false,branchTool)
					}
				}

				// sapling trunk, each age category up until the point of maturity
				for (ac <- preMaturePostPlantingCategories) {
					products ::= new LivingThingProduct(woodMaterial,true,ac,1.0f,branchTool)
				}
				// main trunk, at maturity
				products ::= new LivingThingProduct(woodMaterial,true,plantKind.matureAgeCategory,3.0f,trunkTool)

				// by default, we assume that sentient creatures harvesting trees will not want the twigs or leaves, those
				// are only valuable to animals that eat them
				val excludeFromHarvest = new EntityWithoutFlagDescriptor(Set(EntityFlag("Twig"),EntityFlag("Leaf")))
				defaultHarvestDescriptorBySpecies = defaultHarvestDescriptorBySpecies.withDefaultValue(excludeFromHarvest)
			}
			case PlantKind.Fern => {

			}
			case PlantKind.Mushroom => {
				val capSML = sml.cap
				if ( ! capSML.isEmpty ) {
					val capArch = ItemArchetype.registerArchetype(capSML.name.str, new ConfiguredItemArchetype(capSML))
					products ::= new LivingThingProduct(capArch,true,plantKind.matureAgeCategory,capSML.number.expressionOrElse(1.0f),NoRequirement)
				}
			}
			case PlantKind.Vegetable => {
				if (sml.hasField("root")) {
					val rootArch = ItemArchetype.registerArchetype(sml.field("root").name.str, new ConfiguredItemArchetype(sml.field("root")))

					products ::= new LivingThingProduct(rootArch,true,plantKind.matureAgeCategory,sml.root.number.expressionOrElse(1.0f),NoRequirement)
				}
			}
			case PlantKind.Plant => {

			}
			case _ =>
		}

		plantKind match {
			case PlantKind.Tree | PlantKind.Fern | PlantKind.Mushroom | PlantKind.Vegetable | PlantKind.Plant | PlantKind.Groundcover => {
				val seedSML = sml.seed
				val seedContainerSML = sml.seedContainer
				if ( seedSML.nonEmpty ) {
					val seedArch = new ConfiguredItemArchetype(seedSML)
					val seedData = seedArch.aux[PlantSeedData]
					seedData.forSpecies = this
					seedData.travelDistance = UnitOfMeasure.parseUnitOfDistance( seedSML.travelDistance.strOrElse("1m") )
//					seedArch.subSeeds = seedSML.subSeeds.floatOrExpressionOrElse(0.0f)
					ItemArchetype.registerArchetype(seedArch.name, seedArch)

					products ::= LivingThingProduct.fromSML(seedSML,this,Some(seedArch))
				}
				if ( seedContainerSML.nonEmpty ) {
					val seedContainerArch = ItemArchetype.registerArchetype( seedContainerSML.name.str, new ConfiguredItemArchetype(seedContainerSML) )
					if ( ! seedContainerArch.hasAuxData[PlantSeedContainerData] ) {Noto.warn("Seed container didn't pick up its aux data") }
					if ( seedContainerSML.separationProcess.nonEmpty ) {
						val outputs = seedContainerArch.aux[PlantSeedContainerData].seedParts.map( p => Output(p.archetype,1.0f,new SimpleExpression(p.number) ) )
						val sepName = s"Separate ${seedContainerArch.name}"
						Recipe.registerRecipe(new ConfiguredRecipe(sepName,seedContainerSML.separationProcess,List(seedContainerArch),outputs))
					}

					products ::= LivingThingProduct.fromSML(seedContainerSML,this,Some(seedContainerArch))
				}

				if ( seedSML.isEmpty && seedContainerSML.isEmpty ) {
					Noto.warn("plant must provide a valid seed in sml")
				}

				sml.cutting match {
					case ConfigValue.Sentinel => //this does not have cuttings, carry on
					case cuttingSML => {
						val cuttingArch = new ConfiguredItemArchetype(cuttingSML)
						val seedData = cuttingArch.aux[PlantSeedData]
						seedData.forSpecies = this
						seedData.travelDistance = zeroMeters
						ItemArchetype.registerArchetype(cuttingArch.name, cuttingArch)

						products ::= LivingThingProduct.fromSML(cuttingSML,this,Some(cuttingArch))
					}
				}
			}
			case _ => Noto.warn(s"unknown plant kind : $plantKind")
		}

		parseAgeCategoryStartTimes(sml)

		parseAgeCategoryCollisionShapes(sml)

		parseAgeCategoryInformation(sml)
	}


	protected def createPhysicalInstance: TPhysicalEntityArchetype.PhysicalArchetypedEntity = {
		val plant = new SimplePhysicalEntity
		plant.dynamic = false
//		plant.collisionShape = parseCollisionShape(sml,defaultShape = "box")

		// all seedlings start out small
		plant.collisionShape = collisionShapesByAgeCategory.getOrElse(plantKind.ageCategories.head,seedlingCollisionShape)

		plant.aux[PlantData].ageCategory = plantKind.ageCategories.head

		plant
	}


	override def sanityChecks: List[Assert] =
		Assert(collisionShapesByAgeCategory.size == plantKind.ageCategories.size,"Some age categories have no collision shape") ::
			super.sanityChecks


	setFromSML(sml,overwrite = true)
}







case class SeedPart ( archetype : TPhysicalEntityArchetype , number : Int )











object ConfiguredPlantSpecies {

}