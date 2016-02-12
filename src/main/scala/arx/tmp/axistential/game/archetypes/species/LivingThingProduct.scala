package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/15
 * Time: 9:02 AM
 */

import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.data.world.Season
import arx.axistential.game.logic.requirements.ConfiguredRequirement
import arx.core.Moddable
import arx.core.representation.ConfigValue
import arx.core.units.UnitOfMeasure
import arx.requirements.TRequirement

object LivingThingProduct {
	 def fromSML ( baseSML : ConfigValue, species : Species, productArchetype : Option[TPhysicalEntityArchetype] = None ) = {
		 val sml = if ( baseSML.production.nonEmpty ) {
			 baseSML.production
		 } else { baseSML }

		 val arch = productArchetype match {
			 case Some(a) => a
			 case None => ItemArchetype.archetypeWithName(sml.kind.str)
		 }
		 val structural = sml.structural.boolOrElse(false)
		 val number = sml.number.expressionOrElse(1)

		 val harvestableBy = if (sml.harvestTool.nonEmptyValue) {
			 ConfiguredRequirement.fromSML(sml.harvestTool)
		 } else {
			 TRequirement.Sentinel
		 }

		 val atAgeCategory = sml.ageCategory match {
			 case ConfigValue.Sentinel => species.speciesKind.matureAgeCategory
			 case other => species.speciesKind.ageCategoryWithName(other.str)
		 }

		 val recurring = sml.interval.nonEmpty
		 if ( recurring ) {
			 val interval = UnitOfMeasure.parseUnitOfGameTime(sml.interval.str)
			 val seasonsRaw = extractSingularOrPlural(sml,"season","seasons")
			 val seasons = if ( seasonsRaw.isEmpty ) { Season.AllYear }
								 else { seasonsRaw.map( raw => Season.fromString( raw.str ) ).toSet }

			 new RecurringLivingThingProduct(arch,seasons,atAgeCategory,number,interval,structural,harvestableBy)
		 } else {
			 new LivingThingProduct(arch,structural,atAgeCategory,number,harvestableBy)
		 }
	 }
 }

class LivingThingProduct(
	 val archetype : TPhysicalEntityArchetype ,
	 val structural : Boolean ,
	 val atAgeCategory : AgeCategory,
	 val number : Moddable[Float],
	 val harvestableBy : TRequirement
 )