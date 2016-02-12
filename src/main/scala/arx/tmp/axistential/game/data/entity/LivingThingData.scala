package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/30/14
 * Time: 4:18 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.species.RecurringLivingThingProduct
import arx.axistential.game.archetypes.species.TSpeciesKindCollector
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.requirements.ConfiguredRequirement
import arx.core.representation.ConfigValue
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.units.UnitOfTime
import arx.engine.game.GameEngine
import arx.requirements.TRequirement


/** This is not a TGameEntityAuxData, because currently .aux[] does not handle base classes properly. This allows sharing
  * of common data, but cannot be used as an argument to .aux[]
  */
abstract class LivingThingData {
	var born = 0.seconds
	var ageCategory : AgeCategory = LivingThingData.DefaultAgeCategory
	var productsLastProduced = Map[RecurringLivingThingProduct,UnitOfTime]()
	var parts : List[LivingThingPart] = Nil
	def nonStructuralParts = parts.filterNot(_.structural)
	def structuralParts = parts.filter(_.structural)



	def addPart (part : TPhysicalEntity, structural : Boolean, harvestableBy : TRequirement): Unit = {
		parts ::= LivingThingPart(part,structural,harvestableBy)
	}

	def age = GameEngine.time - born

}
object LivingThingData {
	val DefaultAgeCategory = AgeCategory("unassigned","unassigned",None)
}

case class LivingThingPart (entity : TPhysicalEntity, structural : Boolean, harvestableBy : TRequirement) {

}

trait SpeciesKind {
	var ageCategories = List[AgeCategory]()
	var matureAgeCategory : AgeCategory = AgeCategory("sentinel","sentinel",None)
	var defaultAgeAfterMaturityMultiplier : Float = 2.0f
	def ageCategoryWithName(str : String) = ageCategories.find(_.name.toLowerCase == str.toLowerCase) match {
		case Some(ac) => ac
		case None => Noto.warn(s"No age category named $str in plant kind $this"); ageCategories.last
	}

	def matureAgeCategoryIndex = ageCategories.indexOf(matureAgeCategory)
	def preMatureAgeCategories = ageCategories.takeWhile(! _.maturityAgeCategory)
	def matureOrAfterAgeCategories = ageCategories.slice(matureAgeCategoryIndex,ageCategories.size)
}

class PlantKind (name : String) extends ArxEnum(name) with SpeciesKind {
	var harvestToolByAgeCategory = Map[AgeCategory,TRequirement]()
}
object PlantKind extends ArxEnumObject[PlantKind] with TSpeciesKindCollector[PlantKind] {
	override def parseKindAgeCategory(conf: ConfigValue, kind: PlantKind, ac: AgeCategory): Unit = {
		if (conf.harvestTool.nonEmptyValue) {
			kind.harvestToolByAgeCategory += ac -> ConfiguredRequirement.fromSML(conf.harvestTool)
		}
	}
	override def confLocation: String = "axis/entities/plants/PlantKinds.sml"
	override def kindFromName(name: String): PlantKind = PlantKind(name)

	val Vegetable = PlantKind("Vegetable")
	val Tree = PlantKind("Tree")
	val Mushroom = PlantKind("Fungus")
	val Fern = PlantKind("Fern")
	val Plant = PlantKind("Plant")
	val Groundcover = PlantKind("Groundcover")
}

case class AgeCategory(name : String, adjective : String, var followedBy : Option[AgeCategory]) {
	var maturityAgeCategory = false

	override def toString: String = s"AgeCategory($name)"
}