package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/11/15
 * Time: 7:59 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.core.traits.ArxEnum
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TInheritableAuxData

class FoodData extends TInheritableAuxData with TConfigurableGameEntityAuxData {
	var calories : Float = 1.0f

	def createFromSML(sml: ConfigValue): Option[FoodData] = if ( sml.calories.nonEmpty ) {
		val fd = new FoodData
		fd.calories = sml.calories.float
		Some(fd)
	} else {
		None
	}
}


// prepared food
// grass, foliage, bark, twigs, roots, fruit, fungus
// muscle, fat, offal, skin, bone

/*
So we could have this implicitly attached to the relevant things, assuming that food objects
other than prepared food would never spontaneously occur and always be created by either a
plant or an animal.

Trees would generate foliage, bark, twigs, roots and seed/fruit
Vegetables would generate foliage, roots and seed/fruit
Fungus would generate only... fungus, separate classification
and so on

This would mean that we would have to actually generate a "twigs" object in the tree, and "bark"
as well. That's not a bad thing, mind you, my only concern as that all these items could become
overwhelming. If you harvest a tree and you end up with a hundred different entities, that could
be a bit much. It might be a good idea to put in some basic filtering, twigs for example could
be non-harvested by default. Bark has a number of uses, per wikipedia[1] though, so it shouldn't
be filtered. Instead, we could invest in strong filtering at the inventory level, doing aggressive
grouping, so that we could show : { Bark : 10, Wood : 43, Leaves : 100 } that could in turn drill
down into : { Willow Bark : 4, Oak Bark : 2, Birch Bark : 4 } and so on. On this specific case,
I think leaves could probably also be marked as non-harvestable, I don't foresee any particular
use for them, other than animals eating them. For vegetables though...the leaf of spinach, cabbage
etc are all very much edible. So that could be a case-by-case.




[1][	Source of tannins for tanning leather, construction of backpacks/canoes (birch), broken down
		into fibers and made into rope, cork, cinnamon, quinine, asprin, mulch, shingles, writing surface,
		resin, and hallucinagenics ]

 */

class FoodKind(nomen:String) extends ArxEnum(nomen) {

}
object FoodKind {
	val Grass = new FoodKind("grass")
	val Foliage = new FoodKind("grass")
	val Bark = new FoodKind("grass")
}

object FoodData {
	val EdibleRaw = 10
	val EdibleCooked = 7
	val EdibleByLivestock = 5
	val Inedible = 0

	@deprecated
	def parseEdibility ( str : String ) = str.toLowerCase.stripWhitespace match {
		case "edibleraw" => EdibleRaw
		case "ediblecooked" => EdibleCooked
		case "ediblebylivestock" => EdibleByLivestock
		case "inedible" => Inedible
		case _ => Noto.warn("Unknown edibility value " + str); EdibleRaw
	}
}