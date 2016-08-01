package arx.eldr.game.archetypes.plants

import arx.core.traits.{ArxEnum, ArxEnumObject}

/**
  * The various ways in which a plant can grow (i.e. a tree grows differently than a vine does)
  */

object GrowingStyle extends ArxEnumObject[GrowingStyle] {
	val Tree = GrowingStyle("Tree")
	val Vine = GrowingStyle("Vine")
	val Plant = GrowingStyle("Plant")
	val Bush = GrowingStyle("Bush")
}

class GrowingStyle(nomen: String) extends ArxEnum(nomen) {}