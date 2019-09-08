package arx.samvival.game.entities

import arx.engine.entity.Taxon


class Species(name_ : String, parentTaxons : List[Taxon]) extends Taxon(name_, parentTaxons) {
	def this(name_ : String, parentTaxon : Taxon) { this(name_, parentTaxon :: Nil) }
}

object Species {
	import Taxonomy.Species._

	case object Human extends Species("human", Humanoid)
	case object Elf extends Species("elf", Humanoid)
	case object MudMonster extends Species("mud monster", Monstrous)
}