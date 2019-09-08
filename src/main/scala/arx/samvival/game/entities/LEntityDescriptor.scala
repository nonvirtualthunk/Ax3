package arx.samvival.game.entities

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/19/18
  * Time: 7:16 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._
import arx.engine.entity.Taxon
import arx.engine.lworld.{LEntity, LWorldView}
import arx.engine.requirement.Descriptor

trait LEntityDescriptor {
	def matches (entity : LEntity, worldView : LWorldView) : Boolean
}

case class IsA(taxon : Taxon) extends LEntityDescriptor {
	override def matches(entity: LEntity, worldView: LWorldView): Boolean = {
		worldView.dataOpt[IdentityData](entity).exists(ident => ident.taxons.exists(t => t.isA(taxon)))
	}
}