package arx.samvival.game.logic

import arx.engine.lworld.{LEntity, LWorldView}
import arx.samvival.game.entities.IdentityData

object Identity {

	def name(entity : LEntity)(implicit view : LWorldView) : String = {
		entity.dataOpt[IdentityData] match {
			case Some(identity) => identity.name.getOrElse(identity.taxons.headOption.map(t => t.name).getOrElse("nameless"))
			case None => "nameless"
		}
	}
}
