package arx.samvival.game.actions

import arx.ai.search.Path
import arx.core.vec.coordinates.AxialVec3
import arx.engine.entity.Taxon
import arx.engine.lworld.LEntity
import arx.samvival.game.entities.AttackData
import arx.samvival.game.entities.Taxonomy.Actions

abstract class GameAction {
	def identity : Taxon
}


case class MoveAction(character : LEntity, path : Path[AxialVec3]) extends GameAction {
	val identity : Taxon = Actions.MoveAction
}

case class AttackAction(attacker : LEntity, from : AxialVec3, targets : Set[LEntity], weapon : Option[LEntity], attack : LEntity) extends GameAction {
	val identity : Taxon = Actions.AttackAction
}

case class SwitchActiveCharacter(character : LEntity) extends GameAction {
	val identity : Taxon = Actions.SwitchActiveCharacterAction
}