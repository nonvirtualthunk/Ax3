package arx.tmp.game.logic.descriptors

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/17/13
 * Time: 8:00 PM
 * Created by nonvirtualthunk
 */

import arx.tmp.game.logic.entities.Player
import arx.tmp.game.logic.entities.core.GameEntity

object SelectorLogic extends TSelectorLogic {
	def populateSelectorResults(player: Player, env: World, selector: Selector): SelectorResult = {
		val res = SelectorResult(Map(),selector)
		populateSelectorResults(player,env,res)
		res
	}

	def populateSelectorResults ( player : Player , env : World, selResults : SelectorResult ) : Boolean = {
		var usedEntities = Set[GameEntity]()
		var results = Map[String,Serializable]()

		selResults.unselected.foreach { case (key,descriptor) => {
			if ( descriptor.unambiguousDescriptor ) {
				results += key -> descriptor.exampleMatch
			} else {
				env.entities.filter( e => ! usedEntities.contains(e) && descriptor.matches(e) ).toList match {
					case one :: Nil =>
						usedEntities += one
						results += key -> one
					case _ =>
				}
			}
		}}


		selResults.inputs ++= results
		results.nonEmpty
	}

	def possibleMatches(player: Player, env: World, descriptor: TDescriptor): List[Any] = {
		env.entities.filter( e => descriptor.matches(e) ).toList
		//::: env.auxData[Board].allTiles.filter( t => descriptor.matches(t) ).toList
	}
}

trait TSelectorLogic {
	def populateSelectorResults ( player : Player , env : World, selector : Selector ) : SelectorResult
	def populateSelectorResults ( player : Player , env : World, results : SelectorResult ) : Boolean

	def possibleMatches ( player : Player , env : World, descriptor : TDescriptor ) : List[Any]
}