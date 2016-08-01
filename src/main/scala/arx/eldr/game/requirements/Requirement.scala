package arx.eldr.game.requirements

import arx.core.introspection.CopyAssistant

/**
 * TODO: Add javadoc
 */

trait Requirement {
	var amount = 1
	def amountSatisfiedBy (entity : Any) : Int

	def isSatisfiedBy (entity : Any) : Boolean = {
		isSatisfiedBy(List(entity))
	}
	def isSatisfiedBy (entity : List[Any]) : Boolean = {
		val amountSatisfied = entity.map(amountSatisfiedBy).sum
		amountSatisfied >= amount
	}

	def copyWithAmount(newAmount : Int) = {
		val ret = CopyAssistant.copy(this)
		ret.amount = newAmount
		ret
	}
}
