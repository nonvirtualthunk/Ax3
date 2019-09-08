package arx.samvival.control.widgets

import arx.core.Moddable
import arx.core.vec.{Vec2f, Vec2i}
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.{DimensionExpression, PositionExpression, TextDisplayWidget}
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.lworld.{LEntity, LWorldView}
import arx.graphics.helpers.{Color, ImageSection, RichText, TextSection}
import arx.samvival.game.entities.Fields.{AttackData, IdentityData}
import arx.samvival.game.entities.{CombatData, Weapon}
import arx.samvival.game.logic.{Attacks, Identity}
import arx.core.math.Sext
import arx.samvival.graphics.GameImageManager

class AttackSelectionWidget(parentis : Widget, character : LEntity, attack : LEntity)(implicit view : LWorldView) extends Widget(parentis) {
	val attackName = view.worldCached(Identity.name(attack))
	val weapon = view.worldCached(attack(AttackData).weapon)

	val weaponBreakdown = view.worldCached(view.dataModificationLog[Weapon](weapon))
	val combatBreakdown = view.worldCached(view.dataModificationLog[CombatData](character))

	val strikeBreakdown = view.worldCached(Attacks.baseStrikeProspect(character, attack, combatBreakdown, weaponBreakdown))

	{
		val DD = this.apply[DrawingData]
		DD.backgroundImage = Some("ui/minimalistBorderWhite_ne.png")
		DD.interiorPadding = Vec2i(10,10)
	}
	width = DimensionExpression.Proportional(1.0f)
	height = DimensionExpression.WrapContent


	val nameDisplay = new TextDisplayWidget(this)
	nameDisplay.fontScale = 2.0f
	nameDisplay.text = Moddable(() => RichText(attackName.resolve().capitalize))

	val accuracyDisplay = new TextDisplayWidget(this)
	accuracyDisplay.fontScale = 2.0f
	accuracyDisplay.text = Moddable(() => RichText(s"${strikeBreakdown.accuracy.total.toSignedString}"))
	accuracyDisplay.x = PositionExpression.Relative(nameDisplay, 20)

	val damageDisplay = new TextDisplayWidget(this)
	damageDisplay.fontScale = 2.0f
	damageDisplay.x = PositionExpression.Relative(accuracyDisplay, 20)
	damageDisplay.text = Moddable(() => damageText)



	def damageText = {
		val allSections = strikeBreakdown.damageElements.total.flatMap(de => {
			val bonusStr = strikeBreakdown.damageBonuses.total.getOrElse(de.damageType, Sext(0)).asInt match {
				case 0 => ""
				case i if i > 0 => s" +$i"
				case i if i < 0 => s" -$i"
			}

			TextSection(s"${de.damageDice}${bonusStr}") :: ImageSection(GameImageManager.imageForDamageType(de.damageType), 1.0f, Color.White) :: Nil
		})

		RichText(allSections)
	}

}
