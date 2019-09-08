package arx.samvival.control.widgets

import arx.Prelude.toArxList
import arx.core.vec.Cardinals
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.control.components.windowing.widgets.{DimensionExpression, DynamicWidget, PositionExpression, TopRight}
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.lworld.{LEntity, LWorldView}
import arx.samvival.game.logic.Attacks
import arx.samvival.graphics.data.Selection


class SideBar(desktop : Widget)(implicit view : LWorldView, graphics : GraphicsWorld) extends DynamicWidget[LEntity](desktop) {
	this.width = DimensionExpression.Constant(600)
	this.height = DimensionExpression.Proportional(1.0f)
	this.x = PositionExpression.Constant(0, TopRight)

	this.apply[DrawingData].withData { o =>
		o.backgroundImage = Some("ui/minimalistBorder_ne.png")
	}

	override def currentContent: LEntity = graphics[Selection].selectedCharacters.headOption.getOrElse(LEntity.Sentinel)

	override def generateWidgets(current: LEntity): List[Widget] = if (current == LEntity.Sentinel) { Nil } else {
		val selectedCharInfo = new SelectedCharacterInfo(this, current)

		val possibleAttacks = Attacks.availableAttacks(current)
		val attackSelection = new AttackSelectionWidget(this, current, possibleAttacks.head._2.head)

		selectedCharInfo :: attackSelection :: Nil
	}

	override def positionWidgets(widgets: List[Widget]): Unit = {
		widgets.sliding2.foreach {
			case (a,b) => b.y = PositionExpression.Relative(a, 1, Cardinals.Down)
		}
	}
}
