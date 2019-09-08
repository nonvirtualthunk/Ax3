package arx.samvival.control.widgets

import arx.core.Moddable
import arx.core.vec.{Cardinals, Vec2T, Vec2i, Vec4f}
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets._
import arx.engine.control.components.windowing.widgets.DimensionExpression.Proportional
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.data.Reduceable
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.lworld.{LEntity, LWorldView}
import arx.graphics.helpers._
import arx.samvival.game.entities.Fields.CharacterInfo
import arx.samvival.game.entities.{CharacterInfo, IdentityData}
import arx.samvival.game.logic.Identity
import arx.samvival.graphics.GameImageManager
import arx.samvival.graphics.components.CharacterGraphicsComponent
import arx.samvival.graphics.data.Selection

class SelectedCharacterInfo(parentis : Widget, character : LEntity)(implicit worldView : LWorldView, graphics : GraphicsWorld) extends Widget(parentis) {
	width = DimensionExpression.Proportional(1.0f)
	height = DimensionExpression.Constant(800)
	x = PositionExpression.Constant(0)
	y = PositionExpression.Constant(0)


	this.apply[DrawingData].withData(d => {
		d.backgroundImage = Some("ui/minimalistBorder_ne.png")
		d.interiorPadding = Vec2i(10,10)
	})


	def textDisplay(text : () => RichText) = {
		val ret = new TextDisplayWidget(this)
		ret.fontScale = 2.0f
		ret.text = Moddable(text)
		ret
	}

	def reduceableToRichText[T : Numeric](label : RichTextSection, reduceable : Reduceable[T]) : RichText = {
		val curColor = implicitly[Numeric[T]].compare(reduceable.currentValue, reduceable.maxValue) match {
			case 1 => Vec4f(0.1f,0.9f,0.2f,1.0f)
			case 0 => Vec4f(0.0f,0.0f,0.0f,1.0f)
			case -1 => Vec4f(0.5f,0.1f,0.1f,1.0f)
		}
		RichText(label :: TextSection(" : ") :: TextSection(s"${reduceable.currentValue}", curColor) :: TextSection(s" / ${reduceable.maxValue}") :: Nil)
	}

	val nameDisplay = textDisplay(() => { RichText(Identity.name(character)) })

	val icon = new ImageDisplayWidget(this)
	icon.image = Moddable(() => GameImageManager.imageForCharacter(character))
	icon.scalingStyle = ImageDisplayWidget.ActualSize(2.0f)
	icon[DrawingData].backgroundImage = Some("ui/greenMinorStyledBorder_ne.png")
	icon.x = PositionExpression.Constant(0, TopRight)

	val healthDisplay = textDisplay(() => reduceableToRichText(ImageSection("samvival/ui/heart_icon.png", 4.0f, Color.White), character(CharacterInfo).health))
	healthDisplay.y = PositionExpression.Relative(nameDisplay, 2, Cardinals.Down)

	val moveDisplay = textDisplay(() => reduceableToRichText(ImageSection("samvival/ui/feet_icon.png", 2.0f, Color.White), character(CharacterInfo).movePoints))
	moveDisplay.y = PositionExpression.Relative(healthDisplay, 2, Cardinals.Down)
}
