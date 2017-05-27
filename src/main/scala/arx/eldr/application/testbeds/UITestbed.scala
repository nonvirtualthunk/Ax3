package arx.eldr.application.testbeds

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.engine.advanced.Engine
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.WindowingControlComponent
import arx.engine.control.components.windowing.widgets.DimensionExpression
import arx.engine.control.components.windowing.widgets.TextDisplayWidget
import arx.engine.control.data.WindowingData
import arx.engine.graphics.components.WindowingGraphicsComponent

import scalaxy.loops._

object UITestbed extends Engine {
	override def setUpEngine(): Unit = {
		graphicsEngine.addComponent[WindowingGraphicsComponent]

		controlEngine.addComponent[WindowingControlComponent]

		val WD = controlEngine.controlWorld.aux[WindowingData]

//		val widget = new Widget(WD.desktop)
//		widget.width = DimensionExpression.Proportional(0.5f)
//		widget.height = DimensionExpression.Proportional(0.5f)

		val textWidget = new TextDisplayWidget(WD.desktop)
		textWidget.width = DimensionExpression.Proportional(0.5f)
		textWidget.height = DimensionExpression.Proportional(0.5f)
		textWidget.drawing.drawBackground = false
		textWidget.fontScale = 4
		textWidget.text = Moddable("Hello, World!")
	}
}
