package arx.axistential.testbed

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/30/13
 * Time: 12:01 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.game.archetypes.Material
import arx.core.datastructures.Killable

class ConfiguredWidgetTestbed extends Conflux {
	val windowing = new WindowingSystem2()

	val widget = Widget.fromResource("test/smlwidgets/TestWidget.sml",null)

	val radio = widget.getById[RadioSelectionWidget]( "Radio Widget" )
	radio.configure[String]( List("Wall","Floor","Block") , (s,p) => {
		val w = ImageDisplayWidget( image(s"axis/ui/modes/images/Construct${s}Mode.png") , p )
		w.backgroundImage = image("ui/singlePixelBorder_ne.png")
		w.pixelScale = 1
		w
	})

	val scrollBarTest = widget.getById[Widget]( "Scroll Bar Test Widget" )

	val matList = widget.getById[ListWidget]("Material Selection Widget")
	matList.configure(Material.allArchetypes,(m:Material,p:Widget) => {
		val tdw = TextButton(m.name.capitalize,p)
		tdw.matchTextDimensions()
		tdw.textAlignment = Center

		tdw
	})
	
	windowing.addTopLevelWidget(widget)
	
	val graphWidget = new GraphWidget(None, () => List(Dataset(List(0.0f -> 0.5f, 0.25f -> 0.1f, 0.5f -> 0.3f,0.75f -> 0.4f,1.0f -> 2.0f),Color.Black)))
	graphWidget.width = 75.0f
	graphWidget.height = 75.0f
	graphWidget.x = (windowing.width - graphWidget.width) * 0.5f
	graphWidget.y = (windowing.height - graphWidget.height) * 0.5f
	graphWidget.z = 4.0f
	
	windowing.addTopLevelWidget(graphWidget)

	override def drawGL() = {
		super.drawGL ()

		windowing.draw()
	}

	var lastUpdated = 0l
	var first = true
	override def update(f: Float) = {
		super.update (f)

		graphWidget.watcher.last = 0

		windowing.update(f)
//		val file = ResourceManager.file("test/smlwidgets/TestWidget.sml")
//		val modified = file.lastModified()
//		if ( modified > lastUpdated || first ) {
//			lastUpdated = modified
//			widget.setFromSML( Hocon.parseResource("test/smlwidgets/TestWidget.sml").obj , first )
//			first = false
//		}
	}


	override def handleEvent(event: Event) = windowing.handleEvent(event)

	override def quit() = {
		Killable.kill(Killable.ApplicationLevel)
	}
}
