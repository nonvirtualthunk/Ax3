package arx.axistential.testbed.ai.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/5/13
 * Time: 4:30 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.testbed.ai.logic.DigWorld
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.graphics.AVBO

class DigDisplayWidget(world : DigWorld,parentis:Widget) extends Widget(parentis) {
	val range = 30

	renderers :+= new WidgetRenderingComponent {
		def draw(widget: Widget, vbo: AVBO, context: RenderingContext, beforeChildren: Boolean) {
			val tc = context.textureBlock( image("ui/selectedTileOverlay.png") )

			val ww = context.toPixelScaleX(widget.width)
			val wh = context.toPixelScaleY(widget.height)

			val tw = ww / (range*2+1)
			val th = wh / (range*2+1)

			for ( dx <- -range to range ; dy <- -range to range ) {
				val xp = (dx + range) / (range * 2.0f + 1)
				val yp = (dy + range) / (range * 2.0f + 1)

				var depth = 0
				while ( ! world.terrain.isSolid(VoxelCoord.Center + Vec3i(dx,dy,-depth)) ) { depth += 1 }

				drawQuad(vbo,context,xp * ww,yp * wh,tw,th,tc,Color( (1.0f - (depth / 5.0f)) , 1.0f ))
			}

			val workerTC = context.textureBlock( image("ui/checkbox_on.png") )
			val workers = List(world.worker1,world.worker2)
			for ( worker <- workers ) {
				val xp = (worker.position.x + range) / (range * 2.0f + 1)
				val yp = (worker.position.y + range) / (range * 2.0f + 1)

				drawQuad(vbo,context,xp * ww,yp * wh,tw,th,workerTC,Color.White)
			}
		}
	}

	override protected def updateLogic(f: Float): Unit = {
//		for ( worker <- world.worker1 :: world.worker2 :: Nil ) {
//			if ( ! world.terrain.isSolid( worker.position.toVoxelCoord.minusZ(1) ) ) {
//				worker.position = worker.position.minusZ(1)
//			}
//		}
//		val dx = rand(-range,range)
//		val dy = rand(-range,range)
//		var depth = 0
//		while ( ! world.terrain.isSolid(VoxelCoord.Center + Vec3i(dx,dy,-depth)) ) { depth += 1 }
//		world.terrain.setMaterialAt( VoxelCoord.Center + Vec3i(dx,dy,-depth) , Material.Sentinel )

	}
}
