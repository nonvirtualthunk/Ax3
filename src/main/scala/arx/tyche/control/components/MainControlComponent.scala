package arx.tyche.control.components

import arx.application.Noto
import arx.core.math.Intersection
import arx.core.math.Intersection.NoLineIntersection
import arx.core.vec.Vec3f
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.event.Event.{KeyPressEvent, MouseButton, MousePressEvent}
import arx.engine.entity.TGameEntity
import arx.engine.graphics.data.PovData
import arx.graphics.GL
import arx.tyche.game.data.{DoNothing, Globe, Spirit, SpiritAction, TerrainType}
import arx.tyche.graphics.data.SelectionData
import org.lwjgl.glfw.GLFW
import scalaxy.loops._

class MainControlComponent(ce: ControlEngine) extends ControlComponent(ce) {

	graphics[SelectionData].selectedSpirit = world.entitiesWithData[Spirit].head

	def selectedSpirit : Spirit = graphics[SelectionData].selectedSpirit[Spirit]

	controlEvents.listen {
		case mpe: MousePressEvent =>
			val pov = graphics[PovData].pov
			val start = pov.unproject(Vec3f(mpe.mousePos, 0.0f))
			val end = pov.unproject(Vec3f(mpe.mousePos, 1.0f))

			Intersection.raySphereIntersection(start, end, Vec3f.Zero, 1.0f) match {
				case NoLineIntersection => Noto.info("no intersection")
				case li =>
					val t = li.apply(1)
					val point = start + (end - start) * t

					val patches = world[Globe].patchData
//					val closestPatch = Option(patches.minBy(p => (p.center.asCartesian - point).lengthSafe))
					val intersectedPatches = patches.filter(p => {
						var found = false
						for (i <- 0 until p.vertices.size optimized) {
							if (
								Intersection.rayTriangleIntersection(
									start,
									end,
									p.vertices(i).asCartesian,
									p.vertices((i + 1) % p.vertices.size).asCartesian,
									p.center.asCartesian).numIntersections > 0) {
								found = true
							}
						}
						found
					})
					val closestPatch = if (intersectedPatches.isEmpty) {
						None
					} else {
						Some(intersectedPatches.minBy(p => (p.center.asCartesian - point).lengthSafe))
					}

					closestPatch match {
						case Some(patch) =>
							if (mpe.mouseButton == MouseButton.Right || mpe.modifiers.ctrl) {
								val actionType = selectedSpirit.selectedActionType.getOrElse(selectedSpirit.actionTypes.head)
								selectedSpirit.destination = Some(patch.center)
								selectedSpirit.intendedAction = Some(SpiritAction(actionType, patch.entity))
							} else {
								selectedSpirit.destination = Some(patch.center)
								selectedSpirit.intendedAction = None
							}
						case None => Noto.info("No closest patch")
					}

					world[Globe].markModified()
			}

		case kpe : KeyPressEvent =>
			kpe.key match {
				case GLFW.GLFW_KEY_W => selectAction(0)
				case GLFW.GLFW_KEY_S => selectAction(1)
				case _ => // do nothing
			}
	}


	def selectAction(idx : Int) : Unit = {
		if (selectedSpirit.actionTypes.size > idx) {
			selectedSpirit.selectedActionType = Some(selectedSpirit.actionTypes(idx))
		}
	}
}
