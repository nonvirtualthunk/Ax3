package arx.samvival.graphics.components

import arx.Prelude.toArxAny
import arx.core.vec.Vec3f
import arx.core.vec.coordinates.CartVec
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.graphics.helpers.HSBA
import arx.samvival.game.actions.{AttackAction, MoveAction}
import arx.samvival.game.entities.Physical
import arx.samvival.graphics.data.Selection

class OverlayGraphicsComponent(engine : LGraphicsEngine) extends SamvivalCanvasGraphicsComponent(engine) {
	drawOrder = Layers.CharacterOverlay



	override def draw(canvas: SVCanvas): Unit = {
		for (selectedChar <- graphics[Selection].selectedCharacters) {
			val pos = selectedChar[Physical].exactPosition
			canvas.quad(pos, Layers.CharacterUnderlay)
				.withTexture("samvival/ui/selectedTop.png")
				.withColor(HSBA.fromRGBA(0.8f,0.1f,0.2f,1.0f))
				.withDimensions(CartVec(1.0f,1.0f))
				.withLightColor(Vec3f.One)
				.draw()
			canvas.quad(pos, Layers.CharacterOverlay)
   			.withTexture("samvival/ui/selectedBottom.png")
   			.withColor(HSBA.fromRGBA(0.8f,0.1f,0.2f,1.0f))
   			.withDimensions(CartVec(1.0f,1.0f))
   			.withLightColor(Vec3f.One)
   			.draw()


			var lastPos = pos
			for (action <- graphics[Selection].selectedPossibleActions) {
				action pmatch {
					case MoveAction(char, path) => {
						for (step <- path.steps) {
							val pos = step.node.asCartesian
							if (lastPos != pos) {
								//					canvas.line(lastPos, pos, Layers.CharacterOverlay, 1.0f, HSBA.White).draw()
								val delta = (pos - lastPos).normalizeSafe
								val ortho = delta.cross(Vec3f.UnitZ)

								canvas.quad(pos, Layers.CharacterUnderlay)
									.withTexture("samvival/ui/feet.png")
									.withColor(HSBA.fromRGBA(1.0f, 1.0f, 1.0f, 1.0f))
									.withLightColor(Vec3f.One)
									.withDimensions(CartVec(1.0f, 1.0f))
									.withForward(delta)
									.withOrtho(ortho)
									.draw()
							}
							lastPos = pos
						}
					}
					case AttackAction(char, from, targets, weapon, attack) => {
						for (target <- targets) {
							val targetPos = target[Physical].exactPosition
							val middle = (from.asCartesian + targetPos) * 0.5f
							val direction = (targetPos - from.asCartesian).normalizeSafe

							canvas.quad(middle, Layers.CharacterOverlay)
   							.withTexture("samvival/ui/forwardArrow.png")
   							.withDimensions(CartVec(0.25f,0.25f))
   							.withForward2D(direction)
   							.withColor(HSBA.fromRGBA(1.0f,0.2f,0.2f,1.0f))
   							.draw()
						}
					}
				}
			}
		}
	}
}
