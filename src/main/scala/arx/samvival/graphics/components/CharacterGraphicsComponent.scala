package arx.samvival.graphics.components

import arx.application.Noto
import arx.core.math.Rectf
import arx.core.vec.coordinates.{CartVec, CartVec3}
import arx.core.vec.{Cardinals, Vec2f, Vec3f}
import arx.engine.advanced.lenginecomponents.DrawPriority
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.engine.entity.Taxon
import arx.engine.graphics.components.windowing.{TextRenderer}
import arx.engine.graphics.data.WindowingGraphicsData
import arx.engine.lworld.{GameEventClock, LEntity, LWorldView}
import arx.graphics.Image
import arx.graphics.helpers.HSBA
import arx.resource.ResourceManager
import arx.samvival.game.entities
import arx.samvival.game.entities.Fields.CharacterInfo
import arx.samvival.game.entities.{CharacterClass, CharacterInfo, IdentityData, Levels, Physical, Species, Terrain, Tile, Tiles}
import arx.samvival.game.logic.Leveling
import arx.samvival.graphics.GameImageManager
import arx.samvival.graphics.data.CullingData

class CharacterGraphicsComponent(engine : LGraphicsEngine) extends SamvivalCanvasGraphicsComponent(engine) {
	drawOrder = Layers.Character

	val cullingRevisionWatcher = engine.graphicsWorld[CullingData].createRevisionWatcher
	var hadOverlay = false
	var lastDrawnTime : GameEventClock = GameEventClock(-1)

	override def needsUpdate: Boolean = {
//		world.currentTime > lastDrawnTime || cullingRevisionWatcher.hasChanged || world.dataStore[CharacterData].hasOverlay || world.dataStore[Physical].hasOverlay || hadOverlay
		true
	}

	override def draw(canvas: SVCanvas): Unit = {
		hadOverlay = world.dataStore[CharacterInfo].hasOverlay || world.dataStore[Physical].hasOverlay
		lastDrawnTime = world.currentTime

		val cullData = engine.graphicsWorld[CullingData]

		val characterDataStore = world.dataStore[CharacterInfo]
		val physicalDataStore = world.dataStore[Physical]

		val entitiesWithData = characterDataStore.entities.map(e => (e, characterDataStore.get(e), physicalDataStore.get(e))).toStream.sortBy(t => -t._3.position.asCartesian.y)

		for ((charEnt, character, physical) <- entitiesWithData) {
			if (cullData.hexesInView.contains(physical.exactPosition.asAxialVec) && character.alive) {
				val identity = world.data[IdentityData](charEnt)

				canvas.quad(physical.exactPosition, Layers.Character)
					.withColor(physical.colorMultiplier)
					.withTexture(GameImageManager.imageForCharacter(charEnt))
					.withDimensions(CartVec.One)
					.withLightColor(Vec3f.One)
					.draw()


				drawHealthBar(canvas, character, physical)
			}
		}

//		val font = ResourceManager.font("Baldur", canvas.textureBlock)
//		val textQuads = TextRenderer.render(PixelLayouter, "ATest", font, Rectf(0.0f,0.0f,1000.0f,1000.0f), 1.0f, Cardinals.Left)
//		textQuads.foreach(quad => {
//			quad.texCoords match {
//				case Some(tc) =>
//					canvas.quad(CartVec3.Zero + CartVec3(0.0f,1.0f,0.0f), Layers.CharacterOverlay)
//   					.withOffsetPixels(quad.rect.xy)
//						.withColor(HSBA.White)
//						.withTexCoords(tc)
//						.withDimensions(quad.rect.dimensions)
//						.draw()
//				case None =>
//					Noto.warn("Text rendered with non-TC'd quads")
//			}
//		})

	}

	private def drawHealthBar(canvas: SVCanvas, character: CharacterInfo, physical: Physical) = {
		val healthBarCenter = physical.exactPosition + CartVec3(0.5f, 0.0f, 0.0f)
		val healthBarInnerDim = CartVec(0.1f, 0.5f)
		val healthBarOuterDim = healthBarInnerDim + 0.05f

		canvas.quad(healthBarCenter, Layers.CharacterOverlay)
			.withColor(HSBA.Black.withA(physical.colorMultiplier.a))
			.withDimensions(healthBarOuterDim)
			.draw()

		val pcntHealthBar = character.health.currentValue.toFloat / character.health.maxValue.toFloat
		val dmgPcnt = 1.0f - pcntHealthBar
		canvas.quad(healthBarCenter.xy - CartVec(0.0f, healthBarInnerDim.y * dmgPcnt * 0.5f), Layers.CharacterOverlay)
			.withColor(HSBA.fromRGBA(0.8f, 0.1f, 0.1f, physical.colorMultiplier.a))
			.withDimensions(healthBarInnerDim * CartVec(1.0f,pcntHealthBar))
			.draw()
	}
}
