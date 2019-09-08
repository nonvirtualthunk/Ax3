package arx.rog2.game.application

/**
  * TODO: Add javadoc
  */

import arx.Prelude._

import scalaxy.loops._
import arx.rog2.game.data.world._
import arx.rog2.game.data.entity._
import arx.engine.entity.TGameEntity
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec._
import arx.graphics.Image
import arx.graphics.helpers.HSBA
import arx.graphics.images.Palette
import arx.graphics.images.PaletteGroup
import arx.graphics.images.PaletteImage
import arx.resource.ResourceManager


object PaletteImageApplication extends ImageDisplayEngine {
	override def generateImages: List[Image] = {
		val baseImg = ResourceManager.image("rog/entities/torch.png")
		val palettized = PaletteImage.fromImage(baseImg, implicitPalette = true)

		val mainGroupA = PaletteGroup.fromColors(Vector(HSBA(0.0f,0.1f,0.1f,1.0f), HSBA(43.0f/360.0f, 0.76f, 0.97f, 1.0f), HSBA(43.0f/360.0f,0.85f,0.47f,1.0f)))
		val alteredA = palettized.withPalette(Palette(Vector(mainGroupA),"ColoredA"))

		val mainGroupB = PaletteGroup.fromColors(Vector(HSBA(0.2f,0.1f,0.1f,1.0f), HSBA(113.0f/360.0f, 0.87f, 0.80f, 1.0f), HSBA(40.0f/360.0f,0.80f,0.40f,1.0f)))
		val alteredB = palettized.withPalette(Palette(Vector(mainGroupB),"ColoredB"))
		List(alteredA, alteredB)
	}
}
