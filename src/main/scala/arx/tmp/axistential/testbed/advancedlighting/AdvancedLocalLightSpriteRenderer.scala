package arx.axistential.testbed.advancedlighting

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/20/14
 * Time: 9:04 AM
 */

import arx.axistential.testbed.TestSpriteRendererComponent
import arx.axistential.testbed.TestSpriteRendererComponent.Sprite
import arx.core.vec.Vec4f
import arx.tmp.game.logic.entities.TLightSource
import org.lwjgl.opengl.GL11

class AdvancedLocalLightSpriteRenderer extends TestSpriteRendererComponent{
	override def drawSprite(bucket: RenderBucket, sprite: Sprite) {
		bucket.textureBlock.minFilter = GL11.GL_NEAREST
		bucket.textureBlock.magFilter = GL11.GL_NEAREST

		val baseImage = image(sprite.resource + "diffuse.png")
		val normalImage = image(sprite.resource + "normal.png")

		val position = sprite.position
		val color = Vec4f.One
		val texCoords = bucket.textureBlock(baseImage)
		val dimensions = sprite.dimensions

		val vi = bucket.incrementVertexOffset(4)
		val ii = bucket.incrementIndexOffset(6)

		val AP = AdvancedLocalLightSpriteAttributeProfile



		var i = 0;while (i < 4) {
			bucket.vbo.setA(AP.Vertex,vi + i,position.x,position.y,position.z + (if ( i < 2 ) { 0.0f } else { dimensions.y }))
			bucket.vbo.setAbf(AP.LocalLightStrengths,vi + i,1.0f,1.0f,1.0f,1.0f,128)
			bucket.vbo.setAb(AP.LocalLightIndices,vi + i,0,0,0,0)
			bucket.vbo.setA(AP.TexCoord,vi + i,texCoords(i))
			bucket.vbo.setA(AP.BillboardOffset,vi + i,Cardinals.unitBillboardOffsets(i).x * dimensions.x,0.0f,0.0f)
			i += 1}
		bucket.vbo.setIQuad(ii,vi)
	}

	override def bucketRequirements: RenderBucketRequirements = {
		RenderBucketRequirements(
			AdvancedLocalLightSpriteAttributeProfile, {
			val s = new AdvancedLocalLightSpriteShader (world, pov)
			s.u_LocalLightPositions.set(() => world.entitiesOfType[TLightSource].map(_.exactLightLocation).toVector)
			s
		}
		)
	}
}
