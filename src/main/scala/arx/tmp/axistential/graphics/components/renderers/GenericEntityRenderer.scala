package arx.axistential.graphics.components.renderers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/10/13
 * Time: 11:37 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.entity.CosmeticData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.graphics.helpers.AnthCommonRendering
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.traits.TRenderTarget
import arx.graphics.TToImage
import arx.graphics.TextureBlock
import arx.resource.ResourceManager

import scala.collection.mutable

class GenericEntityRenderer extends TGenericEntityRenderer {
	val graphicsInfoProvider = pio[TGameEntityGraphicsInfoProvider]
	val voxelModelRenderer = pio[TVoxelModelRenderer]

	val recentFlipsByEntity = new mutable.HashMap[GameEntity,Float]

	def intersectionShapeFor(ent: GameEntity, env: World): TIntersectable = SentinelIntersectable

	/**
	 * Render the given entity
 *
	 * @param baseEnt entity to render
	 * @param env World the entity occupies
	 * @param renderTarget rendering target to store points and indices in
	 * @param textureBlock texture block to use for texture coordinates, should be bound when drawn to screen
	 * @param baseTransforms transformations to apply to the rendering
	 * @param graphicsEngine currently active graphics engine, provides point of view, etc
	 */
	def renderEntity(baseEnt: GameEntity, env: World, renderTarget: TRenderTarget, textureBlock: TextureBlock, baseTransforms: Transforms, graphicsEngine: GraphicsEngine): Unit = {
		 require(baseEnt.isInstanceOf[TPhysicalEntity],"Attempting to render entity that is not physical, we don't know how to do that, yet")

		val lightData = env.aux[LightData]
		var info = graphicsInfoProvider.graphicsInfoFor(baseEnt)
		var transforms = baseTransforms
		info match {
			case TransformedGraphicsInfo(baseInfo,modifyingTransforms) => {
				info = baseInfo
				transforms = transforms.mergedWith(modifyingTransforms)
			}
			case _ =>
		}

		val ent = baseEnt.asInstanceOf[TPhysicalEntity]
		val cosmeticData = ent.auxDataOrElse[CosmeticData](CosmeticData.Sentinel)

		 val dims = ent.fastDimensions.inVoxels
		 if ( dims.z < 1.0f && dims.z > 0.0f ) { //Scale to be at least one high, regardless of actual size
			 dims.x /= dims.z
			 dims.y /= dims.z
			 dims.z = 1.0f
		 }
		 dims.x = dims.x * transforms.dimensionMultiplier.x + transforms.dimensionAdder.x
		 dims.y = dims.y * transforms.dimensionMultiplier.y + transforms.dimensionAdder.y
		 dims.z = dims.z * transforms.dimensionMultiplier.z + transforms.dimensionAdder.z

		 info match {
			 case BillboardGraphicsInfo(textures,graphicsInfoColor,selfIlluminated) => {
				 val color = graphicsInfoColor * cosmeticData.color
				 val pos = ent.fastFootPosition + transforms.translation
				 val img = textures.headOption.getOrElse(TToImage(ResourceManager.defaultImage))

				 val scale = img.aspectRatio

				 val billboardWidth = dims.z * scale//math.max(dims.x,dims.y)
				 val billboardHeight = dims.z

				 val baseTexCoords = textureBlock.getOrElseUpdate( img )

				 val zPercent = transforms.zPercent

//				 var flip = ent.facing.cross( ent.position - graphicsEngine.pov.eye ).z < -0.05f
//				 var recentFlip = recentFlipsByEntity.getOrElseUpdate(ent,0.0f)
//				 if ( flip && recentFlip < 0.0f ) { recentFlip += 1.0f; flip = false }
//				 else if ( flip && recentFlip < 10.0f ) { recentFlip += 1.0f }
//				 else if ( ! flip && recentFlip > 0.0f ) { recentFlip -= 1.0f; flip = true }
//				 else if ( ! flip && recentFlip > -10.0f ) { recentFlip -= 1.0f }
//				 recentFlipsByEntity(ent) = recentFlip
//
//				 val texCoords = if ( flip ) { Array( baseTexCoords(1) , baseTexCoords(0) , baseTexCoords(3) , baseTexCoords(2) ) } else { baseTexCoords }
				 val texCoords = baseTexCoords


				 val lighting = if ( selfIlluminated ) { CommonRendering.LightResult(1.0f,1.0f,Vec3f(1.0f,1.0f,1.0f)) } else { AnthCommonRendering.lightOnObject(env,ent) }
				 if ( zPercent >= 1.0f ) {
					 CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,lighting,texCoords)
				 } else {
					 val zStart = texCoords(0).y
					 val zEnd = zStart + (texCoords(2).y - zStart) * zPercent
					 val newTexCoords = Array(ReadVec2f(texCoords(0).x,zStart),ReadVec2f(texCoords(1).x,zStart),ReadVec2f(texCoords(2).x,zEnd),ReadVec2f(texCoords(3).x,zEnd))
					 CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight * zPercent),color * transforms.colorMultiplier,lighting,newTexCoords)
				 }

				 /* This was the beginning of an experiment into rendering trees in multiple segments, with separate lighting.
				  * It hasn't panned out well so far though */
//				if ( selfIlluminated ) {
//					CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,CommonRendering.passthroughLightResult,texCoords)
//				} else {
//					AnthCommonRendering.uprightBillboardGrid(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,texCoords,env)
//				}

			 }
			 case mgi : ModelGraphicsInfo => {
				 val newTransforms = transforms.copy
				 newTransforms.colorMultiplier = mgi.color * cosmeticData.color * transforms.colorMultiplier
				 val centerPosition = ent.position + newTransforms.translation

				 val hasHiResModels = mgi.hiResModels.nonEmpty
				 newTransforms.basis = if ( transforms.basis.isIdentity ) { ent.directionBasis } else { transforms.basis }

				 val useFancy = false

				 if ( useFancy ) {
					 val lighting = env.aux[LightData]
					 if ( hasHiResModels && MathPrelude.distance(graphicsEngine.pov.eye,ent.position) < graphicsEngine.pov.hiResThreshold ) {
						 voxelModelRenderer.renderModelFancy(mgi.hiResModels.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
					 } else if ( mgi.models.nonEmpty ) {
						 voxelModelRenderer.renderModelFancy(mgi.models.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
					 } else {
						 voxelModelRenderer.renderModelFancy(ResourceManager.defaultModel,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
					 }
				 } else {
					 val lighting = if ( mgi.selfIlluminated ) { CommonRendering.passthroughLightResult } else { AnthCommonRendering.lightOnObject(env,ent) }
					 if ( hasHiResModels && MathPrelude.distance(graphicsEngine.pov.eye,ent.position) < graphicsEngine.pov.hiResThreshold ) {
						 voxelModelRenderer.renderModel(mgi.hiResModels.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
					 } else if ( mgi.models.nonEmpty ) {
						 voxelModelRenderer.renderModel(mgi.models.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
					 } else {
						 voxelModelRenderer.renderModel(ResourceManager.defaultModel,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
					 }
				 }
			 }
			 case CustomRendererGraphicsInfo(renderer,icon,dynamic) => {
				 if ( renderer == this ) {
					 Noto.warn("Custom renderer graphics info pointed at GenericEntityRenderer, no way to do that")
				 } else {
					 renderer.renderEntity(ent,env,renderTarget,textureBlock,transforms,graphicsEngine)
				 }
			 }
			 case NoGraphicsInfo => {
//				Noto.warn("No Graphics Info found for " + baseEnt + " : " + baseEnt.name)
			 }
//			 case mti : MaterialTextureInfo => {
//				 //Have the dimensions be based off of actual dimensions, but scaled down so it doesn't overwhelm everything
//				 val basePos = ent.fastFootPosition
//				 val dimensions = functions.max( functions.min( ent.dimensions.inVoxels * 0.9f , Vec3f(0.9f,0.9f,0.9f) ) , Vec3f(0.1f,0.1f,0.1f) )
//				 val position = basePos.plusZ(dimensions.z * 0.5f)
//				 val lighting = AnthCommonRendering.lightOnObject(env,ent)
//
//				 ent match {
//					 case mb : MaterialBlock if ( mb.material.resolve().isWoody ) => {
//						 val maxDim = dimensions.max
//						 val image = ResourceManager.getImage("game/misc/woodPile.png")
//						 CommonRendering.billboardQuad(renderTarget,position,CommonRendering.scaleToImage(ReadVec2f(maxDim,maxDim),image),mti.color,lighting,textureBlock(image))
//					 }
//					 case mb : MaterialBlock if ( mb.material.resolve().isStone ) => {
//						 val maxDim = dimensions.max
//						 val image = ResourceManager.getImage("game/misc/stonePile.png")
//						 CommonRendering.billboardQuad(renderTarget,position,CommonRendering.scaleToImage(ReadVec2f(maxDim,maxDim),image),mti.color,lighting,textureBlock(image))
//					 }
//					 case _ => {
//						 val texCoords = textureBlock.getOrElseUpdate(mti.images.head)
//						 CommonRendering.drawCube(renderTarget,position,dimensions,mti.color,lighting,texCoords)
//					 }
//				 }
//			 }
			 case o => Noto.warn("unknown object rendering type : " + o)
		 }
	}

	/**
	 * A hash of the renderable state, used to determine when an entities state has changed in such a way
	 * that it will require a re-draw. For a stockpile, the render hash might be the number of voxels it
	 * occupies, for a ladder, the number of voxels that are adjacent to a wall.
	 *
	 * @param baseEnt the entity to get a hash from
	 * @param env the containing World
	 * @param textureBlock the texture block that will be used
	 */
	def renderHash(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Int = 0

	def pointsRequiredFor(baseEnt: GameEntity, env: World, textureBlock: TextureBlock): Option[Int] = None


//	 lazy val objectGraphicsInfoProvider = ReflectionAssistant.provideInstanceOf[TGameEntityGraphicsInfoProvider]
//	 lazy val voxelModelRenderer = ReflectionAssistant.provideInstanceOf[TVoxelModelRenderer]
//
//
//	 def renderEntity ( baseEnt : GameEntity , world : World, renderTarget : TRenderTarget , textureBlock : TextureBlock , transforms : Transforms , graphicsEngine : GraphicsEngine) {
//		 require(baseEnt.isInstanceOf[TPhysicalEntity],"Attempting to render entity that is not physical, we don't know how to do that, yet")
//
//		 val env = world.asInstanceOf[Environment]
//		 val lightData = env.lightData
//		 val info = objectGraphicsInfoProvider.graphicsInfoFor(baseEnt)
//
//		 val ent = baseEnt.asInstanceOf[TPhysicalEntity]
//
//		 val dims = ent.fastDimensions.inVoxels
//		 if ( dims.z < 1.0f && dims.z > 0.0f ) { //Scale to be at least one high, regardless of actual size
//			 dims.x /= dims.z
//			 dims.y /= dims.z
//			 dims.z = 1.0f
//		 }
//		 dims.x = dims.x * transforms.dimensionMultiplier.x + transforms.dimensionAdder.x
//		 dims.y = dims.y * transforms.dimensionMultiplier.y + transforms.dimensionAdder.y
//		 dims.z = dims.z * transforms.dimensionMultiplier.z + transforms.dimensionAdder.z
//
//		 info match {
//			 case BillboardGraphicsInfo(textures,graphicsInfoColor,selfIlluminated) => {
//				 val color = graphicsInfoColor * ent.color
//				 val pos = ent.fastFootPosition + transforms.translation
//				 val img = textures.headOption.getOrElse(TToImage(ResourceManager.defaultImage))
//
//				 val billboardWidth = math.max(dims.x,dims.y)
//				 val billboardHeight = dims.z
//
//				 val texCoords = textureBlock.getOrElseUpdate( img )
//
//				 val zPercent = transforms.zPercent
//
//				 val lighting = if ( selfIlluminated ) { CommonRendering.LightResult(1.0f,1.0f,Vec3f(1.0f,1.0f,1.0f)) } else { AnthCommonRendering.lightOnObject(env,ent) }
//				 if ( zPercent >= 1.0f ) {
//					 CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,lighting,texCoords)
//				 } else {
//					 val zStart = texCoords(0).y
//					 val zEnd = zStart + (texCoords(2).y - zStart) * zPercent
//					 val newTexCoords = Array(ReadVec2f(texCoords(0).x,zStart),ReadVec2f(texCoords(1).x,zStart),ReadVec2f(texCoords(2).x,zEnd),ReadVec2f(texCoords(3).x,zEnd))
//					 CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight * zPercent),color * transforms.colorMultiplier,lighting,newTexCoords)
//				 }
//
//				 /* This was the beginning of an experiment into rendering trees in multiple segments, with separate lighting.
//				  * It hasn't panned out well so far though */
// //				if ( selfIlluminated ) {
// //					CommonRendering.uprightBillboardQuad(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,CommonRendering.passthroughLightResult,texCoords)
// //				} else {
// //					AnthCommonRendering.uprightBillboardGrid(renderTarget,pos,Vec2f(billboardWidth,billboardHeight),color * transforms.colorMultiplier,texCoords,env)
// //				}
//
//			 }
//			 case mgi : ModelGraphicsInfo => {
//				 val newTransforms = transforms.copy
//				 newTransforms.colorMultiplier = mgi.color * ent.color * transforms.colorMultiplier
//				 val centerPosition = ent.position + newTransforms.translation
//
//				 val hasHiResModels = mgi.hiResModels.nonEmpty
//				 newTransforms.basis = if ( transforms.basis.isIdentity ) { ent.directionBasis } else { transforms.basis }
//
//				 val useFancy = ent match {
//					 case obj : TObjectEntity => ! obj.objectArchetype.objectFlags.contains( ObjectFlags.LightBlocking )
//					 case _ => true
//				 }
//
//				 if ( useFancy ) {
//					 val lighting = env.lightData
//					 if ( hasHiResModels && Prelude.distance(graphicsEngine.pov.eye,ent.position) < graphicsEngine.pov.hiResThreshold ) {
//						 voxelModelRenderer.renderModelFancy(mgi.hiResModels.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
//					 } else if ( mgi.models.nonEmpty ) {
//						 voxelModelRenderer.renderModelFancy(mgi.models.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
//					 } else {
//						 voxelModelRenderer.renderModelFancy(ResourceManager.defaultModel,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
//					 }
//				 } else {
//					 val lighting = if ( mgi.selfIlluminated ) { CommonRendering.passthroughLightResult } else { AnthCommonRendering.lightOnObject(env,ent) }
//					 if ( hasHiResModels && Prelude.distance(graphicsEngine.pov.eye,ent.position) < graphicsEngine.pov.hiResThreshold ) {
//						 voxelModelRenderer.renderModel(mgi.hiResModels.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
//					 } else if ( mgi.models.nonEmpty ) {
//						 voxelModelRenderer.renderModel(mgi.models.head,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
//					 } else {
//						 voxelModelRenderer.renderModel(ResourceManager.defaultModel,renderTarget,textureBlock,centerPosition,dims,lighting,newTransforms)
//					 }
//				 }
//			 }
//			 case CustomRendererGraphicsInfo(renderer,icon,dynamic) => {
//				 if ( renderer == this ) {
//					 Noto.warn("Custom renderer graphics info pointed at GenericEntityRenderer, no way to do that")
//				 } else {
//					 renderer.renderEntity(ent,env,renderTarget,textureBlock,transforms,graphicsEngine)
//				 }
//			 }
//			 case NoGraphicsInfo => {
// //				Noto.warn("No Graphics Info found for " + baseEnt + " : " + baseEnt.name)
//			 }
//			 case mti : MaterialTextureInfo => {
//				 //Have the dimensions be based off of actual dimensions, but scaled down so it doesn't overwhelm everything
//				 val basePos = ent.fastFootPosition
//				 val dimensions = functions.max( functions.min( ent.dimensions.inVoxels * 0.9f , Vec3f(0.9f,0.9f,0.9f) ) , Vec3f(0.1f,0.1f,0.1f) )
//				 val position = basePos.plusZ(dimensions.z * 0.5f)
//				 val lighting = AnthCommonRendering.lightOnObject(env,ent)
//
//				 ent match {
//					 case mb : MaterialBlock if ( mb.material.resolve().isWoody ) => {
//						 val maxDim = dimensions.max
//						 val image = ResourceManager.getImage("game/misc/woodPile.png")
//						 CommonRendering.billboardQuad(renderTarget,position,CommonRendering.scaleToImage(ReadVec2f(maxDim,maxDim),image),mti.color,lighting,textureBlock(image))
//					 }
//					 case mb : MaterialBlock if ( mb.material.resolve().isStone ) => {
//						 val maxDim = dimensions.max
//						 val image = ResourceManager.getImage("game/misc/stonePile.png")
//						 CommonRendering.billboardQuad(renderTarget,position,CommonRendering.scaleToImage(ReadVec2f(maxDim,maxDim),image),mti.color,lighting,textureBlock(image))
//					 }
//					 case _ => {
//						 val texCoords = textureBlock.getOrElseUpdate(mti.images.head)
//						 CommonRendering.drawCube(renderTarget,position,dimensions,mti.color,lighting,texCoords)
//					 }
//				 }
//			 }
//			 case o => Noto.warn("unknown object rendering type : " + o)
//		 }
//	 }
//
//	 def pointsRequiredFor(ent: GameEntity, env: World, textureBlock : TextureBlock ) = {
//		 val info = objectGraphicsInfoProvider.graphicsInfoFor(ent)
//
//		 info match {
//			 case BillboardGraphicsInfo(textures,color,selfIlluminated) => {
//				 Some(4)
//			 }
//			 case ModelGraphicsInfo(models,color,icon,selfIlluminated) => {
//				 Some(voxelModelRenderer.pointsNeededForModel(models.headOption.getOrElse(ResourceManager.defaultModel),textureBlock))
//			 }
//			 case CustomRendererGraphicsInfo(renderer,icon,_) => {
//				 if ( renderer == this ) {
//					 Noto.warn("Custom renderer graphics info pointed at GenericEntityRenderer, no way to do that")
//					 None
//				 } else {
//					 renderer.pointsRequiredFor(ent,env,textureBlock)
//				 }
//			 }
//			 case NoGraphicsInfo => {
//				 None
//			 }
//			 case mti : MaterialTextureInfo => {
//				 None
//			 }
//			 case o =>
//				 Noto.warn("unknown object rendering type : " + o)
//				 None
//		 }
//	 }
//
//		def capsuleFromPhysicalEntity ( physEnt : TPhysicalEntity ) = {
//			val dims = physEnt.fastDimensions
//			val h = dims.z.inVoxels
//			val r = math.max( dims.x.inVoxels , dims.y.inVoxels )
//			new Capsule(physEnt.position.minusZ(h * 0.5f),physEnt.position.plusZ(h * 0.5f),r * 0.5f)
//		}
//
//	 def intersectionShapeFor(ent: GameEntity, env: World) = {
//		 val info = objectGraphicsInfoProvider.graphicsInfoFor(ent)
//		 ent match {
//			 case physEnt : TPhysicalEntity => {
//				 info match {
//					 case BillboardGraphicsInfo(textures,color,selfIlluminated) => {
//						 capsuleFromPhysicalEntity(physEnt)
//					 }
//					 case ModelGraphicsInfo(models,color,icon,selfIlluminated) => {
//						 val halfDims = physEnt.fastBoundingDimensions.inVoxels * 0.5f
//						 new AABB(physEnt.position - halfDims,physEnt.position + halfDims)
//					 }
//					 case CustomRendererGraphicsInfo(renderer,icon,_) => {
//						 if ( renderer == this ) {
//							 Noto.warn("Custom renderer graphics info pointed at GenericEntityRenderer, no way to do that")
//							 SentinelIntersectable
//						 } else {
//							 renderer.intersectionShapeFor(ent,env) match {
//								 case None => {
//									 Noto.warn("No intersection shape from custom renderer for entity : " + renderer + " : " + ent)
//									 SentinelIntersectable
//								 }
//								 case Some(shape) => shape
//							 }
//						 }
//					 }
//					 case NoGraphicsInfo => {
//						 SentinelIntersectable
//					 }
//					 case mti : MaterialTextureInfo => {
//						 val halfDims = physEnt.fastBoundingDimensions.inVoxels * 0.5f
//						 new AABB(physEnt.position - halfDims,physEnt.position + halfDims)
//					 }
//					 case o =>
//						 Noto.warn("unknown object rendering type : " + o)
//						 SentinelIntersectable
//				 }
//			 }
//			 case _ =>
//				 Noto.warn("Generic entity renderer can't yet handle non physical entities")
//				 SentinelIntersectable
//		 }
//	 }
//
//	 /**
//	  * A hash of the renderable state, used to determine when an entities state has changed in such a way
//	  * that it will require a re-draw. For a stockpile, the render hash might be the number of voxels it
//	  * occupies, for a ladder, the number of voxels that are adjacent to a wall.
//	  *
//	  * @param baseEnt the entity to get a hash from
//	  * @param env the containing environment
//	  * @param textureBlock the texture block that will be used
//	  */
//	 def renderHash(baseEnt: GameEntity, env: World, textureBlock: TextureBlock) = {
//		 val info = objectGraphicsInfoProvider.rawGraphicsInfoFor(baseEnt)
//
//		 info match {
//			 case CustomRendererGraphicsInfo(renderer,_,_) => {
//				 renderer.renderHash(baseEnt,env,textureBlock)
//			 }
//			 case StatefulGraphicsInfo(_,_,_) => baseEnt.activeStates.hashCode()
//			 case _ => 0
//		 }
//	 }

}
