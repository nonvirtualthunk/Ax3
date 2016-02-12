package arx.axistential.game.logic.structuralsupport

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/14
 * Time: 2:56 PM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.StructuralSupportData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.structuralsupport.StructuralSupportLogic.MaterialInfo
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.TVoxelView

import scalaxy.loops._

object StructuralSupportLogic extends TStructuralSupportLogic {
	import StructuralSupportData._
	val kgPerUnit = 75

	val DiagonalLeft = 6
	val DiagonalRight = 7
	val DiagonalFront = 8
	val DiagonalBack = 9
	val NumCoreDirections = 10
	val DiagonalsStartQ = 6
	val DiagonalsEndQ = 10
	val DiagonalAll = 10
	val FromArch = 11
	val NumDirections = 12
	val DiagonalCorrespondence = Map(DiagonalLeft -> Left, DiagonalRight -> Right, DiagonalFront -> Front, DiagonalBack -> Back)
	val DiagonalCorrespondenceArray = fillArray(10)(i => DiagonalCorrespondence.getOrElse(i,-1))
	val DiagVec = Map(DiagonalLeft -> Vec3i(-1,0,-1),DiagonalRight -> Vec3i(1,0,-1),DiagonalFront -> Vec3i(0,1,-1),DiagonalBack -> Vec3i(0,-1,-1))
	
	val cardinals = (for(i <- 0 until 10) yield {
		if (i < 6) { dirvec(i) }
		else { DiagVec(i) }
	}).toArray

	val spreadVecs = (for(i <- 0 until 10) yield {
		if (i < 6) { dirvec(i) }
		else { DiagVec(i) * Vec3i(1,1,-1) }
	}).toArray

	def isDiagonal (direction : Int) = if (direction < 6) { false } else { true }

	case class MaterialInfo (
		material : Material,
		effVerticalSupport : Short,
		effHorizontalSupport : Short,
		effDiagonalSupport : Short,
		effWeight : Short,
		effDiagonalWeight : Short,
		verticalSupportOverWeight : Short,
		diagonalAllPercentage : Float
	)
	protected val materialInfo = Array.ofDim[MaterialInfo](255)
	
	def supportFromDirection (v : VoxelCoord, terrainTalea : TVoxelView[Byte], supportTalea : TVoxelView[Int], direction : Int, newMatInfo : MaterialInfo)(implicit TD:TerrainData) = {
		val ax = v.x + cardinals(direction).x
		val ay = v.y + cardinals(direction).y
		val az = v.z + cardinals(direction).z

		val matByte = terrainTalea(ax,ay,az)

		if (TerrainByteUtils.isSolid(matByte)) {
			val matInfo = materialInfoFor(matByte)
			val supportStruct = supportTalea(ax,ay,az)
			val source = supportStruct.source
			if (source.isRootBlock) {
				val base = direction match {
					case Bottom => matInfo.effVerticalSupport * 7
					case i if i >= 6 => matInfo.effDiagonalSupport
					case _ => matInfo.effHorizontalSupport
				}
				math.min(base,StructuralSupportData.MaxSupportValue)
			} else {
				val max = direction match {
					case Bottom => matInfo.effVerticalSupport min newMatInfo.effVerticalSupport
					case i if i >= 6 => matInfo.effDiagonalSupport min newMatInfo.effDiagonalSupport
					case _ => matInfo.effHorizontalSupport min newMatInfo.effHorizontalSupport
				}
				math.min(supportStruct.support,max)
			}
		} else {
			0
		}
	}

	def archSupportFromDirection(v: VoxelCoord, terrainWindow: TVoxelView[Byte], supportWindow: TVoxelView[Int], targetSource : Short, direction: Int, newMatInfo : MaterialInfo)(implicit TD : TerrainData) = {
		if (direction != Top) {
			val ax = v.x + cardinals(direction).x
			val ay = v.y + cardinals(direction).y
			val az = v.z + cardinals(direction).z

			val matByte = terrainWindow(ax,ay,az)

			if (TerrainByteUtils.isSolid(matByte)) {
				val matInfo = materialInfoFor(matByte)
				val supportStruct = supportWindow(ax,ay,az)
				val source = supportStruct.source
				// if the block we're considering this for is part of an x-arch, it cannot pull from an x-keystone
				if (	(targetSource.isXVoussoir && source.isXKeystone) || (targetSource.isYVoussoir && source.isYKeystone) ) {
					0
				} else {
					val effDirection = if (source.isInArch) { Bottom } else { direction }
					if (source.isRootBlock) {
						val base = effDirection match {
							case Bottom => matInfo.effVerticalSupport * 7
							case _ => matInfo.effHorizontalSupport
						}
						math.min(base,StructuralSupportData.MaxSupportValue)
					} else {
						val max = effDirection match {
							case Bottom => matInfo.effVerticalSupport min newMatInfo.effVerticalSupport
							case _ => matInfo.effHorizontalSupport min newMatInfo.effHorizontalSupport
						}
						math.min(supportStruct.support,max)
					}
				}
			} else { 0 }
		} else { 0 }
	}


	override def computeSupportValueFor(v: VoxelCoord)(implicit world : World) : StructuralSupportValue = {
		val TD = world.aux[TerrainData]
		computeSupportValueForBase(v,materialInfoFor(TD.materialAt(v)),false)
	}

	override def hypotheticalSupportValueFor(v: VoxelCoord, withAddedMaterial: Material)(implicit world: World): StructuralSupportValue = {
		computeSupportValueForBase(v,materialInfoFor(withAddedMaterial),true)
	}

	def computeSupportValueForBase(v: VoxelCoord, matInfo : MaterialInfo, hypo : Boolean)(implicit world : World) : StructuralSupportValue = {
		implicit val TD = world.aux[TerrainData]
		implicit val SSD = world.aux[StructuralSupportData]

		val terrainView = TD.materialGrid.windowCenteredOnTaleaAt(v,readOnly = true)
		val supportView = SSD.supportGrid.windowCenteredOnTaleaAt(v,readOnly = true)
		val tpos = v - terrainView.center
		val newMatInfo = matInfo
		val supportStruct = supportView(tpos.x,tpos.y,tpos.z)
		val supportSource = supportStruct.source

		if ( ! hypo && supportSource.isRootBlock ) { return supportStruct }

		val supportLevels = Array.ofDim[Int](NumDirections)
		val supportSources = Array.ofDim[Short](NumDirections)
		val SL = supportLevels
		for (q <- 0 until NumCoreDirections optimized) {
			// only check the diagonals if they are properly connected
			if (q < 6 || supportLevels(Bottom) > 0 || supportLevels(DiagonalCorrespondenceArray(q)) > 0) {
				val rawSupport = supportFromDirection(tpos,terrainView,supportView,q,newMatInfo)
				val weight = if (isDiagonal(q)) { newMatInfo.effDiagonalWeight } else { newMatInfo.effWeight }
				supportLevels(q) = rawSupport - weight
				supportSources(q) = supportView(tpos.x + cardinals(q).x,tpos.y + cardinals(q).y,tpos.z + cardinals(q).z).source
				//				if (supportSources(q).direction == DiagonalAll && (q == Left || q == Right || q == Front || q == Back)) {
				//					supportLevels(q) = 0
				//				}
			}
		}

		if (supportSource.isInArch) {
			supportLevels(FromArch) = {
				val xarch = supportSource.isInXArch
				val yarch = supportSource.isInYArch
				val left = xarch || SL (DiagonalLeft) > 0 || SL (Left) > 0
				val right = xarch || SL (DiagonalRight) > 0 || SL (Right) > 0
				val front = yarch || SL (DiagonalFront) > 0 || SL (Front) > 0
				val back = yarch || SL (DiagonalBack) > 0 || SL (Back) > 0

				var reduction = 0.0f
				if (left && right) { reduction += 0.5f }
				else if (left || right) { reduction += 0.1f }

				if (front && back) { reduction += 0.5f }
				else if (front || back) { reduction += 0.1f }

				var highestRawSupport = 0
				for (q <- 0 until 6 optimized) {
					// we don't allow X-arches to pull support from the y directions, and vice versa, and neither
					// direction of arch can pull support from above, it wouldn't really make sense
					if ( q == Top ||
						((q == Left || q == Right) && (yarch && ! xarch)) ||
						((q == Front || q == Back) && (xarch && ! yarch)))
					{}
					else {
						highestRawSupport = math.max(highestRawSupport,archSupportFromDirection(tpos,terrainView,supportView,supportSource,q,newMatInfo))
					}
				}
				highestRawSupport - (newMatInfo.effWeight * (1.0f - reduction)).toInt
			}
			//		} else if (supportLevels(Bottom) > 0) {
		} else if (TD.isSolid(v + dirvec(Bottom))) {
			supportLevels(DiagonalAll) = {
				var reduction = 0.0f

				if (supportLevels(DiagonalLeft) > 0 && supportLevels(DiagonalRight) > 0) {
					reduction += 0.5f
				} else if (supportLevels(DiagonalLeft) > 0 || supportLevels(DiagonalRight) > 0) {
					reduction += 0.1f
				}

				if (supportLevels(DiagonalFront) > 0 && supportLevels(DiagonalBack) > 0) {
					reduction += 0.5f
				} else if (supportLevels(DiagonalFront) > 0 || supportLevels(DiagonalBack) > 0) {
					reduction += 0.1f
				}

				supportLevels(Bottom) + (newMatInfo.effWeight * reduction * newMatInfo.diagonalAllPercentage).toInt
			}

			for (q <- DiagonalsStartQ until DiagonalsEndQ optimized) { supportLevels(q) = 0 }
		}

		var maxSupport = 0
		var maxQ = 0
		for (q <- 0 until NumDirections optimized) {
			val supportValue = supportLevels(q)
			if (supportValue > maxSupport) {
				maxSupport = supportValue
				maxQ = q
			}
		}

		StructuralSupportData.pack((supportSource & (~StructuralSupportData.DirectionMask)) | maxQ,maxSupport)
	}

	def materialInfoFor( b : Byte )(implicit TD : TerrainData) : MaterialInfo = {
		var r = materialInfo(b & 0xff)
		if (r == null) {
			val mat = TD.materialForByte(b)
			r = materialInfoFor(mat)
			materialInfo(b & 0xff) = r
		}
		r
	}

	def materialInfoFor(mat : Material) : MaterialInfo = {
		val evss = (mat.verticalSupportStrength.inKg / kgPerUnit) / voxelsPerMeter
		val ehss = (mat.horizontalSupportStrength.inKg / kgPerUnit) / voxelsPerMeter
		val ew = (mat.density.inKgPerMeter3 / kgPerUnit) / voxelsPerMeter
		val edw = (ew * 1.414f).toShort
		val vsow = (evss / ew).toShort

		val dap = evss / (evss + ehss)

		MaterialInfo(mat,evss.toShort,ehss.toShort,(evss * 0.33f + ehss * 0.666f).toShort,ew.toShort,edw,vsow,dap)
	}

}

trait TStructuralSupportLogic {
	def computeSupportValueFor (v : VoxelCoord)(implicit world : World) : StructuralSupportData.StructuralSupportValue
	def hypotheticalSupportValueFor (v : VoxelCoord, withAddedMaterial : Material)(implicit world : World) : StructuralSupportData.StructuralSupportValue
	def materialInfoFor (mat : Material) : MaterialInfo
}