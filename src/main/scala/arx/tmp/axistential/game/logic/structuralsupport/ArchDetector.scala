package arx.axistential.game.logic.structuralsupport

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/14
 * Time: 2:59 PM
 */

import arx.axistential.game.data.world.Arch
import arx.axistential.game.data.world.StructuralSupportData
import arx.axistential.game.data.world.TerrainData
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.engine.world.World

import scalaxy.loops._

object ArchDetector extends ArchDetector {
	import StructuralSupportData._
	private final val archVec = Array(Vec3i.UnitX,Vec3i.UnitY)
	private final val EmptyVoxSet = Set[VoxelCoord]()

	def checkForSelfSupportingChain ( chain : List[VoxelCoord])(implicit world : World) : Boolean = {
		val SSD = world.aux[StructuralSupportData]
		// considering all stones but the keystone, ensure that their support values are monotonically increasing
		// .tail because the keystone is at the end, then we reversed
		for (v <- chain.reverse.tail) {
			val ss = SSD.supportGrid(v)
			if (ss.source.direction == Top || ss.support <= 0) {
				return true
			}
		}
		false
	}

	override def verifyArch(arch: Arch)(implicit world : World): Boolean = {
		!(checkForSelfSupportingChain(arch.leftChain) || checkForSelfSupportingChain(arch.rightChain))
	}


	override def keystonesFor(v: VoxelCoord, xyi : Int)(implicit world: World): Set[VoxelCoord] = {
		keystonesFor(v,xyi,-1)
	}

	def keystonesFor (v : VoxelCoord, xyi : Int, from : Int)(implicit world : World) : Set[VoxelCoord] = {
		val SSD = world.aux[StructuralSupportData]
		val sourceDat = SSD.source(v)
		if (sourceDat.isKeystone(xyi)) {
			Set(v)
		} else if (sourceDat.isVoussoir(xyi)) {
			var ret = keystonesFor(v + Vec3i.UnitZ,xyi)
			if (from != 0) { ret ++= keystonesFor(v + archVec(xyi),xyi,1) }
			if (from != 1) { ret ++= keystonesFor(v - archVec(xyi),xyi,0) }
			ret
		} else {
			EmptyVoxSet
		}
	}

	def identifyPossibleKeystones(startV:VoxelCoord,vec:ReadVec3i,TD: TerrainData) = {
		var ret = List[VoxelCoord]()
		var v = startV + vec
		while ( TD.isSolid(v.x,v.y,v.z) && ! TD.isSolid(v.x,v.y,v.z-1) ) {
			ret ::= v
			v = v + vec
		}
		ret.reverse
	}

	def walkUpArchPath(from : VoxelCoord, vec : ReadVec3i)(implicit TD : TerrainData) : List[VoxelCoord] = {
		val hv = from + vec
		val vv = from.plusZ(1)
		if (TD.isSolid(hv)) {
			if (TD.isSolid(hv.minusZ(1))) {
				List(from)
			} else {
				from :: walkUpArchPath(hv,vec)
			}
		} else if (TD.isSolid(vv)) {
			from :: walkUpArchPath(vv,vec)
		} else { List(from) }
	}

	def couldBeXKeystone (v : VoxelCoord)(implicit TD:TerrainData) = ! TD.isSolid(v.x,v.y,v.z-1) && TD.isSolid(v.x-1,v.y,v.z) && TD.isSolid(v.x+1,v.y,v.z)
	def couldBeYKeystone (v : VoxelCoord)(implicit TD:TerrainData) = ! TD.isSolid(v.x,v.y,v.z-1) && TD.isSolid(v.x,v.y-1,v.z) && TD.isSolid(v.x,v.y+1,v.z)
	def couldBeKeystone (v : VoxelCoord,xyi : Int)(implicit TD:TerrainData) = if (xyi == Xi) { couldBeXKeystone(v) } else { couldBeYKeystone(v) }


	def possibleArchesForBlock(v:VoxelCoord,xyi:Int)(implicit world : World) : List[Arch] = {
		implicit val TD = world.aux[TerrainData]
		var ret = List[Arch]()

		if (couldBeKeystone(v,xyi)) {
			ret :::= createArchIfPossibleForKeystone(v,xyi).toList
		} else {
			for (neg <- -1 to 1 by 2 optimized) {
				walkUpArchPath(v,archVec(xyi) * neg) match {
					case l if l.size > 1 => {
						val end = l.last
						val head = l.head
						if ( head.x != end.x || head.y != end.y ) {
							ret :::= createArchIfPossibleForKeystone(end,xyi).toList
						}
					}
					case _ => // do nothing
				}
			}
		}
		ret
	}
	
	def createArchIfPossibleForKeystone(v:VoxelCoord,xyi:Int)(implicit world : World) : Option[Arch] = {
		
		val TD = world.aux[TerrainData]
		val SSD = world.aux[StructuralSupportData]
		var leftKeystones = identifyPossibleKeystones(v,archVec(xyi) * -1,TD)
		var rightKeystones = identifyPossibleKeystones(v,archVec(xyi),TD)

		var keystone = v
		while (leftKeystones.size > rightKeystones.size + 1) {
			rightKeystones ::= keystone
			keystone = leftKeystones.head
			leftKeystones = leftKeystones.tail
		}
		while (rightKeystones.size > leftKeystones.size + 1) {
			leftKeystones ::= keystone
			keystone = rightKeystones.head
			rightKeystones = rightKeystones.tail
		}
		if (TD.isSolid(keystone.x,keystone.y,keystone.z-1)) { return None }

		val leftBoost = if (leftKeystones.size < rightKeystones.size) {1} else {0}
		val rightBoost = if (rightKeystones.size < leftKeystones.size) {1} else {0}

		val leftChain = createChain(keystone,xyi,archVec(xyi) * -1,leftBoost,TD,SSD)
		val rightChain = createChain(keystone,xyi,archVec(xyi),rightBoost,TD,SSD)

		// here we ensure that both chains are sufficiently long, drop down at least one z level, are at least
		// close in length, and that each is based on a block adjacent to a block not in the chain (to prevent
		// hanging, self-supporting arches)
		if (leftChain.size > 1 && rightChain.size > 1
			&& leftChain.last.z - leftChain.head.z > 0
			&& rightChain.last.z - rightChain.head.z > 0
			&& leftChain.size - 1 > (rightChain.size - 1) / 2
			&& rightChain.size - 1 > (leftChain.size - 1) / 2
			&& hasNonTopAdjacentBlock(leftChain,TD)
			&& hasNonTopAdjacentBlock(rightChain,TD)
		){
			Some(Arch(leftChain,rightChain,xyi))
		} else {
			None
		}
	}

	/**
	 * Rules for a valid arch chain :
	 * 1) monotonically increasing slope (i.e. gets steeper as it goes down)
	 * 2) at most VerticalSupport * 2 blocks in chain
	 */
	def createChain (keystone : VoxelCoord,xyi : Int,vec : ReadVec3i, horizontalBoost : Int,TD:TerrainData,SSD:StructuralSupportData) = {
		var lastSolidChain = List[VoxelCoord]()
		var chain = List(keystone)

//		val keystoneMat = materialInfoFor( TD.materialByteAt(keystone) )

		var horizontalSteps = horizontalBoost
		var verticalSteps = 0
		var lastSlope = 0.0f
		var lastStepWasHorizontal = true
		var maxRemainingSteps = 50//keystoneMat.verticalSupportOverWeight * 2

		var totalHorizontalSteps = 0
		var maxHorizontalRun = 0

		var ended = false
		while (! ended && maxRemainingSteps > 0) {
			val v = chain.head
			var thisStepWasHorizontal = true
			// if we've reached a root block, that is the base of our arch, going down further will not help us
			val curSource = SSD.source(v)
			if (curSource.isRootBlock) {
				ended = true
				if (horizontalSteps == 0) {
					val allowedVerticalEndSection = lastSlope.ceil.toInt
					lastSolidChain = chain.tail.drop((verticalSteps - allowedVerticalEndSection - 1).max(0))
				} else
				if (verticalSteps / horizontalSteps.toFloat >= lastSlope) {
					lastSolidChain = chain.tail
				}

				// this is a possible approach to getting rid of self supporting arches, but it would
				// only work if there was a full update pass before doing arch creation, otherwise it
				// breaks arches depending on ordering
				//			} else if (chain.head.z != chain.last.z && curSource.direction == Top) {
				//				lastSolidChain = Nil
				//				ended = true
			} else if (TD.isSolid(v.x,v.y,v.z - 1)) {
				verticalSteps += 1
				chain ::= v.minusZ(1)
				thisStepWasHorizontal = false
				if (lastStepWasHorizontal) {
					val slope = verticalSteps / horizontalSteps.toFloat
					//					println("Slope : " + slope)
					if (slope < lastSlope) {
						ended = true
					} else {
						// we only update the chain that we'll actually return when we complete a segment with a valid slope
						// that way we automatically backtrack to the last good spot in the arch when dealing with non-monotonic
						lastSolidChain = chain
						lastSlope = slope
						maxHorizontalRun = math.max(maxHorizontalRun,horizontalSteps)
						totalHorizontalSteps += horizontalSteps
						verticalSteps = 0
						horizontalSteps = 0
					}
				}
			} else if (TD.isSolid(v + vec)) {
				horizontalSteps += 1
				chain ::= v + vec
				thisStepWasHorizontal = true
			} else {
				// we want to update the last solid chain even if we've run out of blocks, so long as the slope is ok
				if (verticalSteps / horizontalSteps.toFloat >= lastSlope) {
					//					chain ::=
					lastSolidChain = chain
				}
				ended = true
			}

			// when we switch from horizontal to vertical step, we calculate our slope
			//			if (! ended && lastStepWasHorizontal != thisStepWasHorizontal && verticalSteps > 0 && horizontalSteps > 0 ) {
			//
			//			}
			lastStepWasHorizontal = thisStepWasHorizontal

			maxRemainingSteps -= 1
		}

		// mini-arch guard
		if (totalHorizontalSteps <= 1) {
			Nil
		} // lintel guard
		else if ((maxHorizontalRun > (totalHorizontalSteps +1)/ 2 && totalHorizontalSteps >= 3) ||
			(maxHorizontalRun == totalHorizontalSteps && totalHorizontalSteps > 1))
		{
			Nil
		} else {
			lastSolidChain
		}
	}


	def hasNonTopAdjacentBlock(l : List[VoxelCoord],TD:TerrainData) : Boolean = {
		if (l.size < 2) { false }
		else {
			val v = l.head
			val prev = l(1)
			for (q <- 0 until 6 optimized) {
				if (q != Top) {
					val av = v + dirvec(q)
					if (av != prev && TD.isSolid(av)) {
						return true
					}
				}
			}
			false
		}
	}

}

trait ArchDetector {
	def verifyArch (arch : Arch)(implicit world : World) : Boolean
	def keystonesFor (v : VoxelCoord,xyi : Int)(implicit world : World) : Set[VoxelCoord]
	def possibleArchesForBlock(v:VoxelCoord,xyi:Int)(implicit world : World) : List[Arch]
}