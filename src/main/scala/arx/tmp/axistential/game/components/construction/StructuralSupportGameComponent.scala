package arx.axistential.game.components.construction

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/22/14
 * Time: 8:19 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.world.Arch
import arx.axistential.game.data.world.StructuralSupportData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.structuralsupport.ArchDetector
import arx.axistential.game.logic.structuralsupport.StructuralSupportLogic
import arx.axistential.game.logic.structuralsupport.TStructuralSupportLogic
import arx.core.datastructures.SynchronizedQueue
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures._

import scala.collection.mutable
import scalaxy.loops._

/**
 * TODO: We could make all blocks adjacent to blocks that are mined out become affected by support values
 */
class StructuralSupportGameComponent extends GameEngineComponent {
	lazy val archDetector = pio[ArchDetector]
	lazy val structuralSupportLogic = pio[TStructuralSupportLogic]


	val spreadVecs = StructuralSupportLogic.spreadVecs

	private final val archVec = Array(Vec3i.UnitX,Vec3i.UnitY)

	val lastUpdatedByTalea = new mutable.HashMap[ITalea[_],Int]
	val taleaQ = new SynchronizedQueue[ITalea[_]]
	lazy val TD = world.aux[TerrainData]
	lazy val SSD = world.aux[StructuralSupportData]

	override def initialize(): Unit = {
		world.aux[TerrainData].materialGrid.onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				taleae.foreach( t => taleaQ.enqueue(t) )
			}
		}
	}




	def removeArch (arch : Arch) {
		SSD.archMaps(arch.xyi).remove(arch.keystone)
		for (chain <- arch.chains; block <- chain) {
			SSD.setSupport(block,StructuralSupportData.DirtyBit,0)
		}
	}

	def removeStaleArches (v : VoxelCoord) = {
		var archesRemoved = List[Arch]()
		val sourceAbove = SSD.source(v.plusZ(1))
		for (xyi <- 0 until 2 optimized ) {
			if (sourceAbove.isInArch(xyi)) {
				for (keystone <- archDetector.keystonesFor(v.plusZ(1),xyi)) {
					archesRemoved ::= SSD.archMaps(xyi)(keystone)
				}
			}
		}

		archesRemoved.foreach(removeArch)

		archesRemoved
	}


	def subBlockAdded(v : VoxelCoord) = {
		var recalcArches = List[Arch]()
		var addedArches = List[Arch]()
		var recalcBlocks = Vector[VoxelCoord]()

		// this needs to account for the possibility of two X arches sharing voussoir's in a leg
		recalcArches :::= removeStaleArches(v)

		for (xyi <- 0 until 2 optimized) {
			val newArches = archDetector.possibleArchesForBlock(v,xyi)

			for (arch <- newArches) {
				SSD.setSource(arch.keystone,SSD.source(arch.keystone) | StructuralSupportData.KeystoneBits(xyi))
				for (block <- arch.allNonKeystoneBlock) {
					SSD.setSource(block,SSD.source(block) | StructuralSupportData.VoussoirBits(xyi))
				}

				recalcArches ::= arch
				SSD.archMaps(xyi).put(arch.leftChain.last,arch)
				addedArches ::= arch
			}
		}

		SSD.setSource(v,SSD.source(v) & (~StructuralSupportData.RootBlock))

		// we may need to do better ordering than this
		for (arch <- recalcArches.sortBy(a => a.leftChain.head.z min a.rightChain.head.z)) {
			recalcBlocks ++= arch.leftChain
			recalcBlocks ++= arch.rightChain
		}
		recalcBlocks +:= v
		recalcBlocks -> addedArches
	}

	def updateSupport(v : VoxelCoord) : Boolean = {
		val existingSupport = SSD.supportGrid(v)
		val newSupportStruct = structuralSupportLogic.computeSupportValueFor(v)
		val maxSupport = newSupportStruct.support
		if (maxSupport != existingSupport.support) {
			SSD.supportGrid(v) = newSupportStruct.s
			true
		} else {
			false
		}
	}

	def blockAdded(talea: TLoggingTalea[Byte], tpos: ReadVec3i, newByte: Byte) {
		val v = VoxelCoord(talea.position) + tpos
		blockAdded(v,newByte)
	}

	def blockAdded(v : VoxelCoord, newByte : Byte) {
		val (updateCandidates,newArches) = subBlockAdded(v)
		recursivelyUpdateSupport(updateCandidates)

		val badArches = newArches.filterNot( archDetector.verifyArch )
		badArches.foreach(removeArch)
		recursivelyUpdateSupport(badArches.toList.flatMap(a => a.leftChain ::: a.rightChain))
	}

	def blackout (v : VoxelCoord, blackSet : mutable.Set[VoxelCoord], computeQ : mutable.Queue[VoxelCoord]) {
		for (xyi <- 0 to 1 optimized ; keystone <- archDetector.keystonesFor(v,xyi)) {
			val arch = SSD.archMaps(xyi)(keystone)
			removeArch(arch)
			for (av <- arch.allBlocks if ! blackSet(av)) {
				blackSet.add(av)
				computeQ.enqueue(av)
			}
		}

		val preSupportStruct = SSD.supportGrid(v)
		updateSupport(v)
		val postSupportStruct = SSD.supportGrid(v)

		if (postSupportStruct != preSupportStruct) {
			SSD.supportGrid(v) = StructuralSupportData.pack(StructuralSupportData.DirtyBit,0)
			for (q <- 0 until StructuralSupportLogic.NumCoreDirections optimized) {
				val vprime = v + StructuralSupportLogic.cardinals(q)
				if (! blackSet(vprime)) {
					blackSet.add(vprime)
					computeQ.enqueue(vprime)
				}
			}
		}

	}

	def blockRemoved(talea: TLoggingTalea[Byte], tpos: ReadVec3i) = {
		val v = VoxelCoord(talea.position + tpos)
		val curSupportStruct = SSD.supportGrid(v)
		val curSupport = curSupportStruct.support
		val curSource = curSupportStruct.source

		val Q = new mutable.Queue[VoxelCoord]()
		val blackSet = new mutable.HashSet[VoxelCoord]()
		Q.enqueue(v)
		blackSet.add(v)
		while (Q.nonEmpty) {
			blackout(Q.dequeue(),blackSet,Q)
		}

		for (bv <- blackSet.toList.sortBy(_.z) if bv != v) {
			if (SSD.source(bv).nonRootBlock) {
				SSD.supportGrid(bv) = StructuralSupportData.pack(0,1)
				blockAdded(bv,TD.materialByteAt(bv))
			}
		}
	}

	def recursivelyUpdateSupport(updateCandidates : Seq[VoxelCoord]) {
		val q = new mutable.Queue[VoxelCoord]
		q.enqueue(updateCandidates :_*)

		while (q.nonEmpty) {
			val v = q.dequeue()
			val res = updateSupport(v)
			if (res) {
				for (i <- 0 until 10 optimized) {
					q.enqueue(v + spreadVecs(i))
				}
			}
		}
	}

	def updateTaleaModifications(talea: TLoggingTalea[Byte], modifications: List[LoggedTaleaModification[Byte]]) = {
		for (modification <- modifications) {
			// switched to use isSolidTerrain here, objects should not be able to provide support, by default
			val wasSolid = TerrainByteUtils.isSolidTerrain( modification.oldValue )
			val isSolid = TerrainByteUtils.isSolidTerrain( modification.newValue )

			if (isSolid && !wasSolid) {
				blockAdded(talea,modification.position,modification.newValue)
			} else if (!isSolid && wasSolid) {
				blockRemoved(talea,modification.position)
			}
		}
	}

	def updateTalea(talea: TLoggingTalea[Byte]): Unit = {
		val lastUpdated = lastUpdatedByTalea.getOrElse(talea,0)
		val modifications = talea.loggedModifications.takeWhile(_.revision > lastUpdated)
		if (modifications.nonEmpty) {
			lastUpdatedByTalea (talea) = modifications.head.revision

			updateTaleaModifications(talea,modifications)
		}
	}

	override def update(time: UnitOfTime): Unit = {
		while (taleaQ.nonEmpty) {
			taleaQ.dequeueOpt() match {
				case Some(talea) => {
					talea match {
						case lt : TLoggingTalea[Byte] => {
							updateTalea(lt)
						}
						case _ => Noto.warn("StructuralSupportGameComponent cannot handle non-logging taleae")
					}
				}
				case _ => //do nothing
			}
		}
	}
}
