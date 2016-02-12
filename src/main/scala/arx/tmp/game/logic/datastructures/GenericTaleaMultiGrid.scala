package arx.tmp.game.logic.datastructures


/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/24/11
 * Time: 2:01 PM
 * Created by nonvirtualthunk
 */
/*
class GenericTaleaGrid[@specialized(Byte) T,TaleaType <: ITalea[T]]( var newTaleaFunc: (Vec3i) => TaleaType )(implicit man : Manifest[TaleaType])
	extends TInfiniteVoxelStore[T] with Externalizable with TSubContainerStore[TaleaType]
 */
class GenericTaleaMultiGrid ( var taleaGrids : List[TSubContainerStore[ITalea[_]]] )extends TSubContainerStore[List[ITalea[_]]]{
	def getSubContainer(x: Int, y: Int, z: Int) : List[ITalea[_]] = {
		taleaGrids.map( _.getSubContainer(x,y,z) )
	}
	def subContainerDimension = Talea.dimension
	def subContainerDimensionPo2 = Talea.dimensionPo2
}