package arx.tmp.game.logic.world.util

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

import overlock.atomicmap.AtomicMap

import scala.collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 9/24/11
 * Time: 10:29 AM
 * Created by nonvirtualthunk
 */

//@SerialVersionUID(1L)
trait TIdentifiable {
	def identifier : String
}
@SerialVersionUID(1L)
class EntityMapper[T <: TIdentifiable](defaultEntity: T) extends AnyRef with Iterable[T] with Externalizable{
	def this () { this(null.asInstanceOf[T]) }
	var entities = new ArrayBuffer[T]
	var manifests = new ArrayBuffer[Manifest[_]]
	var entityToIndex = AtomicMap.atomicNBHM[String,Byte]
	var manifestToIndex = AtomicMap.atomicNBHM[Manifest[_],Byte]
	if ( defaultEntity != null ){
		entities += defaultEntity
		entityToIndex.put(defaultEntity.identifier,0.toByte)
	}
	def add[E <: T : Manifest] (ent: E): Byte = {
		entityToIndex.getOrElseUpdate(ent.identifier,{
			entities += ent
			(entities.size - 1).toByte
		})
	}
	def apply[E <: T : Manifest] (ent: E): Byte = { add(ent) }
	def apply (index: Int): T = { entities(index) }
	def get (index: Int): T = { entities(index) }
	def get (index: Byte): T = { entities(index) }
	def get[E <: T : Manifest] (c : Class[E]) : Byte = { add(c.newInstance()) }

	def iterator: Iterator[T] = { entities.iterator }
	override def size: Int = { entities.size }

	def writeExternal(p1: ObjectOutput) {
		p1.writeObject(entities)
		p1.writeObject(manifests)
		p1.writeNBHM(entityToIndex)
		p1.writeNBHM(manifestToIndex)
	}

	def readExternal(p1: ObjectInput) {
		entities = p1.readAs[ArrayBuffer[T]]
		manifests = p1.readAs[ArrayBuffer[Manifest[_]]]
		entityToIndex = p1.readNBHM[String,Byte]
		manifestToIndex = p1.readNBHM[Manifest[_],Byte]
	}
}