package arx.tmp.game.logic.entities.core

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/2/15
 * Time: 8:05 AM
 */

import arx.tmp.game.logic.entities.data.TAuxData

import scala.collection.mutable

trait THasAuxData[U <: TAuxData] {
	var _auxData = new mutable.HashMap[Class[_],U]()

	def auxData[T <: U : Manifest] : T = {
		val man = manifest[T].runtimeClass
		if ( _auxData.contains(man) ) {
			_auxData(man).asInstanceOf[T]
		} else {
			fallback match {
				case Some(f) =>
					if (f.hasAuxData) {
						f.auxData[T]
					} else {
						val n = ReflectionAssistant.instantiate(manifest[T])
						addAuxData(n)
						n
					}
				case None =>
					val n = ReflectionAssistant.instantiate(manifest[T])
					addAuxData(n)
					n
			}
		}
	}
	protected def onNewAuxDataCreated ( gead : U ) {}
	def auxDataOrElse[T <: TAuxData : Manifest](orElse : T) : T = {
		val man = manifest[T].runtimeClass
		fallback match {
			case None =>
				_auxData.getOrElse(man,orElse).asInstanceOf[T]
			case Some(f) =>
				_auxData.get(man) match {
					case Some(found) => found.asInstanceOf[T]
					case None => f._auxData.getOrElse(man,orElse).asInstanceOf[T]
				}
		}
	}
	/** Returns an auxData that descends from the given type, if one exists */
	def auxDataWithTrait [T : Manifest] = _auxData.values.find(v => manifest[T].runtimeClass.isAssignableFrom(v.getClass)).asInstanceOf[Option[T]]

	def removeAuxData[T <: TAuxData : Manifest]() { _auxData -= manifest[T].runtimeClass }
	def removeAuxData(clazz : Class[_]) { _auxData -= clazz }
	def manualAddAuxData(d : U): Unit = {
		addAuxData(d)
	}
	def manualAddAuxDataAs[T <: TAuxData : Manifest] (d : U): Unit = {
		addAuxData(d)
		_auxData(manifest[T].runtimeClass) = d
	}
	protected def addAuxData(d : U): Unit = {
		onNewAuxDataCreated(d)
		d.onAssignedToObject(this)

//		var curClass = d.getClass
//		while (curClass != null) {
			_auxData(d.getClass) = d
//		}
	}

	def aux [T <: U : Manifest] = auxData[T]

	def auxDataOpt[T <: TAuxData : Manifest] : Option[T] = {
		val man = manifest[T].runtimeClass
		if ( _auxData.contains(man) ) {
			Some(_auxData(man).asInstanceOf[T])
		} else {
			fallback match {
				case None => None
				case Some(f) => f.auxDataOpt[T]
			}
		}
	}
	def hasAuxData[T <: TAuxData : Manifest] = _auxData.contains(manifest[T].runtimeClass)

	def allAuxiliaryData = _auxData.values.toList

	def fallback : Option[THasAuxData[U]] = None
}
