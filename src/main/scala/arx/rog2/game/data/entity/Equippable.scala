package arx.rog2.game.data.entity

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/5/17
  * Time: 7:17 AM
  */

import arx.Prelude._
import arx.application.Noto

import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.TGameEntity
import enumeratum._

class Equippable extends TGameEntityAuxData {
	protected var self : TGameEntity = _

	var allowedSlots = Set[BodySlotType]()
	protected var _equippedOn = none[TGameEntity]
	protected var _equippedToSlot = none[BodySlot]

	def equippedOn = _equippedOn
	def equippedToSlot = _equippedToSlot

	def equipOn(ent : TGameEntity, slot : BodySlot): Boolean = {
		ent.auxDataOpt[Equipper] match {
			case Some(equipper) =>
				if (equipper.slots.contains(slot) && allowedSlots.contains(slot.slotType)) {
					if (equipper.equippedEntities.contains(slot)) {
						Noto.warn(s"Attempted to equip an $self to an occupied slot on $ent")
						false
					} else {
						equipper._equippedEntities += slot -> self
						_equippedOn = Some(ent)
						_equippedToSlot = Some(slot)
						self.auxDataOpt[Physical] match {
							case Some(pd) => pd.heldIn = Some(ent)
							case None => Noto.warn("Equipping a non-physical entity. It's possible, it's just strange")
						}
						true
					}
				} else {
					Noto.info(s"Attempted to equip invalid equippable/slot/entity combination, $self, $ent, $slot")
					false
				}
			case _ =>
				Noto.error(s"Cannot equip an entity to a non-equipper entity : $ent")
				false
		}
	}

	def unequip() : Boolean = {
		_equippedOn match {
			case Some(onEnt) =>
				_equippedToSlot match {
					case Some(slot) =>
						onEnt.auxDataOpt[Equipper] match {
							case Some(equipper) =>
								if (equipper.equippedEntities(slot) == self) {
									equipper._equippedEntities -= slot
									_equippedOn = None
									_equippedToSlot = None
									self.auxDataOpt[Physical] match {
										case Some(pd) => pd.heldIn = None
										case None => Noto.warn("Un-Equipping a non-physical entity. It's possible, it's just strange")
									}
									true
								} else {
									Noto.error(s"Mismatch between equipped $self and $equipper on contents of slot")
									false
								}
							case None =>
								Noto.error(s"Equipped to entity $onEnt somehow doesn't have equipper data?")
								false
						}
					case None =>
						Noto.warn(s"Attempting to unequip $self from $onEnt, but no slot")
						false
				}
			case None =>
				Noto.warn(s"Attempting to unequip $self, but not equipped to an entity")
				false
		}
	}

	override def onAssignedToEntity(entity: TGameEntity): Unit = {
		self = entity
	}
}

class Equipper extends TGameEntityAuxData {
	protected var self : TGameEntity = _

	var slots = Set[BodySlot]()
	protected[entity] var _equippedEntities = Map[BodySlot, TGameEntity]()
	def equippedEntities = _equippedEntities

	def equip(ent : TGameEntity, toSlot : BodySlot): Boolean = {
		ent.auxDataOpt[Equippable] match {
			case Some(equippable) =>
				equippable.equipOn(self, toSlot)
			case None =>
				Noto.error(s"Cannot equip a non-equippable entity : $ent")
				false
		}
	}

	override def onAssignedToEntity(entity: TGameEntity): Unit = {
		self = entity
	}
}

case class BodySlot(slotType : BodySlotType, qualifier : BodySlotQualifier = BodySlotQualifier.None) {
	override def toString: String = qualifier match {
		case BodySlotQualifier.None => slotType.entryName.toLowerCase
		case q => qualifier.entryName.toLowerCase + " " + slotType.entryName.toLowerCase()
	}
}


sealed trait BodySlotQualifier extends EnumEntry

object BodySlotQualifier extends Enum[BodySlotQualifier] {
	val values = findValues

	case object Left extends BodySlotQualifier
	case object Right extends BodySlotQualifier
	case object Upper extends BodySlotQualifier
	case object Lower extends BodySlotQualifier
	case object None extends BodySlotQualifier
}


sealed trait BodySlotType extends EnumEntry

object BodySlotType extends Enum[BodySlotType] {
	val values = findValues

	case object Head extends BodySlotType
	case object Arm extends BodySlotType
	case object Leg extends BodySlotType
	case object Torso extends BodySlotType
	case object Hand extends BodySlotType
}