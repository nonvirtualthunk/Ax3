package arx.axistential.game.components.fluid

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/13
 * Time: 2:19 PM
 */

class FloodFillQueue {
	var power = 0
	var capAND = 0
	var capacity = 0
	var front = 0
	var lastFront = 0
	var back = 0
	var backingArray : Array[Int] = null
	var intsPerElement = 4
	var elementsInUse = 0
	var elementCapacity = capacity / intsPerElement

	setPower(15)

	def setPower ( p : Int ) {
		power = p
		capAND = (1 << power) - 1
		capacity = (1 << power)
		val newBackingArray = Array.ofDim[Int](capacity)
		if ( backingArray != null ) {
			Array.copy(backingArray,0,newBackingArray,0,backingArray.length)
		}
		backingArray = newBackingArray
		elementCapacity = capacity / intsPerElement
	}

	def nonEmpty = front != back
	def enqueue( x : Int , y : Int , z : Int , pressure : Int) {
		elementsInUse += 1
		if ( elementsInUse >= elementCapacity ) {
			setPower(power+1)
		}
		backingArray(back+0) = x
		backingArray(back+1) = y
		backingArray(back+2) = z
		backingArray(back+3) = pressure
		back = (back + 4) & capAND
	}
	def dequeue() {
		lastFront = front
		front = (front + 4) & capAND
		elementsInUse -= 1
	}
	def clear () {
		front = back
		elementsInUse = 0
	}

	def x = backingArray(lastFront)
	def y = backingArray(lastFront+1)
	def z = backingArray(lastFront+2)
	def pressure = backingArray(lastFront+3)
}