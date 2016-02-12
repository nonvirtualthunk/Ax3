package arx.tmp.game.logic.entities.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/8/11
 * Time: 7:26 PM
 * Created by nonvirtualthunk
 */

@SerialVersionUID(1L)
class GameVariable(var _maxValue: Float) extends Serializable{
	def this ( d: Double ) { this(d.toFloat) }
	var restorationRate: Float = 0.0f
	var depletionRate: Float = 0.0f

	var _up: Float = 0.0f
	var _down: Float = 0.0f

	def damage: Float = _down
	def downBy: Float = _down - _up
	def bonus: Float = _up
	def upBy: Float = _up - _down

	def current: Float = _maxValue + _up - _down

	def maximum: Float = _maxValue
	def maximum_=(f: Float){ _maxValue = f }

	def restore (){
		_up = scala.math.max(0.0f,_up - depletionRate)
		_down = scala.math.max(0.0f,_down - restorationRate)
	}

	//========================================
	def setMax ( f: Float ): GameVariable = { _maxValue = f;this }
	def setDamage ( f: Float ): GameVariable = { _down = f; this }
	def setBonus ( f: Float ): GameVariable = { _up = f;this }

	//========================================
	def withMaximum ( f: Float ): GameVariable = { val copy = GameVariable(this);copy.maximum = f;copy }
	def withDamage ( f: Float ): GameVariable = { val copy = GameVariable(this);copy._down = f;copy }
	def withBonus ( f: Float ): GameVariable = { val copy = GameVariable(this);copy._up = f;copy }

	def withMaximum: MaximumFocusedGameVariable = { val copy = new MaximumFocusedGameVariable;copy.copyFrom(this);copy }
	//========================================
	def withMaximumPlus ( f: Float ): GameVariable = { val copy = GameVariable(this);copy.maximum = (this.maximum + f);copy }
	def withMaximum_+ ( f: Float ): GameVariable = { val copy = GameVariable(this);copy.maximum = (this.maximum + f);copy }
	def withDamagePlus ( f: Float ): GameVariable = { val copy = GameVariable(this);copy._down = (this._down + f);copy }
	def withBonusPlus ( f: Float ): GameVariable = { val copy = GameVariable(this);copy._up = f;(this._up + f);copy }

	def copyFrom (gv: GameVariable): GameVariable = {
		this._up = gv._up
		this._down = gv._down
		this.restorationRate = gv.restorationRate
		this.depletionRate = gv.depletionRate
		this
	}

	def withAdded ( m : GameVariable.Modifier ) : GameVariable = {
		m match {
			case GameVariable.Bonus(b) => this._up += b;this
			case GameVariable.Damage(d) => this._down += d;this
			case GameVariable.Maximum(m) => this._maxValue += m;this
		}
	}
	def withSubtracted ( m : GameVariable.Modifier ) : GameVariable = {
			m match {
				case GameVariable.Bonus(b) => this._up -= b;this
				case GameVariable.Damage(d) => this._down -= d;this
				case GameVariable.Maximum(m) => this._maxValue -= m;this
			}
		}
	def withNew ( m : GameVariable.Modifier ) : GameVariable = {
		m match {
			case GameVariable.Bonus(b) => this._up = b;this
			case GameVariable.Damage(d) => this._down = d;this
			case GameVariable.Maximum(m) => this._maxValue = m;this
		}
	}
	def + ( m : GameVariable.Modifier ) : GameVariable = {
		m match {
			case GameVariable.Bonus(b) => val copy = GameVariable(this);copy._up += b;copy
			case GameVariable.Damage(d) => val copy = GameVariable(this);copy._down += d;copy
			case GameVariable.Maximum(m) => val copy = GameVariable(this);copy._maxValue += m;copy
		}
	}
	def + ( m : GameVariable ) : GameVariable = {
		val ret = GameVariable(this);
		ret._maxValue += m._maxValue
		ret._up += m._up
		ret._down += m._down
		ret
	}
	def - ( m : GameVariable ) : GameVariable = {
		val ret = GameVariable(this);
		ret._maxValue -= m._maxValue
		ret._up -= m._up
		ret._down -= m._down
		ret
	}
	def * ( f : Float ) : GameVariable = {
		val ret = GameVariable(this)
		ret._maxValue *= f
		ret._up *= f
		ret._down *= f
		ret
	}
	override def toString = "(max : " + maximum + ", current : " + current + ")"
}

class MaximumFocusedGameVariable extends GameVariable(0.0f) {
	def + (f: Float): MaximumFocusedGameVariable = { val copy = new MaximumFocusedGameVariable();copy.copyFrom(this);copy.maximum += f;copy }
	def - (f: Float): MaximumFocusedGameVariable = { val copy = new MaximumFocusedGameVariable();copy.copyFrom(this);copy.maximum -= f;copy }
}

object GameVariable{
	implicit def Double2GameVariable ( a: Double ) : GameVariable = {
		new GameVariable(a.toFloat)
	}
	implicit def Int2GameVariable( a: Int ) : GameVariable = {
		new GameVariable(a.toFloat)
	}
//	def apply (f: Float): GameVariable = { new GameVariable(f) }
	def apply (f: Float): Float = f
	def apply (gv: GameVariable): GameVariable = {
		new GameVariable(gv._maxValue).copyFrom(gv)
	}

	class Modifier {}
	case class Bonus ( f : Float ) extends Modifier
	case class Damage ( f : Float ) extends Modifier
	case class Maximum ( f : Float ) extends Modifier
}
object Damageable{
//	def apply (f: Float): GameVariable = { new GameVariable(f) }
	def apply (f: Float): Float = f
}