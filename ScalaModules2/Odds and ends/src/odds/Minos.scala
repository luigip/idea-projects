package odds

object Minos extends App{
  
  sealed abstract class Fighter (var HP: Int)
  case object Altheus extends Fighter(27)
  case object Kremton extends Fighter(50)
  
  val rand = new java.util.Random()
  
  sealed abstract class Zone(val damage: Int)
  case object Body extends Zone(1)
  case object Groin extends Zone(2)
  case object Head extends Zone(3)
  
  val zones = List(Body, Groin, Head)
  
  val doPrint = false
  def cprintln(a: Any) { if (doPrint) println(a)}
  def cprint(a: Any) {if (doPrint) print(a)}
  
  def attack(attacker: Fighter, attackZone: Zone, defender: Fighter, defendZone: Zone) = {
//    cprint(attacker + " goes for the " + attackZone + "... " + defender + " defends " + defendZone)
    if (attackZone == defendZone) {
//      cprintln("... miss!")
      true
    } 
    else { 
//      cprintln("... hit! " + "for " + attackZone.damage)
      defender.HP -= attackZone.damage
      false 
    } 
  } 
  
  def getAttackZone: Zone = rand.nextInt(6) + 1 match {
    case 1 => Head
    case 2 | 3 => Groin
    case 4 | 5 | 6 => Body
  }
  
  def getDefendZone: Zone = rand.nextInt(6) + 1 match {
    case 1 => Body
    case 2 | 3 => Groin
    case 4 | 5 | 6 => Head    
  }
  
  def fight(active: Fighter, alAttack: Zone, alDefend: Zone): Fighter = { 
    var nextActive = active
//    cprint("Altheus: " + Altheus.HP + "  Kremton: " + Kremton.HP + " --- ")
//    cprintln(active + " attacks...")
    if(Altheus.HP <= 0) Kremton
    else if(Kremton.HP <= 0) Altheus
    else {
      if (
        active match {
          case Altheus => {
            attack(Altheus, alAttack, Kremton, getDefendZone)
          }
          case Kremton => {
            attack(Kremton, getAttackZone, Altheus, alDefend)
          }
        }
      ) {
        nextActive = if (active == Altheus) Kremton else Altheus
      }
      fight(nextActive, alAttack, alDefend)
    }
  }
  
  val trials = 1000000
  timed {
    for {
    //    a <- List(Body)
    //    d <- List(Body)
      a <- zones
      d <- zones
    } {
      println("---- Attacking " + a + ", defending " + d + " ----")
      val wins = (1 to trials).map(_ => {
        Altheus.HP = 30
        Kremton.HP = 50
        fight(Altheus, a, d)
      }).count(Altheus ==)
      val pct = wins / trials.toDouble
      println("Win: %.2f %%" format (pct * 100))
    }
  }

  def timed[A](f: =>A) = {
    val t0 = System.currentTimeMillis()
    f
    val t1 = System.currentTimeMillis()
    println((t1-t0)/1000.0 + " seconds")
  }

}