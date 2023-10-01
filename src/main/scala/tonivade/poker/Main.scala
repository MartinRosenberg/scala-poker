package tonivade.poker

@main
def main(): Unit = {
  val players = List(Player("Pepe"), Player("Paco"), Player("Toni"), Player("Curro"), Player("Perico"))

  val result = GameHand.runHandLoop(new Game(players))

  println(result.players)
  println(result.winner)
  println(result.winner.map(_._2.bestHand))
}
