package tonivade.poker

@main
def main(): Unit = {
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))

  val result = GameHand.runHandLoop(new Game(players))

  println(result.players)
  println(result.winner)
  println(result.winner.map(_._2.bestHand))
}
