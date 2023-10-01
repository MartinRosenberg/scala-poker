package tonivade.poker

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.data.StateT
import cats.data.StateT._
import cats.Monad

enum HandPhase {
  case PreFlop, Flop, Turn, River, Showdown
}

enum Role {
  case Regular, Dealer, SmallBlind, BigBlind, Folded
}

enum Action {
  case Fold, Check, Call, AllIn
  case Raise(raise: Int)
}

case class Player(name: String, wallet: Int = 500)

object Player {
  import Console._

  type Winner = (Player, FullHand)
  
  def speak[S](player: Player): StateT[IO, S, Action] = 
    for {
      _ <- print(s"${player.name} turn")
      string <- read
    } yield toAction(string).getOrElse(Action.Check)
    
  def toAction(string: String): Option[Action] = 
    string match {
      case "fold" => Some(Action.Fold)
      case "check" => Some(Action.Check)
      case "allin" => Some(Action.AllIn)
      case "call" => Some(Action.Call)
      case "raise" => Some(Action.Raise(1))
      case _ => None
    }
}

case class HandCards(card1: Card, card2: Card, card3: Card, card4: Option[Card] = None, card5: Option[Card] = None) {
  def setCard4(card: Card): HandCards = copy(card4 = Some(card))
  def setCard5(card: Card): HandCards = copy(card5 = Some(card))
  
  def combinations: List[List[Card]] = toList.combinations(3).toList
  
  private lazy val toList: List[Card] = List(card1, card2, card3) ++ card4.toList ++ card5.toList
}

case class PlayerHand(player: Player, role: Role, card1: Card, card2: Card, bet: Int = 0) {
  def bestHand(cards: HandCards): Player.Winner =
    (player, hands(cards).reduce((p1, p2) => if (p1.bestHand > p2.bestHand) p1 else p2))
    
  def remaining: Int = player.wallet - bet
  
  def fold: PlayerHand = copy(role = Role.Folded)
  def allIn: PlayerHand = copy(bet = player.wallet)
  def update(value: Int): PlayerHand = copy(bet = bet + value)

  private def hands(cards: HandCards): List[FullHand] = 
    for {
      combination <- cards.combinations
    } yield FullHand(card1, card2, combination(0), combination(1), combination(2))
}

case class GameHand(phase: HandPhase, players: List[PlayerHand], cards: Option[HandCards]) {
  lazy val pot: Int = players.map(_.bet).sum
  lazy val maxBet: Int = players.map(_.bet).max
  lazy val notFolded: List[PlayerHand] = players.filter(_.role != Role.Folded)

  def toPhase(phase: HandPhase): GameHand = copy(phase = phase)
  def setFlop(cards: HandCards): GameHand = copy(cards = Some(cards))
  def setTurn(card: Card): GameHand = copy(cards = cards.map(_.setCard4(card)))
  def setRiver(card: Card): GameHand = copy(cards = cards.map(_.setCard5(card)))
  
  def update(player: Player, action: Action): GameHand =
    action match {
      case Action.Fold => fold(player)
      case Action.Check => check(player)
      case Action.AllIn => allIn(player)
      case Action.Call => call(player)
      case Action.Raise(value) => raise(player, value)
    }

  def find(player: Player): Option[PlayerHand] = players.find(_.player == player)
  
  def winner: Option[Player.Winner] =
    cards.map(c => players.map(_.bestHand(c)).reduce((p1, p2) => if (p1._2 > p2._2) p1 else p2))
  
  private def fold(player: Player): GameHand = update(player)(_.fold)
  private def check(player: Player): GameHand = update(player)(identity)
  private def allIn(player: Player): GameHand = update(player)(_.allIn)
  private def call(player: Player): GameHand = 
    diff(player).map(value => update(player)(_.update(value))).getOrElse(this)
  private def raise(player: Player, raise: Int): GameHand = 
    diff(player).map(value => update(player)(_.update(value + raise))).getOrElse(this)
  
  private def update(player: Player)(action: PlayerHand => PlayerHand): GameHand = 
    copy(players = updatePlayer(player)(action))
    
  private def updatePlayer(player: Player)(action: PlayerHand => PlayerHand): List[PlayerHand] = 
    players.map {
      playerHand => if (playerHand.player == player) action(playerHand) else playerHand
    }
  
  private def diff(player: Player): Option[Int] =
    players.find(_.player == player).map(maxBet - _.bet).filter(_ >= 0)
}

object GameHand {
  import Console._

  def runHandLoop(game: Game): GameHand = 
    handLoop(game).runA(Deck.shuffle).unsafeRunSync()
  
  def handLoop(game: Game): StateT[IO, Deck, GameHand] = 
    for {
      _ <- print(game)
      preFlop <- nextGameHand(game)
      flop <- phaseLoop(preFlop)
      turn <- phaseLoop(flop)
      river <- phaseLoop(turn)
      showdown <- phaseLoop(river)
    } yield showdown
  
  def nextGameHand(game: Game): StateT[IO, Deck, GameHand] = 
    for {
      players <- playerList(game)
    } yield GameHand(HandPhase.PreFlop, players, None)
    
  def phaseLoop(hand: GameHand): StateT[IO, Deck, GameHand] = 
    for {
      _ <- print(s"current pot ${hand.pot} in ${hand.phase}")
      _ <- print(hand)
      bets <- BetTurn.runBetLoop(hand)
      _ <- print(bets)
      nextHand <- nextPhase(bets)
    } yield nextHand
  
  def nextPhase(current: GameHand): StateT[IO, Deck, GameHand] = 
    current.phase match {
      case HandPhase.PreFlop => toFlop(current)
      case HandPhase.Flop => toTurn(current)
      case HandPhase.Turn => toRiver(current)
      case HandPhase.River => toShowdown(current)
      case HandPhase.Showdown => pure(current)
    }

  def winner(current: GameHand): StateT[IO, Deck, Option[Player.Winner]] =
    pure(current.winner)
  
  private def toFlop(current: GameHand): StateT[IO, Deck, GameHand] = 
    for {
      cards <- Deck.burnAndTake3
    } yield current.toPhase(HandPhase.Flop).setFlop(cards)
  
  private def toTurn(current: GameHand): StateT[IO, Deck, GameHand] =
    for {
      card <- Deck.burnAndTake 
    } yield current.toPhase(HandPhase.Turn).setTurn(card)
  
  private def toRiver(current: GameHand): StateT[IO, Deck, GameHand] =
    for {
      card <- Deck.burnAndTake 
    } yield current.toPhase(HandPhase.River).setRiver(card)
    
  private def toShowdown(current: GameHand): StateT[IO, Deck, GameHand] =
    pure(current.toPhase(HandPhase.Showdown))

  private def playerList(game: Game): StateT[IO, Deck, List[PlayerHand]] = 
    game.players.map(newPlayerHand(_, game))
      .foldLeft(pure[IO, Deck, List[PlayerHand]](Nil))((sa, sb) => map2(sa, sb)((acc, b) => acc :+ b))

  private def newPlayerHand(player: Player, game: Game): StateT[IO, Deck, PlayerHand] = 
    for {
      role <- pure[IO, Deck, Role](game.playerRole(player))
      card1 <- Deck.take
      card2 <- Deck.take
    } yield PlayerHand(player, role, card1, card2)
  
  private def map2[F[_]: Monad, S, A, B, C](sa: StateT[F, S, A], sb: StateT[F, S, B])(map: (A, B) => C): StateT[F, S, C] = 
    sa.flatMap(a => sb.map(b => map(a, b)))
}

case class BetTurn(hand: GameHand, players: List[Player], bets: List[Action] = Nil) {
  lazy val allPlayersSpeak: Boolean = bets.filterNot(_ == Action.Fold).size >= hand.notFolded.size
  lazy val allBetsBalanced: Boolean = hand.notFolded.forall(_.bet == hand.maxBet)
  lazy val noMoreBets: Boolean = allPlayersSpeak && allBetsBalanced
  
  lazy val turn: Player = players.head
  lazy val nextTurn: BetTurn = copy(players = players.tail :+ players.head)
  
  def options(player: Player): List[Action] = 
    hand.find(player) match {
      case Some(playerHand) => 
        playerHand.role match {
          case Role.BigBlind => blind(playerHand) :+ Action.Raise(1)
          case Role.SmallBlind => blind(playerHand) :+ Action.Raise(1)
          case Role.Dealer => regular(playerHand) :+ Action.Raise(1)
          case Role.Regular => regular(playerHand) :+ Action.Raise(1)
          case Role.Folded => Nil
        }
      case None => Nil
    }
    
  def canBet(player: Player): Boolean = hand.find(player).exists(_.role != Role.Folded)
  
  def update(player: Player, action: Action): BetTurn = 
    copy(hand = hand.update(player, action), bets = bets :+ action)
  
  private def blind(playerHand: PlayerHand): List[Action] = 
    if (hand.phase != HandPhase.PreFlop || allPlayersSpeak) regular(playerHand) else Nil
  private def regular(playerHand: PlayerHand): List[Action] = 
    List(Action.Fold, if (canCall(playerHand)) Action.Call else Action.Check)
  private def canCall(playerHand: PlayerHand): Boolean = hand.maxBet > playerHand.bet
}

object BetTurn {
  import Console._
  
  def runBetLoop(hand: GameHand): StateT[IO, Deck, GameHand] = 
    pure(betLoop.runS(from(hand)).unsafeRunSync().hand)
  
  val betLoop: StateT[IO, BetTurn, Unit] = 
    for {
      _ <- nextTurn
      player <- turn
      canBet <- canBet(player)
      _ <- if (canBet) playerTurn(player) else noop[BetTurn]
      end <- betOver
      _ <- if (end) noop[BetTurn] else betLoop
    } yield ()
  
  def playerTurn(player: Player): StateT[IO, BetTurn, Unit] = 
    for {
      options <- options(player)
      _ <- print(s"$player can $options")
      action <- Player.speak(player)
      _ <- update(player, action)
      _ <- print(s"${player.name} has $action")
    } yield ()
  
  def from(hand: GameHand): BetTurn = BetTurn(hand, hand.players.map(_.player))

  def betOver: StateT[IO, BetTurn, Boolean] = inspect(_.noMoreBets)
  def canBet(player: Player): StateT[IO, BetTurn, Boolean] = inspect(_.canBet(player))
  
  def turn: StateT[IO, BetTurn, Player] = inspect(_.turn)
  def nextTurn: StateT[IO, BetTurn, Unit] = modify(_.nextTurn)  
  def options(player: Player): StateT[IO, BetTurn, List[Action]] = inspect(_.options(player))
  
  def update(player: Player, action: Action): StateT[IO, BetTurn, Unit] = modify(_.update(player, action))
}

case class Game(players: List[Player], round: Int = 1) {
  def dealer: Player = players.head
  def smallBlind: Player = players.tail.head
  def bigBlind: Player = players.tail.tail.head

  def next: Game = {
    val newPlayers = players.filter(_.wallet > 0)
    Game(newPlayers.tail :+ newPlayers.head, round + 1)
  }
  
  def playerRole(player: Player): Role = 
    player match {
      case Player(name, _) if name == dealer.name => Role.Dealer
      case Player(name, _) if name == smallBlind.name => Role.SmallBlind
      case Player(name, _) if name == bigBlind.name => Role.BigBlind
      case _ => Role.Regular
    }
}

object Game {
  import Console._
  
  def start(players: List[Player]): StateT[IO, Game, Unit] = set(Game(players))
  
  def next(game: Game): StateT[IO, Game, Unit] = set(game.next)
  
  def readPlayer[S]: StateT[IO, S, Player] =
    for {
      _ <- print("enter player name")
      name <- read
    } yield Player(name)
  
  def addPlayer(player: Player): StateT[IO, List[Player], Unit] = modify(_ :+ player)

  def playersLoop: StateT[IO, List[Player], Unit] = 
    for {
      player <- readPlayer
      _ <- addPlayer(player)
      _ <- print("do you want to add more players")
      more <- read
      _ <- if (more == "y") playersLoop else noop[List[Player]]
    } yield ()
}

object Console {
  import scala.io.StdIn.readLine
  
  def noop[S]: StateT[IO, S, Unit] = pure(())

  def print[S](value: Any): StateT[IO, S, Unit] = pure(println(value))
  
  def read[S]: StateT[IO, S, String] = pure(readLine())
}
