package tonivade.poker

import cats.data.StateT
import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PokerSpec extends AnyFlatSpec with Matchers {
  import Action._
  import GameHand._
  import Hand._
  import HandPhase._
  import Rank._
  import Role._
  import Suit._
  
  val toni: Player = Player("toni")
  val pepe: Player = Player("pepe")
  val paco: Player = Player("paco")

  val game: Game = Game(List(toni, pepe, paco))
  val deck: Deck = Deck.shuffle

  val preFlop: StateT[IO, Deck, GameHand] = nextGameHand(game)
  val flop: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
  } yield flop
  val turn: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
  } yield turn
  val river: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
    river <- nextPhase(turn)
  } yield river
  val showdown: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
    river <- nextPhase(turn)
    showdown <- nextPhase(river)
  } yield showdown

  "Straight value" should "be 4" in {
    Straight.value should be (4)
  }

  "Straight" should "be greater than Pair" in {
    Straight.compare(Pair) should be > 0
  }
  
  "Game Dealer" should "be toni" in {
    game.dealer should be (toni)
  }
  
  "Game SmallBlind" should "be pepe" in {
    game.smallBlind should be (pepe)
  }
  
  "Game BigBlind" should "be paco" in {
    game.bigBlind should be (paco)
  }
  
  "Game Hand" should "start with an empty bet" in {
    val hand = preFlop.runA(deck).unsafeRunSync()
    hand.pot should be (0)
  }
  
  "HandCards with 3 cards" should "have 1 combination" in {
    val cards = HandCards(Card(Clubs, Two), Card(Spades, Three), Card(Hearts, Queen))

    cards.combinations.size should be (1)
  }
  
  "HandCards with 4 cards" should "have 4 combinations" in {
    val cards = HandCards(Card(Clubs, Two), Card(Spades, Three), Card(Hearts, Queen), Some(Card(Diamonds, Jack)))
    
    cards.combinations.size should be (4)
  }
  
  "HandCards with 5 cards" should "have 6 combinations" in {
    val cards = HandCards(Card(Clubs, Two), Card(Spades, Three), Card(Hearts, Queen), Some(Card(Diamonds, Jack)), Some(Card(Hearts, Four)))
    
    cards.combinations.size should be (10)
  }
  
  "GameHand in PreFlop" should "have no winner" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), None)
    
    hand.winner should be (None)
  }
  
  "GameHand with ThreeOfAKind" should "win against Pair" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val cards = HandCards(Card(Diamonds, Ace), Card(Hearts, Three), Card(Spades, Jack))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), Some(cards))
    
    val (winner, winnerHand) = hand.winner.get

    winnerHand.bestHand should be (ThreeOfAKind)
    winner should be (toni)
  }
  
  "GameHand with FullHouse" should "win against ThreeOfAKind" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val cards = HandCards(Card(Diamonds, Ace), Card(Spades, King), Card(Spades, Jack), Some(Card(Hearts, King)))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), Some(cards))
    
    val (winner, winnerHand) = hand.winner.get

    winnerHand.bestHand should be (FullHouse)
    winner should be (pepe)
  }
  
  "GameHand" should "start with a state PreFlop" in {
    val hand = preFlop.runA(deck).unsafeRunSync()

    hand.phase should be (PreFlop)
  }
  
  "GameHand" should "Flop follow PreFlop" in {
    val hand = flop.runA(deck).unsafeRunSync()

    hand.phase should be (Flop)
  }
  
  "GameHand" should "Turn follow Flop" in {
    val hand = turn.runA(deck).unsafeRunSync()

    hand.phase should be (Turn)
  }
  
  "GameHand" should "River follow Turn" in {
    val hand = river.runA(deck).unsafeRunSync()

    hand.phase should be (River)
  }
  
  "GameHand" should "Showdown follow River" in {
    val hand = showdown.runA(deck).unsafeRunSync()

    hand.phase should be (Showdown)
  }
  
  "GameHand" should "be updated with two Raises two Calls to 6" in {
    val hand = preFlop.runA(deck).unsafeRunSync()

    val newHand = hand
        .update(pepe, Raise(1))
        .update(paco, Raise(1))
        .update(toni, Call)
        .update(pepe, Call)
        
    newHand.pot should be (6)
    newHand.maxBet should be (2)
  }
  
  "Bet turn" should "be next after Dealer Player" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.turn should be (paco)
  }
  
  "Bet turn nextTurn" should "be next Player" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.nextTurn.turn should be (pepe)
  }
  
  "Bet turn nextTurn nextTurn" should "be next Player" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.nextTurn.nextTurn.turn should be (toni)
  }
  
  "Bet turn in PreFlop for BigBlind Player" should "only can Raise" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.options(pepe) should be (List(Raise(1)))
  }
  
  "Bet turn in PreFlop for BigBlind Player" should "do any Action after first round" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).copy(bets = List(Raise(1), Raise(1), Call))
    
    bets.options(paco) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in PreFlop for SmallBlind Player" should "only can Raise" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.options(paco) should be (List(Raise(1)))
  }
  
  "Bet turn in PreFlop for SmallBlind Player" should "do any Action after first round" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).copy(bets = List(Raise(1), Raise(1), Call))
    
    bets.options(paco) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in Flop for SmallBlind Player" should "do any Action after first round" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).copy(bets = List(Raise(1), Raise(1), Call))
    
    bets.options(paco) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in PreFlop for Dealer Player" should "do any Action" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.options(toni) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in PreFlop for Dealer Player with Raise" should "do any Action but Call instead of Check" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).update(pepe, Raise(1)).update(paco, Raise(1))
    
    bets.options(toni) should be (List(Fold, Call, Raise(1)))
  }
}