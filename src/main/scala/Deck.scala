package tonivade.poker

import cats.data.StateT
import cats.data.StateT.*
import cats.effect.IO

import scala.util.Random

enum Suit {
  case Clubs, Spades, Diamonds, Hearts
}

enum Rank extends Ordered[Rank] {
  def compare(that: Rank): Int = this.ordinal - that.ordinal

  case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace
}

case class Card(suit: Suit, rank: Rank)

object Card {
  val all: Seq[Card] = for {
    suit <- Suit.values
    rank <- Rank.values
  } yield Card(suit, rank)
}

case class Deck(cards: Seq[Card]) {
  def take: Card = cards.head
  def burn: Deck = Deck(cards.tail)
}

object Deck {
  def ordered: Deck = Deck(Card.all)
  def shuffle: Deck = Deck(Random.shuffle(Card.all))

  def burnAndTake: StateT[IO, Deck, Card] =
    for {
      _ <- burn
      card <- take
    } yield card

  def burnAndTake3: StateT[IO, Deck, HandCards] =
    for {
      _ <- burn
      card1 <- take
      card2 <- take
      card3 <- take
    } yield HandCards(card1, card2, card3)

  def take: StateT[IO, Deck, Card] = StateT {
    deck => IO(deck.burn, deck.take)
  }

  private def burn: StateT[IO, Deck, Unit] = modify(_.burn)
}