(** Abstract logic behind any person at the table *)

type hand = {
  card1 : Card.card;
  card2 : Card.card;
}
(** Abstract type representing the two cards the player has. *)

type state
(** The current state of the player. *)

val create_hand : Card.card -> Card.card -> hand
(** Creates the hand with the players' two cards. *)

val cards_to_ints : Card.card list -> int list
(** Converts a list of cards into a list of their integer values. *)

val get_cards : hand -> Card.card * Card.card
(** Retrives a pair of the two cards in a hand. *)

val first_card : hand -> Card.card
(** Retrieves the first card from a hand. *)

val second_card : hand -> Card.card
(** Retrieves the second card from a hand. *)

type decision =
  | Fold
  | CheckCall
  | Raise of int  (** The types of decisions a player can make. *)
