(** Representation of a game.

    This module represents the properties each card may have. *)

type t = {
  river : Card.card list;
  pot : int;
  player_hand : Player.hand;
  bot_hand : Bot.hand;
  big_blind : string;
  player_money : int;
  bot_money : int;
  previous_bet : int;
  previous_action : string;
  deck : int list;
  turn : string;
}
(**The type t of a game state*)

val init_state : Player.hand -> Bot.hand -> t
(**[init_state] creates an initial state that is used to evaluate the preflop*)

val init_state_fold : Player.hand -> Bot.hand -> int -> int -> string -> t
(**[init_state_fold] creates an initial state that is used when a player folds*)

val init_round_state :
  Player.hand -> Bot.hand -> t -> string -> int -> int -> string -> t
(**[init_state_fold] creates an initial state that is used when evaluating the
   state in post-preflop rounds*)

val add_card : t -> Card.card -> t
(**[add_card] returns a state with a new card added to the river*)

val grab_card : t -> Card.card
(**[grab_card] grabs a card with the deck making sure there are no repetitions*)

val grab_card_on_start : unit -> Card.card list
(**[grab_card_on_start] grabs cards before pre_flop for each hand*)

val find_winner : Bot.hand -> Bot.hand -> Card.card list -> string
(**[find_winner] evaluates the two player hands and river to determine who the
   winner is*)

val grab_card : t -> Card.card
(** [grab_card t] is a card taken from the remaining cards in [t.deck] *)
