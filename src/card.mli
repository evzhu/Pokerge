(** Representation of a card.

    This module represents the properties each card may have. *)

type card
(** The abstract type of suit of a card. *)

val create_card : string -> string -> card
(** Creates a card with a given value and suit. *)

val get_suit : card -> string
(** Gets the suit of [card] *)

val get_value : card -> string
(** Gets the value of [card] *)

val convert_value_to_string : card -> string
(** Converts the card number (A, 2, 3, ...) into a string. *)

val convert_value_to_int : card -> int
(** Converts the card number into its integer equivalent. Jack is 11, Queen is
    12, King is 13, and Ace is 14. *)
