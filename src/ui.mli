(**UI displaying game data*)

val display_hands : State.t -> string
(** [display_hands a] returns a string of the hands in gamestate a. *)

val display_river : State.t -> string
(** [display_river a] returns a string of all the community cards in gamestate
    a. *)

val display_pot : State.t -> string
(** [display_pot a] returns a string representing the integer value of the pot
    in gamestate a. *)

(** [display_all a] returns a string representing the concatenated values of the
    hands, river, and pot in gamestate a *)

val display_all : State.t -> string
