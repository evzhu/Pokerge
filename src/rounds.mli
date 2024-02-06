(** Rounds are the functions used to play the rounds past the preflop of a poker
    game*)

val play_flop_round : State.t -> State.t
(**[play_flop_round old_state] plays out the flop round of a poker game by
   creating a new state from [old_state]*)

val match_hand : Card.card list -> int -> Card.card
(**[match_hand card_list] returns a card from [card_list] based on the given
   input*)

val process_command : State.t -> Command.command -> State.t
(**[process_command curr_state command] evaluates [command] under the
   [curr_state] to decide what player should do*)

val register_bot_decision : State.t -> Bot.decision -> State.t
(**[register_bot_decision curr_state bot_decision] evaluates [bot_decision]
   under the [curr_state]*)

val initialize_flop : State.t -> State.t
(**[initialzie_flop curr_state] adds cards to the river for the flop round*)
