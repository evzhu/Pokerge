open Poker.Card
open Poker.Player
open Poker.Command
open Poker.State
open Poker.Bot
open Poker.Ui


(**************************************** Main ***************************************)

(**[initialize_game] initializes the game by creating the initial state with random hands*)
let initialize_game () = 
  print_endline "New Round!";
  let card_list = Poker.State.grab_card_on_start () in 
  let player_hand = Poker.Player.create_hand (Poker.Rounds.match_hand card_list 0) (Poker.Rounds.match_hand card_list 1) in
  let bot_hand = Poker.Bot.create_hand (Poker.Rounds.match_hand card_list 2) (Poker.Rounds.match_hand card_list 3) in
  let initial_state = Poker.State.init_state player_hand bot_hand in 
  Poker.Preflop.play_preflop_round initial_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Pokerge! \n";
  print_endline "Would you like to play a game? (Y/N).";
  let game_input = read_line () in 
  match (String.lowercase_ascii game_input) with 
  | "y" -> initialize_game ()
  | "yes" -> initialize_game ()
  | "N" -> exit 0
  | "no" -> exit 0
  | _ -> exit 0

(* Execute the game engine. *)
let () = main ()