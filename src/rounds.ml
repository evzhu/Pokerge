open Card
open Player
open Command
open State
open Bot
open Ui

(************************* Helper functions used by all round functions
  *****************************)

let match_hand (card_list : Card.card list) n =
  match card_list with
  | [ a; b; c; d ] ->
      if n = 0 then a else if n = 1 then b else if n = 2 then c else d
  | _ -> failwith "Expected hand"

(**[raise curr_state] allows the user to place a valid bet under [curr_state]*)
let rec raise curr_state =
  try
    print_endline "How much would you like to bet?";
    let bet_input = read_line () in
    let bet_amount = int_of_string bet_input in
    if curr_state.player_money - bet_amount < 0 then begin
      print_endline "You do not have enought to bet this amount. Try again.";
      raise curr_state
    end
    else bet_amount
  with Failure _ ->
    print_endline "Please enter a valid amount.";
    raise curr_state

let register_bot_decision (curr_state : State.t) (bot_decision : Bot.decision) =
  match bot_decision with
  | Fold ->
      print_endline " \nBot Folds. Next Round!";
      print_endline "\n ------------------------------ \n";
      { curr_state with previous_action = "BotFold" }
  | CheckCall ->
      if curr_state.previous_bet = 0 then (
        print_endline " \n Bot Checks.";
        { curr_state with previous_action = "CheckCall" })
      else (
        print_endline " \n Bot Calls. \n";
        {
          curr_state with
          pot = curr_state.pot + curr_state.previous_bet;
          bot_money = curr_state.bot_money - curr_state.previous_bet;
          previous_action = "CheckCall";
        })
  | Raise x ->
      let bet_amount = x in
      print_endline ("\nBot raises " ^ string_of_int bet_amount ^ "\n");
      {
        curr_state with
        pot = curr_state.pot + bet_amount;
        bot_money = curr_state.bot_money - bet_amount;
        previous_bet = bet_amount;
      }
  | No -> curr_state

let process_command curr_state command =
  match command with
  | Quit ->
      print_endline "\nThanks for playing!";
      exit 0
  | Fold ->
      print_endline " \nNice try. Next Round!";
      print_endline "\n ------------------------------ \n";
      { curr_state with previous_action = "PlayerFold" }
  | CheckCall ->
      if curr_state.previous_bet = 0 then
        { curr_state with previous_action = "CheckCall" }
      else
        {
          curr_state with
          pot = curr_state.pot + curr_state.previous_bet;
          player_money = curr_state.player_money - curr_state.previous_bet;
          previous_action = "CheckCall";
        }
  | Bet ->
      let bet_amount = raise curr_state in
      {
        curr_state with
        pot = curr_state.pot + bet_amount;
        player_money = curr_state.player_money - bet_amount;
        previous_bet = bet_amount;
        turn = "bot";
      }
  | AllIn ->
      {
        curr_state with
        pot = curr_state.pot + curr_state.player_money;
        player_money = 0;
        previous_bet = curr_state.player_money;
        turn = "bot";
        previous_action = "AllIn";
      }

(**[process_doublecheck curr_state] checks if in the [curr_state] both players
   have checked the round*)
let process_doublecheck curr_state =
  if curr_state.previous_bet = 0 then { curr_state with previous_action = "" }
  else
    {
      curr_state with
      pot = curr_state.pot + curr_state.previous_bet;
      player_money = curr_state.player_money - curr_state.previous_bet;
      previous_action = "";
    }

let process_doublecheck_bot curr_state =
  if curr_state.previous_bet = 0 then { curr_state with previous_action = "" }
  else
    {
      curr_state with
      pot = curr_state.pot + curr_state.previous_bet;
      bot_money = curr_state.bot_money - curr_state.previous_bet;
      previous_action = "";
    }

(**[evaluate curr_state winner] creates a new state for a final evaluation based
   on [winner] *)
let evaluate curr_state winner =
  print_endline (winner ^ " wins!");
  if winner = "player" then
    {
      curr_state with
      player_money = curr_state.pot + curr_state.player_money;
      previous_action = "BotFold";
    }
  else if winner = "bot" then
    {
      curr_state with
      bot_money = curr_state.pot + curr_state.bot_money;
      previous_action = "PlayerFold";
    }
  else { curr_state with previous_action = "BotFold"; pot = 0 }

(**************************************** Helper functions for initialize
  ************************************)

let initialize_allin curr_state =
  print_endline "\n ------------------------------ \n";
  let player_hand = curr_state.player_hand in
  let bot_hand = curr_state.bot_hand in

  let river_card1 = State.grab_card curr_state in
  let new_state = State.add_card curr_state river_card1 in

  let river_card2 = State.grab_card new_state in
  let new_state2 = State.add_card curr_state river_card2 in

  let final_turn_state =
    State.init_round_state player_hand bot_hand new_state2 curr_state.big_blind
      curr_state.player_money curr_state.bot_money curr_state.turn
  in
  final_turn_state

(**[initialzie_turn curr_state] adds a card to the river for the turn and river
   round*)
let initialize_turn_river curr_state =
  print_endline "\n ------------------------------ \n";
  let player_hand = curr_state.player_hand in
  let bot_hand = curr_state.bot_hand in

  let river_card1 = State.grab_card curr_state in
  let new_state = State.add_card curr_state river_card1 in

  let final_turn_state =
    State.init_round_state player_hand bot_hand new_state curr_state.big_blind
      curr_state.player_money curr_state.bot_money curr_state.turn
  in
  final_turn_state

let initialize_flop curr_state =
  print_endline "\n ------------------------------ \n";
  let player_hand = curr_state.player_hand in
  let bot_hand = curr_state.bot_hand in

  let river_card1 = State.grab_card curr_state in
  let new_state = State.add_card curr_state river_card1 in

  let river_card2 = State.grab_card new_state in
  let new_state2 = State.add_card new_state river_card2 in

  let river_card3 = State.grab_card new_state2 in
  let new_state3 = State.add_card new_state2 river_card3 in

  let final_flop_state =
    State.init_round_state player_hand bot_hand new_state3 curr_state.big_blind
      curr_state.player_money curr_state.bot_money curr_state.turn
  in
  final_flop_state

(**************************************** River round
  ***************************************)

(**[play_river_round old_state] plays out the river round of a poker game by
   creating a new state from old_state *)
let rec play_river_round curr_state =
  try
    if curr_state.previous_action = "BotFold" then
      let small_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      {
        curr_state with
        player_money = curr_state.player_money;
        turn = small_blind;
        river = [];
      }
    else if curr_state.previous_action = "PlayerFold" then
      let small_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      {
        curr_state with
        bot_money = curr_state.bot_money;
        turn = small_blind;
        river = [];
      }
    else if curr_state.turn = "player" then begin
      print_string (Ui.display_all curr_state);
      print_endline "\n Your turn. Enter a move: \n";
      let command_input = read_line () in
      let command = Command.parse command_input in
      match command with
      | CheckCall ->
          if curr_state.previous_bet != 0 then
            let player_hand =
              Bot.create_hand
                (Player.first_card curr_state.player_hand)
                (Player.second_card curr_state.player_hand)
            in
            let new_state =
              evaluate curr_state
                (State.find_winner curr_state.bot_hand player_hand
                   curr_state.river)
            in
            play_river_round new_state
          else if curr_state.previous_action = "CheckCall" then
            let player_hand =
              Bot.create_hand
                (Player.first_card curr_state.player_hand)
                (Player.second_card curr_state.player_hand)
            in
            let new_state =
              evaluate curr_state
                (State.find_winner curr_state.bot_hand player_hand
                   curr_state.river)
            in
            play_river_round new_state
          else
            let new_player_state = process_command curr_state command in
            play_river_round { new_player_state with turn = "bot" }
      | _ ->
          let new_player_state = process_command curr_state command in
          play_river_round { new_player_state with turn = "bot" }
    end
    else begin
      print_string (Ui.display_all curr_state);
      let bot_decision =
        Bot.initial_hand_decision curr_state.bot_hand curr_state.bot_money
      in
      match bot_decision with
      | CheckCall ->
          if curr_state.previous_bet != 0 then (
            print_endline "Bot Calls";
            let player_hand =
              Bot.create_hand
                (Player.first_card curr_state.player_hand)
                (Player.second_card curr_state.player_hand)
            in
            let new_state =
              evaluate curr_state
                (State.find_winner curr_state.bot_hand player_hand
                   curr_state.river)
            in
            play_river_round new_state)
          else if curr_state.previous_action = "CheckCall" then (
            print_endline "Bot Checks";
            let player_hand =
              Bot.create_hand
                (Player.first_card curr_state.player_hand)
                (Player.second_card curr_state.player_hand)
            in
            let new_state =
              evaluate curr_state
                (State.find_winner curr_state.bot_hand player_hand
                   curr_state.river)
            in
            play_river_round new_state)
          else
            let new_bot_state = register_bot_decision curr_state bot_decision in
            play_river_round { new_bot_state with turn = "player" }
      | _ ->
          let new_bot_state = register_bot_decision curr_state bot_decision in
          play_river_round { new_bot_state with turn = "player" }
    end
  with Empty | Malformed ->
    print_endline
      "Please enter in a valid command. Valid commands include 'Check', \
       'Call', 'Bet', 'Fold'.";
    play_river_round curr_state

(**************************************** Turn round
  ***************************************)

(**[play_turn_round old_state] plays out the turn round of a poker game by
   creating a new state from old_state *)
let rec play_turn_round curr_state =
  try
    if curr_state.previous_action = "BotFold" then
      let small_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      {
        curr_state with
        player_money = curr_state.player_money;
        turn = small_blind;
        river = [];
      }
    else if curr_state.previous_action = "PlayerFold" then
      let small_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      {
        curr_state with
        bot_money = curr_state.bot_money;
        turn = small_blind;
        river = [];
      }
    else if curr_state.turn = "player" then begin
      print_string (Ui.display_all curr_state);
      print_endline "\n Your turn. Enter a move: \n";
      let command_input = read_line () in
      let command = Command.parse command_input in
      match command with
      | CheckCall ->
          if curr_state.previous_bet != 0 then
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck curr_state in
            play_river_round
              (initialize_turn_river { check_state with turn = small_blind })
          else if curr_state.previous_action = "CheckCall" then
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck_bot curr_state in
            play_river_round
              (initialize_turn_river { check_state with turn = small_blind })
          else
            let new_player_state = process_command curr_state command in
            play_turn_round { new_player_state with turn = "bot" }
      | _ ->
          let new_player_state = process_command curr_state command in
          play_turn_round { new_player_state with turn = "bot" }
    end
    else begin
      print_string (Ui.display_all curr_state);
      let bot_decision =
        Bot.initial_hand_decision curr_state.bot_hand curr_state.bot_money
      in
      match bot_decision with
      | CheckCall ->
          if curr_state.previous_bet != 0 then (
            print_endline "Bot Calls";
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck_bot curr_state in
            play_river_round
              (initialize_turn_river { check_state with turn = small_blind }))
          else if curr_state.previous_action = "CheckCall" then (
            print_endline "Bot Checks";
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck_bot curr_state in
            play_river_round
              (initialize_turn_river { check_state with turn = small_blind }))
          else
            let new_bot_state = register_bot_decision curr_state bot_decision in
            play_turn_round { new_bot_state with turn = "player" }
      | _ ->
          let new_bot_state = register_bot_decision curr_state bot_decision in
          play_turn_round { new_bot_state with turn = "player" }
    end
  with Empty | Malformed ->
    print_endline
      "Please enter in a valid command. Valid commands include 'Check', \
       'Call', 'Bet', 'Fold'.";
    play_turn_round curr_state

(**************************************** Flop round
  ***************************************)

let rec play_flop_round curr_state =
  try
    if curr_state.previous_action = "BotFold" then
      let small_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      {
        curr_state with
        player_money = curr_state.player_money;
        turn = small_blind;
        river = [];
      }
    else if curr_state.previous_action = "PlayerFold" then
      let small_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      {
        curr_state with
        bot_money = curr_state.bot_money;
        turn = small_blind;
        river = [];
      }
    else if curr_state.turn = "player" then begin
      print_string (Ui.display_all curr_state);
      print_endline "\n Your turn. Enter a move: \n";
      let command_input = read_line () in
      let command = Command.parse command_input in
      match command with
      | CheckCall ->
          if curr_state.previous_bet != 0 then
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck curr_state in
            play_turn_round
              (initialize_turn_river { check_state with turn = small_blind })
          else if curr_state.previous_action = "CheckCall" then
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck_bot curr_state in
            play_turn_round
              (initialize_turn_river { check_state with turn = small_blind })
          else
            let new_player_state = process_command curr_state command in
            play_flop_round { new_player_state with turn = "bot" }
      | _ ->
          let new_player_state = process_command curr_state command in
          play_flop_round { new_player_state with turn = "bot" }
    end
    else begin
      print_string (Ui.display_all curr_state);
      let bot_decision =
        Bot.initial_hand_decision curr_state.bot_hand curr_state.bot_money
      in
      match bot_decision with
      | CheckCall ->
          if curr_state.previous_bet != 0 then (
            print_endline "Bot Calls";
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck_bot curr_state in
            play_turn_round
              (initialize_turn_river { check_state with turn = small_blind }))
          else if curr_state.previous_action = "CheckCall" then (
            print_endline "Bot Checks";
            let small_blind =
              if curr_state.big_blind = "player" then "bot" else "player"
            in
            let check_state = process_doublecheck_bot curr_state in
            play_turn_round
              (initialize_turn_river { check_state with turn = small_blind }))
          else
            let new_bot_state = register_bot_decision curr_state bot_decision in
            play_flop_round { new_bot_state with turn = "player" }
      | _ ->
          let new_bot_state = register_bot_decision curr_state bot_decision in
          play_flop_round { new_bot_state with turn = "player" }
    end
  with Empty | Malformed ->
    print_endline
      "Please enter in a valid command. Valid commands include 'Check', \
       'Call', 'Bet', 'Fold'.";
    play_flop_round curr_state
