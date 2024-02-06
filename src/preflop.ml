open Card
open Player
open Command
open State
open Bot
open Ui
open Rounds

let rec play_preflop_round curr_state =
  try
    let previous_action = curr_state.previous_action in
    if previous_action = "PlayerFold" || previous_action = "BotFold" then begin
      if curr_state.player_money = 0 then begin
        print_endline "You ran out of money! GAME OVER.";
        exit 0
      end
      else if curr_state.bot_money = 0 then begin
        print_endline "Bot ran out of money! YOU WIN.";
        exit 0
      end
      else print_endline "New Round!";
      let card_list = State.grab_card_on_start () in
      let player_hand =
        Player.create_hand
          (Rounds.match_hand card_list 0)
          (Rounds.match_hand card_list 1)
      in
      let bot_hand =
        Bot.create_hand
          (Rounds.match_hand card_list 2)
          (Rounds.match_hand card_list 3)
      in
      let new_big_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      if previous_action = "PlayerFold" then
        let initial_state =
          State.init_state_fold player_hand bot_hand curr_state.player_money
            (curr_state.bot_money + curr_state.pot)
            curr_state.big_blind
        in
        play_preflop_round { initial_state with big_blind = new_big_blind }
      else
        let initial_state =
          State.init_state_fold player_hand bot_hand
            (curr_state.player_money + curr_state.pot)
            curr_state.bot_money curr_state.big_blind
        in
        play_preflop_round { initial_state with big_blind = new_big_blind }
    end
    else if previous_action = "CheckCall" then
      let small_blind =
        if curr_state.big_blind = "player" then "bot" else "player"
      in
      let new_state = Rounds.initialize_flop curr_state in
      let round_state =
        Rounds.play_flop_round
          { new_state with previous_action = ""; turn = small_blind }
      in
      play_preflop_round { round_state with river = []; pot = 0 }
    else if curr_state.turn = "player" then begin
      print_string (Ui.display_all curr_state);
      print_endline "\n Your turn. Enter a move: \n";
      let command_input = read_line () in
      let command = Command.parse command_input in
      let new_player_state = Rounds.process_command curr_state command in
      play_preflop_round { new_player_state with turn = "bot" }
    end
    else begin
      print_string (Ui.display_all curr_state);
      let bot_decision =
        Bot.initial_hand_decision curr_state.bot_hand curr_state.bot_money
      in
      let new_bot_state =
        Rounds.register_bot_decision curr_state bot_decision
      in
      play_preflop_round { new_bot_state with turn = "player" }
    end
  with Empty | Malformed ->
    print_endline
      "Please enter in a valid command. Valid commands include 'Check', \
       'Call', 'Bet', 'Fold', 'Quit'.";
    play_preflop_round curr_state
