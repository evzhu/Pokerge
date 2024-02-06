open OUnit2
open Poker.Card
open Poker.Player
open Poker.Bot
open Poker.State

(** As a group, we decided to primarily focus testing on the cards, and making
    sure that the methods that calculate the card values work. This involves
    detailed testing of the bot class, and all of the check methods for
    different types of cards. All of these tests are glass box tests, as they
    are background methods that were used to calculate the make_decision method.
    They are all helper for the bot decision on what to do. We also ran some
    longer test to get the best valued hand out of a river and the bot's hand
    combined. As long as the bot is making an correctly educated decision, it
    ensures the rest of the bot works smoothly. Those tests are black box tests
    that are based on the specifications of the method. By testing the checks
    and the calculation, it ensures that the create card works for bot, the
    combinations work, proper decisions are made, and the comparison works well.
    Secondly, we tested the find winner function of state, and ran brief testing
    of the value methods that provide values for different hands. By confirming
    that the findWinner method works perfectly, we therefore can argue that the
    methods that help the findWinner method also work smoothly. Most of these
    tests for findWinner are black box testing, which only require knowledge of
    the method itself. However, testing the methods that give values for hands
    is glass box testing that requires information of how the methods work, and
    what they return. We also did a large amount of manual testing through
    playing the game, and repeated checks on different parts of it. Our decision
    for starting this project was because we all played poker together. Instead
    of just us, the coders, doing the testing, we enlisted a few friends who
    play poker with us. We showed them the basic commands, and then had them
    mess around, play the game, and try to break the game. This tested all of
    the different methods in state, ui, command, and player, and makes sure
    there are no loose ends in the project. A simple issue that they found and
    we resolved is the problem at the start, when the user is asked to play the
    game. When they wrote a letter other than y or n, the game would crash, as
    it didn't take in that input. We ended up making it so that any response
    other than Y or y would end the game, as it is an assumption that they did
    not want to play. Then, they are free to rerun to play the game. We believe
    that all of these tests, along with the manual play testing, proves the
    correctness of the code, and the overall system. This is because the tests
    run an in depth check of the different methods needed for calculations, and
    the manual testing confirmed the lack of errors throughout the different
    modules, and the methods that are used to let them work together. Glass box
    testing the important helper methods confirms that the insides of important
    methods are correct, and by black box testing large methods such as ones to
    find the bot's decision based on their cards and the river, we prove
    correctness of those methods. The correctness of state and UI are proved
    through the manual testing, and the lack of problems with displaying the
    game as it is played. *)

let pp_int i = "\"" ^ string_of_int i ^ "\""

let pp_string s = "\"" ^ s ^ "\""


let bot_check_hand_bool_test (name : string)
    (func : Poker.Card.card list -> bool) (input : Poker.Card.card list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (func input)

let bot_check_hand_value_test (name : string) (input : Poker.Card.card list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Poker.Bot.hand_value input) ~printer:pp_int

let bot_check_decision_test (name : string) (input1 : Poker.Bot.hand)
    (input2 : Poker.Card.card list) (input3 : int)
    (expected_output : Poker.Bot.decision) : test =
  name >:: fun _ ->
  assert_equal expected_output (Poker.Bot.make_decision input1 input2 input3)

let state_check_winner_test (name : string) (input1 : Poker.Bot.hand)
    (input2 : Poker.Bot.hand) (input3 : Poker.Card.card list)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Poker.State.find_winner input1 input2 input3) ~printer:pp_string

(*"Spade" | 1 -> "Clubs" | 2 -> "Hearts" | 3 -> "Diamond"*)
let twoSpades = Poker.Card.create_card "Two" "Spade"
let twoHearts = Poker.Card.create_card "Two" "Hearts"
let fiveHearts = Poker.Card.create_card "Five" "Hearts"
let sixHearts = Poker.Card.create_card "Six" "Hearts"
let sevenHearts = Poker.Card.create_card "Seven" "Hearts"
let eightHearts = Poker.Card.create_card "Eight" "Hearts"
let nineHearts = Poker.Card.create_card "Nine" "Hearts"
let jackHearts = Poker.Card.create_card "Jack" "Hearts"
let kingHearts = Poker.Card.create_card "King" "Hearts"
let aceHearts = Poker.Card.create_card "Ace" "Hearts"
let sevenClubs = Poker.Card.create_card "Seven" "Clubs"
let eightDiamonds = Poker.Card.create_card "Eight" "Diamond"
let fourClubs = Poker.Card.create_card "Four" "Clubs"
let twoDiamonds = Poker.Card.create_card "Two" "Diamond"
let twoClubs = Poker.Card.create_card "Two" "Clubs"
let fiveClubs = Poker.Card.create_card "Five" "Clubs"
let fiveSpades = Poker.Card.create_card "Five" "Spade"
let tenClubs = Poker.Card.create_card "Ten" "Clubs"
let nineSpades = Poker.Card.create_card "Nine" "Spade"
let sevenSpades = Poker.Card.create_card "Seve" "Spade"
let aceSpades = Poker.Card.create_card "Ace" "Spade"
let nineDiamonds = Poker.Card.create_card "Nine" "Diamond"
let sixSpades = Poker.Card.create_card "Six" "Spade"
let sixDiamonds = Poker.Card.create_card "Six" "Diamond"
let queenClubs = Poker.Card.create_card "Queen" "Clubs"
let aceClubs = Poker.Card.create_card "Ace" "Clubs"
let sixClubs =  Poker.Card.create_card "Six" "Clubs"
let pairRiver = [ twoClubs; nineHearts; eightDiamonds; fourClubs; kingHearts ]

let pairRiverWithFlush =
  [ twoClubs; nineHearts; fiveHearts; fourClubs; tenClubs ]

let pairRiver2 = [ twoClubs; twoDiamonds; fiveHearts; fourClubs; eightDiamonds ]
let twoPairRiver = [ twoClubs; sevenHearts; fiveHearts; sevenSpades; twoSpades ]

let allHeartsRiver =
  [ eightHearts; nineHearts; fiveHearts; sixHearts; twoHearts ]

let clubsFlushPossibleRiver =
  [ twoClubs; fiveHearts; sevenClubs; fourClubs; fiveSpades ]

let playerHand4 = Poker.Bot.create_hand jackHearts twoDiamonds
let playerHand1 = Poker.Bot.create_hand twoHearts twoSpades
let playerHand2 = Poker.Bot.create_hand jackHearts twoSpades
let playerHand3 = Poker.Bot.create_hand fiveClubs aceHearts
let botHand1 = Poker.Bot.create_hand sevenClubs fiveClubs
let botHand2 = Poker.Bot.create_hand jackHearts fiveClubs
let botHand3 = Poker.Bot.create_hand twoHearts fiveClubs
let botHand4 = Poker.Bot.create_hand sixHearts nineSpades
let botHand5 = Poker.Bot.create_hand aceSpades kingHearts
let playerHand5 = Poker.Bot.create_hand nineDiamonds nineHearts
let river5 = [ twoClubs; sixSpades; sixDiamonds; queenClubs; aceClubs ]

let bot_check_tests =
  [
    bot_check_hand_bool_test "noPairTest false" Poker.Bot.check_pair
      [ twoClubs; fiveHearts; sevenClubs; fourClubs; eightDiamonds ]
      false;
    bot_check_hand_bool_test "OnePairTest true" Poker.Bot.check_pair
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; eightDiamonds ]
      true;
    bot_check_hand_bool_test "noTwoPairTest false" Poker.Bot.check_2pair
      [ twoClubs; fiveHearts; sevenClubs; fourClubs; eightDiamonds ]
      false;
    bot_check_hand_bool_test "noTwoPairTest w/ only 1 pair false"
      Poker.Bot.check_2pair
      [ twoClubs; fiveHearts; sevenClubs; twoClubs; eightDiamonds ]
      false;
    bot_check_hand_bool_test "TwoPairTest true" Poker.Bot.check_2pair
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; fiveClubs ]
      true;
    bot_check_hand_bool_test "noThreeKindTest false" Poker.Bot.check_3kind
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; fiveClubs ]
      false;
    bot_check_hand_bool_test "ThreeKindTest true" Poker.Bot.check_3kind
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; twoHearts ]
      true;
    bot_check_hand_bool_test "straightTest false" Poker.Bot.check_straight
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; fiveClubs ]
      false;
    bot_check_hand_bool_test "straightTest true" Poker.Bot.check_straight
      [ fourClubs; fiveHearts; sixHearts; sevenClubs; eightDiamonds ]
      true;
    bot_check_hand_bool_test "straightTest different order true"
      Poker.Bot.check_straight
      [ sixHearts; fiveClubs; sevenClubs; eightDiamonds; fourClubs ]
      true;
    bot_check_hand_bool_test "flushTest false" Poker.Bot.check_flush
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; fiveClubs ]
      false;
    bot_check_hand_bool_test "almostFlushTest false" Poker.Bot.check_flush
      [ twoHearts; fiveHearts; sevenClubs; kingHearts; aceHearts ]
      false;
    bot_check_hand_bool_test "flushTest true" Poker.Bot.check_flush
      [ twoHearts; fiveHearts; jackHearts; kingHearts; aceHearts ]
      true;
    bot_check_hand_bool_test "flushTest2 true" Poker.Bot.check_flush
      [ twoHearts; fiveHearts; jackHearts; sixHearts; aceHearts ]
      true;
    bot_check_hand_bool_test "fullHouseTest false" Poker.Bot.check_full_house
      [ twoHearts; fiveHearts; jackHearts; kingHearts; aceHearts ]
      false;
    bot_check_hand_bool_test "fullHouseTestTwosFives true"
      Poker.Bot.check_full_house
      [ twoHearts; twoSpades; twoDiamonds; fiveClubs; fiveHearts ]
      true;
    bot_check_hand_bool_test "fullHouseTestFivesTwos true"
      Poker.Bot.check_full_house
      [ twoHearts; fiveSpades; twoHearts; fiveClubs; fiveHearts ]
      true;
    bot_check_hand_bool_test "fourKindTest false" Poker.Bot.check_4kind
      [ twoHearts; twoSpades; fiveHearts; fiveClubs; twoClubs ]
      false;
    bot_check_hand_bool_test "fourKindTest 2s true" Poker.Bot.check_4kind
      [ twoHearts; twoSpades; fiveHearts; twoDiamonds; twoClubs ] true;

      bot_check_hand_bool_test "fourKindTest 6s true" Poker.Bot.check_4kind
      [ sixDiamonds; sixHearts; sixSpades; twoDiamonds; sixClubs ]
    
      true;
    bot_check_hand_bool_test "straightFlush true" Poker.Bot.check_straight_flush
      [ fiveHearts; sixHearts; sevenHearts; eightHearts; nineHearts ]
      true;
    bot_check_hand_bool_test "straightFlushUnordered true"
      Poker.Bot.check_straight_flush
      [ eightHearts; sixHearts; nineHearts; sevenHearts; fiveHearts ]
      true;
    bot_check_hand_value_test "pair value"
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; eightDiamonds ]
      2;
    bot_check_hand_value_test "highCard value"
      [ twoClubs; fiveHearts; sevenClubs; jackHearts; eightDiamonds ]
      1;
    bot_check_hand_value_test "2pair value : 3"
      [ twoClubs; fiveHearts; sevenClubs; twoDiamonds; fiveClubs ]
      3;
    bot_check_hand_value_test "2pair value variation : 3"
      [ fiveClubs; fiveHearts; sevenClubs; twoDiamonds; twoHearts ]
      3;
    bot_check_hand_value_test "3 kind value : 4"
      [ twoClubs; twoHearts; sevenClubs; twoDiamonds; fiveClubs ]
      4;
    bot_check_hand_value_test "straight value : 5"
      [ sixHearts; sevenClubs; eightDiamonds; nineHearts; fiveClubs ] 5;

      bot_check_hand_value_test "straight value straight river 2: 5"
      [ sixHearts; sevenClubs; eightDiamonds; nineHearts; fiveClubs ]
      
      5;
    bot_check_hand_value_test "flush value : 6"
      [ sixHearts; aceHearts; twoHearts; nineHearts; kingHearts ]
      6;
      bot_check_hand_value_test "flush value 2 : 6"
      [ twoHearts; fiveHearts; jackHearts; sixHearts; aceHearts ]
      6;
    bot_check_hand_value_test "full house value : 7"
      [ twoClubs; twoDiamonds; twoHearts; fiveClubs; fiveHearts ]
      7;
    bot_check_hand_value_test "4 kind value : 8"
      [ twoClubs; twoDiamonds; twoHearts; twoSpades; fiveHearts ]
      8;
      bot_check_hand_value_test "4 kind value 6s : 8"
      [ sixDiamonds; sixHearts; sixSpades; twoDiamonds; sixClubs ]
      8;
      
    bot_check_hand_value_test "straight flush value : 9"
      [ sixHearts; sevenHearts; eightHearts; nineHearts; fiveHearts ]
      9;
    bot_check_decision_test "High card fold decision"
      (Poker.Bot.create_hand aceHearts twoClubs)
      [ fiveClubs; sixHearts; sevenHearts; jackHearts; fourClubs ]
      100 Poker.Bot.Fold;
    bot_check_decision_test "pair check decision"
      (Poker.Bot.create_hand aceHearts twoClubs)
      [ fiveClubs; sixHearts; sevenHearts; jackHearts; twoDiamonds ]
      100 Poker.Bot.CheckCall;
    bot_check_decision_test "2 pair check decision"
      (Poker.Bot.create_hand aceHearts twoClubs)
      [ fiveClubs; sixHearts; sevenHearts; fiveHearts; twoDiamonds ]
      100 Poker.Bot.CheckCall;
    bot_check_decision_test "3 kind raise decision"
      (Poker.Bot.create_hand aceHearts twoClubs)
      [ fiveClubs; sixHearts; sevenHearts; twoHearts; twoDiamonds ]
      100
      (Poker.Bot.Raise (100 / 7));
    bot_check_decision_test "flush raise decision"
      (Poker.Bot.create_hand aceHearts twoClubs)
      [ fiveClubs; sixHearts; sevenHearts; twoHearts; jackHearts ]
      100 (Poker.Bot.Raise (100 / 7));
      bot_check_decision_test "flush raise decision river flush"
      (Poker.Bot.create_hand aceHearts twoClubs)
      [ twoHearts; fiveHearts; jackHearts; sixHearts; aceHearts ]
      100 (Poker.Bot.Raise (100 / 7));
      

      bot_check_decision_test "straight raise decision"
      (Poker.Bot.create_hand eightHearts twoClubs)
      [ sixHearts; sevenClubs; eightDiamonds; nineHearts; fiveClubs ]
      100
      
      (Poker.Bot.Raise (100 / 7));
    bot_check_decision_test "straight flush raise decision"
      (Poker.Bot.create_hand eightHearts twoClubs)
      [ fiveClubs; sixHearts; sevenHearts; nineHearts; fiveHearts ]
      100
      (Poker.Bot.Raise (100 / 7));
    bot_check_decision_test "4 kind raise decision"
      (Poker.Bot.create_hand eightHearts twoClubs)
      [ twoDiamonds; sixHearts; twoSpades; twoHearts; fiveHearts ]
      100
      (Poker.Bot.Raise (100 / 7));
  ]

let winnerTests =
  [
    state_check_winner_test "player winner pair vs high card" botHand1
      playerHand4 pairRiver "player";
    state_check_winner_test "bot winner flush vs pair " botHand1 playerHand4
      pairRiverWithFlush "bot";
    state_check_winner_test "player winner 4 kind vs 2 pair" botHand1
      playerHand1 pairRiver2 "player";
    state_check_winner_test "player winner 4 kind vs 2 pair second" botHand2
      playerHand1 pairRiver2 "player";
    state_check_winner_test "player winner pair vs high card second" botHand2
      playerHand4 pairRiver "player";
    state_check_winner_test "tie 3 kind vs 3 kind "
      botHand2 playerHand3 clubsFlushPossibleRiver "tie";
    state_check_winner_test "bot winner full house vs 2 pair" botHand1
      playerHand3 clubsFlushPossibleRiver "bot";
    state_check_winner_test "hearts flush tie" botHand3 playerHand1
      allHeartsRiver "tie";
    state_check_winner_test "player winner full house vs 2 pair" botHand4
      playerHand4 twoPairRiver "player";
    state_check_winner_test "bot winner pair vs high" botHand4 playerHand3
      pairRiver "bot";
    state_check_winner_test "bot winner better pair  vs pair" botHand4
      playerHand2 pairRiver "bot";
    state_check_winner_test "bot winner better 2 pair  vs 2 pair" botHand5
      playerHand5 river5 "bot";
    state_check_winner_test "bot winner  2 pair + king high  vs 2 pair" botHand5
      playerHand3 river5 "bot";
  ]

let suite = "test suite" >::: List.flatten [ bot_check_tests; winnerTests ]
let _ = run_test_tt_main suite
