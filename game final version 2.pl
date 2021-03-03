/*
 *    Programmer- Nissim Iluz.
 *    File Name - game final version.
 *    Descripiton - Remikov short version.
 *    Input - Commands and Elections. In-game instructions.
 *    Output - The state of the game.
 *    Synopsys - play.
 *
 */

/*
 * During the game, we add and edit several predicates with other predicates.
 * *  alpha (A, Lonely, Order)
 * *  - Saves the best arrangement found during the search.
 * *  A- the minimum number of cards that are not in a sequence
 * *  Lonely - Cards left alone during the best arrangement
 * *  Order - the best arrangement to date.
 *
 * *  save (Lonely)
 * *  - Keeps other arrangements, used to select a card to download.
 * *  Lonely- The list of cards to download when a possible arrangement is completed.
 *
 * *  level (Level)
 * *  - Saves the current level.
 * *  The player can choose between 3 difficulty levels, during the game the computer automatically changes the level.
 *
 * *  card (Value, Color, Serial Num)
 * *  - playing cards.
 * *  Serial_num - The card's serial number.
 *
 * *  card_manager (Num)
 * *  - Saves the number of cards remaining in the cash register
 */


 % start the gmae.
play:-
    create_card,!,
    get_ten_card(List1,List2),
    quicksort1(List1,ListPlayer2),
    color_order(ListPlayer2,ListPlayer1),
    quicksort1(List2,ListComputer2),
    write('The game start'),nl,
    level_config(ListComputer2,ListComputer1),nl,
   (  ListComputer1=exit,!
    ;
     alpha(A,Lonely,Order),
     write('At any stage you can type:'),nl,
     tab(8),write('help - to get help.'),nl,
     tab(8), write('aaa or abc -to change the order of your cards.'),nl,
     tab(8), write('exit - to end the game.'),nl,
     tab(2), write('(You can enter the first letter of the answer)'),nl,nl,
      tab(1), write('To start a new game enter new or again at any stage'),nl,
     nl, write('Your cards are:'),nl,
     printCards(ListPlayer1),nl,
     (  A==0.0,win_the_gamae(Order)
     ;
        player_win(ListPlayer1)
     ;
       random_between(1,2,X),
       plus_one(ListPlayer1,ListPlayer),     % The next play of the tree play will have another card in the card list.
       plus_one(ListComputer1,ListComputer), % The next play of the tree play will have another card in the card list.
       NewA is A+2,  % The next play of the tree play will have another card in the lonley card list.
       updating_alpha(NewA,Lonely,Order),



       (
           X==1,write('computer begins'),nl,
           computer_turn(ListPlayer,ListComputer,nil)
       ;
           X==2,write('you begins'),nl,
           player_turn(ListPlayer,ListComputer,nil)
    )))
    .
/*
 * computer_turn(ListPlayer,ListComputer,X)
 * ListPlayer - the cards of the player
 * ListComputer - The cards of the computer save as Cards-Jokers-Number of cards
 * X - the last downloaded card (It can be picked up), in the initially equal to nil
 * Calling to 'checkin' at finish
 */
% the player input exit at is turn. end the game
computer_turn(_,_,'exit'):-!.
computer_turn(_,_,'end'):-!,
    nl,nl,write('You want to start a new game?'),nl,
    read(YON),
     (     (  YON=='yes';YON=='y';YON=='Y',YON=='YES',YON=='ye'),!,nl,nl,nl,play
     ;
     true).

 % Checks whether to take the card that the player has downloaded
computer_turn(ListPlayer,ListComputer,X):-
    X\=nil, % not initially
    insert_card(X,ListComputer,NewList),
    alpha(A1,Lonely1,Order1),
    play_tree(NewList),
    alpha(A2,_,_),
    A1\=A2, %  fail when taking the card does not produce a better result
    (A is A1-1, A2=<A,!;updating_alpha(A1,Lonely1,Order1),fail), % Checks
%   whether the new arrangement is found to be significantly better.
%   otherwise, Alpha restores
   checking(ListPlayer,NewList,1).

% take a new card
computer_turn(ListPlayer,ListComputer,_):-
    get_card(Card),
    insert_card(Card,ListComputer,NewList),
    alpha(A1,Lonely1,Order1),
    updating_alpha(A1,[Card|Lonely1],Order1), % We do not miss one at the end of the previous turn
    play_tree(NewList),
    checking(ListPlayer,NewList,2).

/*
 * The second part of the computer turn manager.
 * Determines the next step.
 * When finished calling player_turn or win_the game
 */
checking(ListPlayer,ListComputer1,Num2):-
   adapter_level(ListComputer1,ListComputer), % adapter level. A==0 only and only if Level=1
   level(Level),
   write('-------------------------------------My turn--------------------------------------------------------'),nl,
   alpha(A,Lonely1,Order1),
    (   (A==0;A==0.0),win_the_gamae(Order1),!
        ;
	drop(Lonely1,Card_to_drop),
        alpha(A1,Lonely2,Order),
        delete_card(Card_to_drop,ListComputer,Final_list), % delete_card from computer list
        delete_card(Card_to_drop,Lonely2,Lonely3),  % delete_card from lonely in alpha
        updating_alpha(A1,Lonely3,Order),
        (
            Num2==2,write('I took a new card, and drop '),printCard(Card_to_drop)
         ;
            Num2==1,write('I took the card you downloaded, and drop '),printCard(Card_to_drop)
        ),
            (Level==1,A1=<1.0,win_the_gamae(Order),!
        ;
            nl,nl,write('-------------------------------------Your turn------------------------------------------------------'),nl,
            player_turn(ListPlayer,Final_list,Card_to_drop),!
    )
    ).
/*
 * Allows the player to perform his moves,
 * When the player wins - success, otherwise call the computer turn
 */
% when the player began and it is his first turn
player_turn(ListPlayer,ListComputer,nil):-
    !,
    follow('no',insert,_,ListPlayer,Res_list,Drop_card),
    (
    Drop_card\=exit, player_win(Res_list),!,computer_turn(_,_,end)
    ;
    computer_turn(Res_list,ListComputer,Drop_card)
    ).
% Otherwise
player_turn(ListPlayer,ListComputer,X):-
     follow(nil,chose,X,ListPlayer,New_list,Drop_card),
     (
     Drop_card\=exit,player_win(New_list),!,computer_turn(_,_,end)
     ;
     computer_turn(New_list,ListComputer,Drop_card)).

/*
 * follow(Command,Direction,X,ListPlayer,NewListPlayer,New_X)
 *
 * Command- player input.
 *
 * Direction shows the step we are in.
 * *  chose- chose to take the card thet the camputer drop
 * *  insert- insert the card
 * *  select- select card to drop
 * *  drop- drop the card
 * *  done- finishedh the turn
 *
 */
follow('exit',_,_,_,_,exit):-!. % Returns exit
follow('again',_,_,_,_,exit):-!,nl,nl,nl,play.
follow('new',_,_,_,_,exit):-!,nl,nl,nl,play.

follow('end_of_file',_,_,_,_,exit):-!. % Returns exit

follow(nil,chose,X,ListPlayer,New_list,Drop_card):-!,
     write('Your cards are:'),nl,
     write('for help enter help. for change the order of your cards enter aaa/abc'),nl,
     printCards(ListPlayer),nl,nl,
     write('Would you like to pick up the card? '),printCard(X),
     write('  (yes or no) '),nl,
     read(Command),nl,
     follow(Command,insert,X,ListPlayer,New_list,Drop_card).

follow('no',insert,_,List,Res_list,Drop_card):-!,
     !,
     get_card(Card),
     write('you get  :'),printCard(Card),write('  do you want to keep it? (yes or not)'),nl,
     read(Command),
     follow(Command,insert2,Card,List,Res_list,Drop_card).

follow('yes',Direction,Card,List,Res_list,Drop_card):-
    (   Direction==insert;Direction==insert2),!,
    insert(Card,List,New_list),
    follow(nil,select,_,New_list,Res_list,Drop_card).


% Returns the list unchanged and the card the player refused to accept as a card to download.
follow('no',insert2,Card,List,List,Card):-!.

% Allows the user to select the card to download, and identifies the card
follow(nil,select,_,ListPlayer,New_list,Drop_card):-!,
     write('enter the index of the card you went  to drop or the nuber of card and is color as illustrated "Number-Color":'),
     nl,printCards(ListPlayer),
     printNumbering(ListPlayer),
     nl,read(Command),
     follow(Command,drop,_,ListPlayer,New_list,Drop_card).

follow(Num,drop,_,List-Joker-Num_of_Cards,Res_List,Drop_card):-
    integer(Num),
    Num_of_Cards>=Num,Num>0,!,
    drop_player(List-Joker,Num,Newlist,X),
    write('you chose to drop '),printCard(X),
    write(' are you sure? (yes or not)  '),nl,
    read(Command),nl,
    follow(Command,done,X,Newlist-Num_of_Cards,Res_List,Drop_card).

follow(Value-Color,drop,_,List-Joker-Num_of_Cards,Res_List,Drop_card):-
    option_of_follow(Value,drop1),
    option_of_follow(Color,drop2),!, % Checks whether the selection is valid
follow(nil,drop2,Value-Color,List-Joker-Num_of_Cards,Res_List,Drop_card).

follow(nil,drop2,Value-Color,List-Joker-Num_of_Cards,Res_List,Drop_card):-
   (   member(card(Value,Color),List),!;member(card(Value,Color),Joker),!;
       member1(card(Value,Color),List)), % Checks whether the selection is valid
    delete_card_player(card(Value,Color),List-Joker,New_list-New_jokers),!,
    write('you chose to drop '),printCard(card(Value,Color)),write(.),nl,
    follow(yes,done,card(Value,Color),New_list-New_jokers-Num_of_Cards,Res_List,Drop_card).

follow(nil,drop2,_,ListPlayer,New_list,Drop_card):-
    write('The card does not exist'),nl,
    follow(nil,select,_,ListPlayer,New_list,Drop_card).

/*
 * Downloads the card that the player has selected to download.
 * Returns the User's card list after the swap,
 * and the card that the player downloaded as a downloaded card.
 */
follow('yes',done,Drop_card,Newlist,Newlist,Drop_card):-!.

% Returns the identified card and requests to select a new card
follow('no',done,X,List,Res_List,Drop_card):-!,
   insert(X,List,New_list),
   follow(nil,'select',_,New_list,Res_List,Drop_card) .

% Allows call for help and return to previous follow
follow('help',Direction,X,ListPlayer,Res_list,Drop_card):-
   (    Direction=insert; Direction= insert2),!,
    helper(insert,ListPlayer),nl,
    write('Would you like the card? '),printCard(X),
    write('  (yes or no) '),nl,
    read(Command),nl,
    follow(Command,Direction,X,ListPlayer,Res_list,Drop_card).

follow('help',drop,_,ListPlayer,New_list,Drop_card):-!,
    helper(drop,ListPlayer),nl,
    follow(nil,select,_,ListPlayer,New_list,Drop_card).

% Lets to change the order of cards when the cards are displayed
follow(Command,drop,_,ListPlayer,New_list,Drop_card):-
     (Command=='aaa';Command=='abc'),!,
    change_order(ListPlayer,New_list1),
    follow(nil,select,_,New_list1,New_list,Drop_card).

follow(Command,insert,X,List,Res_list,Drop_card):-
    (Command=='aaa';Command=='abc'),!,
    change_order(List,New_list),
    follow(nil,chose,X,New_list,Res_list,Drop_card).

follow(Command,insert2,X,List,Res_list,Drop_card):-
    (Command=='aaa';Command=='abc'),!,
    change_order(List,New_list),
    printCards(New_list),nl,
    write('Would you like the card? '),printCard(X),nl,
    read(New_command),nl,
    follow(New_command,insert2,X,New_list,Res_list,Drop_card).

/*
 * Invalid input Therapist
 */
% Synonyms
follow(Unknown,Direction,X,ListPlayer,NewListPlayer,New_X):-
    in_short(Command,Unknown),
    follow(Command,Direction,X,ListPlayer,NewListPlayer,New_X),!.
% Uppercase
follow(Unknown,Direction,X,ListPlayer,NewListPlayer,New_X):-
    atom(Unknown),
    Unknown\=_-_,
    string_code(1,Unknown, Code),Code<91,!,
    string_lower(Unknown, Unknown2),
    atom_string(Command, Unknown2),
    follow(Command,Direction,X,ListPlayer,NewListPlayer,New_X).

/*
 * Finds a match from the valid input list
 *
 * * In this order of predicates we make several adjustments passable
 */
% Match found
follow(yes,understand,nil,nil,nil,yes):-!.
% No match found
follow(no,understand,nil,nil,nil,no):-!.

% Finds a match from the valid input list
follow(Unknown1-Unknown2,drop,X,ListPlayer,NewListPlayer,New_X):-
    atomic(Unknown1),atomic(Unknown2),
    string_chars(Unknown1,ListU1),
    string_chars(Unknown2,ListU2),
    findall(X1,option_of_follow(X1,drop1),Commands1),
    findall(X2,option_of_follow(X2,drop2),Commands2),
     matching(ListU1,Commands1,Command1-Num1),Num1>0,
     matching(ListU2,Commands2,Command2-Num2),Num2>0,
     write("I didn't understand you. Did you mean  '"), write(Command1-Command2),write("'?  "),
     nl,read(YoN),
     follow(YoN,understand,_,_,_,Back),
     (   Back==exit,New_X=exit,!;
         Back==yes,!,follow(Command1-Command2,drop,X,ListPlayer,NewListPlayer,New_X)).

% Finds a match from the valid input list
follow(Unknown,Direction,X,ListPlayer,NewListPlayer,New_X):-
    atomic(Unknown),
    not(in_short(Unknown,_)), % There is a mistake- Invalid input
    not(in_short(_,Unknown)), % There is a mistake- Invalid input
    % option_of_follow- Predicates guarding valid input, depending on the other variables.
    findall(X1,option_of_follow(X1,Direction),Commands),
    string_chars(Unknown,ListU),
    matching(ListU,Commands,Command-Num),
    Num>0,
    write("I didn't understand you. Did you mean  '"), write(Command),write("'?  "),
    nl,read(YoN),
    follow(YoN,understand,_,_,_,Back),
    (   Back==exit,New_X=exit,!;
         Back==yes,!,follow(Command,Direction,X,ListPlayer,NewListPlayer,New_X)).

% All options failed trying again
follow(_,Direction,X,ListPlayer,NewListPlayer,New_X):-
   Direction\=understand,
   write('I could not understand you. Please try again'),nl,
   read(Command),
   follow(Command,Direction,X,ListPlayer,NewListPlayer,New_X).

% check if the player wins, by trying to find order without rest
player_win(List):-
   (List=[_,_,_,_]-_-_,change_order(List,Clist),!;Clist=List),
   player_win(Clist,Order),
   win_the_game2(Order).
/*
 *  player_win(List_card,Legal order)
 * Trying to arrange the first card in the card list first by series and then by color.
 * The arrangement must be at least contain three cards
 * Continues with the list of cards left after arranging.
 *
 *	Can only be run on sorted list in ascending order.
 */
player_win([card(Value,Color)|Cards]-Jokers-Num,[Order|Old_order]):-
   series(card(Value,Color),Cards-Jokers-Num,Rest_card-Rest_jokers-Left_num,Order-Series_num),Series_num>=3,
    player_win(Rest_card-Rest_jokers-Left_num,Old_order).
player_win([card(Value,Color)|Cards]-Jokers-Num,[Order|Old_order]):-
  color(card(Value,Color),Cards-Jokers-Num,Rest_card-Rest_jokers-Left_num,Order-Color_Num),Color_Num>=3,
  player_win(Rest_card-Rest_jokers-Left_num,Old_order).
player_win([]-[]-_,[]):-!.

win_the_gamae(Order):- nl,nl,write('I won! This is my cards:   '), printCards(Order),computer_turn(_,_,end).

win_the_game2(Order):- nl,nl,write('You won!'),printCards(Order).
/*
 * play tree
 *
 * - play tree/5 Success only when the number of cards with no
 * arrangement is less than or equal to 1 - The leaves retain the
 * relevant results in the predicates.
 *
 * play_tree(In_progress-Num1,Rest_cards-Num2,Temp_cards-Num3,Lonely_cards-Num4,Order)
 * * In_progress - The cards we are currently working on.
 * * Num1 - Number of cards in the In_progress list
 * * Rest_cards - The remaining cards
 * * Num2 - Number of cards in the Rest_cards list
 * * Lonely_cards - The cards we went through and are not in legal order.
 * * Num3 - Number of cards in the Lonely_cards list
 * * Order- The cards we went through and they are in legal order
 *
 */
play_tree(List):-
     retractall(save(_)),fail % Deletes all memories
     ;
     play_tree([]-0,List,[]-0,[]-0,[]),!;true.

% Create a new order from rist. allowin redo.
play_tree([]-0,[Card|Rest]-Jokers-Left_num,Temp-Num_temp,Lonely-Num_Lonely,Order):-
    heuristic_function(Num_Lonely,Num_temp), % Heuristic function
    Send_left_num is Left_num-1,
    (
    color(Card,Rest-Jokers-Send_left_num,New_rest,In_progress)
    ;
    series(Card,Rest-Jokers-Send_left_num,New_rest,In_progress)
    ),
    play_tree(In_progress,New_rest,Temp-Num_temp,Lonely-Num_Lonely,Order).

% Handling existing arrangements
% Num1 (Number of cards in the In_progress list)>=3
play_tree(In_progress-Num_of_card,Rest_cards,Temp_cards,Lonely_cards,Order):-
    Num_of_card>=3,
    play_tree([]-0,Rest_cards,Temp_cards,Lonely_cards,[In_progress|Order]).

% Num_of_card=2
play_tree(In_progress-2,Rest_cards,Temp_cards,Lonely-Num_Lonely,Order):-
    New_num_Lonely is Num_Lonely+1.5,
    play_tree([]-0,Rest_cards,Temp_cards,[In_progress|Lonely]-New_num_Lonely,Order).

/*
 * Num_of_card=1
 *
 * has_card - checker for the single card, is there a  card whose value is X + 2 of the same color in the list of remaining
 *          - if there is we insert the pair to temp list.
 * has_temp - Checker for the single card, is there a  Pair of cards of shape (card(X-2,Color),card(X,Color)) in the temp list
 *			- if there is we transfer the pair to lonlet list.
 * Check1,Check2 save whether the test was success or not .
 * The card goes into the lonely list iff the two checkers fail.
 *
 */
play_tree([Card1]-1,Rest_cards,Temp-Num_temp,Lonely-Num_Lonely,Order):-
    (has_temp(Card1,Temp,Lonely,NewTemp1,New_Lonely),Check1=1,!;Check1=0,NewTemp1=Temp,New_Lonely=Lonely),
    (Check1==0,has_card(Card1,Rest_cards,NewTemp1,NewTemp),Check2=1,!;Check2=0,NewTemp=NewTemp1),
    (   (
    Check1==1 ,New_num_temp is Num_temp-1,New_num_Lonely is Num_Lonely+1.5
    ;
    Check1==0,Check2==1 ,New_num_temp is Num_temp+1,New_num_Lonely is Num_Lonely
    ),
    play_tree([]-0,Rest_cards,NewTemp-New_num_temp,New_Lonely-New_num_Lonely,Order)
    ;
    Check1==0,Check2==0,New_num_Lonely is Num_Lonely+1.0,
    play_tree([]-0,Rest_cards,Temp-Num_temp,[Card1|Lonely]-New_num_Lonely,Order)
    ).


/*
 * leaf
 * Rest_cards and Rest_jokers is empty
 * Can only succeed when the number of lonely cards is less than or equal to 1
 */
play_tree([]-0,[]-[]-_,Temp-Num_temp,Lonely-Num_Lonely,Order):-
    alpha(A,_,_),
    level(Level),
    conc2(Temp,Lonely,New_Lonely),
    New_num_Lonely is Num_temp+ Num_Lonely,
    Until is A+1,
    New_num_Lonely=<Until,
    assert(save(Lonely)),
    New_num_Lonely<A,
    updating_alpha(New_num_Lonely,New_Lonely,Order),
    (Level==1,New_num_Lonely=<1;Level\=1,New_num_Lonely=<0).

% Copies the first card of each pair of cards at temp to lonely list
conc2([[Card1,_]|Temp],Lonely,[Card1|Rest]):-
    conc2(Temp,Lonely,Rest).
conc2([],Lonely,Lonely).


/*
 * series(Card,List_of_Card,New_list_of_card,Order)
 *
 * Allowing  redo in that order of priorities:
 * 1.1. The longest sequence without jokers.
 * 1.2. The longest sequence with one joker.
 * 1.3. The longest sequence with two joker.
 * .
 * .
 * 1._. The longest sequence with all jokers.
 * 2.1. A sequence obtained without the last card from the sequence 1 without jokers.
 * 2.2. A sequence obtained without the last card from the sequence 1 without jokers with one joker.
 * .
 * .
 * 2._. A sequence obtained without the last card from the sequence 1 with all jokers.
 * 3.1. A sequence obtained without the last card from the sequence 2 without jokers
 * .
 * .
 * .
 * _.1. A sequence of two.
 * can't create an order of one card. lonely card can be created only in color arrangement.
 */
series(Card,Cards,Rest,[Card|Series]-Series_num):-
    series1(Card,Cards,Rest,Series-Series_num).

series(Card,Cards,Rest,[Card|Series]-Series_num):-
    series2(Card,Cards,Rest,Series-Series_num).

% Puts the next card in ascending order
series1(card(Value1,Color),[card(Value2,Color)|Cards]-Jokers-Left_num,Card1-Jokers1-Left_num1,[card(Value2,Color)|Series]-New_num):-
    Value2 is Value1+1,
    (
    series1(card(Value2,Color),Cards-Jokers-Left_num,Card1-Jokers1-Left_num2,Series-Num)
    ;
    done(card(Value2,Color),Cards-Jokers-Left_num,Card1-Jokers1-Left_num2,Series-Num)
    ;
    series2(card(Value2,Color),Cards-Jokers-Left_num,Card1-Jokers1-Left_num2,Series-Num)
    ),
    Left_num1 is Left_num2-1,
    New_num is Num+1.
% Skip card
series1(card(Value1,Color1),[card(Value2,Color2)|Cards]-Jokers-Left_num,[card(Value2,Color2)|Cards1]-Jokers1-Left_num1,Order):-
    Expected_value is Value1 +1,
    Expected_value>=Value2,
    series1(card(Value1,Color1),Cards-Jokers-Left_num,Cards1-Jokers1-Left_num1,Order).
% Puts a joker
series2(card(Value1,Color),Cards-[card(joker,magenta)|Jokers]-Left_num,Card1-Jokers1-Left_num1,[card(joker,magenta)|Series]-New_num):-
    Expected_value is Value1 +1,
    (
    series1(card(Expected_value,Color),Cards-Jokers-Left_num,Card1-Jokers1-Left_num2,Series-Num)
    ;
    done(_,Cards-Jokers-Left_num,Card1-Jokers1-Left_num2,Series-Num)
    ;
    series2(card(Expected_value,Color),Cards-Jokers-Left_num,Card1-Jokers1-Left_num2,Series-Num)
    ),
    Left_num1 is Left_num2-1,
    New_num is Num+1.

done(_,Cards,Cards,[]-1).


/*
 * Allowing  redo in that order of priorities:
 * 1.1. The largest-length arrangement  with no jokers
 * 1.2 The largest-length arrangement includes one jokers
 * 1.3 The largest-length arrangement includes two jokers
 * .
 * .
 * 1._.The largest-length arrangement includes all jokers
 * 2.1 The largest-length arrangement obtained without the last card from the previous sequence includes all jokers
 * 2.2 The largest-length arrangement obtained without the last card from the previous sequence includes all jokers-1
 * .
 * .
 * 2._. The largest-length arrangement obtained without the last card from the previous sequence with no jokers
 * .
 * .
 * _. A single card (Can return a single card that is not a joker)
*/
color(card(Value1,Color1),[card(Value2,Color2)|Cards]-Jokers-Num,New_list_of_card,[card(Value1,Color1)|New_order]-Nun_order):-
    color(card(Value1,Color1),[Color1],[card(Value2,Color2)|Cards]-Jokers-Num,New_list_of_card,New_order-Nun_order).

color(card(Value1,Color1),Cards-Jokers-Num,New_list_of_card,[card(Value1,Color1)|New_order]-Nun_order):-
    add_jokers([]-1,Cards-Jokers-Num,New_list_of_card,New_order-Nun_order).

color(card(Value,Color1),Colors,[card(Value,Color2)|Rest]-Jokers-Left_num,Rest_card-Rest_jokers-Left_num1,[card(Value,Color2)|Same_color_list]-Num1):-
    not(member(Color2,Colors)),
    (
    color(card(Value,Color1),[Color2|Colors],Rest-Jokers-Left_num,Rest_card-Rest_jokers-Left_num2,Same_color_list-Num2)
    ;
    add_jokers([]-1,Rest-Jokers-Left_num,Rest_card-Rest_jokers-Left_num2,Same_color_list-Num2)
    ),
    Left_num1 is Left_num2-1,
    Num1 is Num2+1.

color(card(Value1,Color1),Colors,[card(Value1,Color2)|Rest]-Jokers-Left_num,[card(Value1,Color2)|Rest_card2]-Jokers2-Left_num2,Result):-
    color(card(Value1,Color1),Colors,Rest-Jokers-Left_num,Rest_card2-Jokers2-Left_num2,Result).

add_jokers(Order,Rest_card,Rest_card,Order).
add_jokers(Same_color_list-Num,Card-[card(joker,magenta)|Joker]-Old_left_num,New_rest_card,Order):-
    New_left_num is Old_left_num-1,
    New_num is Num+1,
    add_jokers([card(joker,magenta)|Same_color_list]-New_num,Card-Joker-New_left_num,New_rest_card,Order).

/*
 * Checks for lonely cards whether exists a pair [card(X-2,Color),card(X,Color)] in temp list
 * if there is -Deletes the pair from a temp list and puts the pair in the lonely list.
 * Otherwise fails.
 * Execution - Find a pair whose second organ is the single card
 * Stops the search when the lonely list is empty or when in the next list in tamping
 * ([card (Value2, Color2), card (Value3, Color3)])  the value of the first organ
 * (Value2) is greater than the lonely card.
 */

has_temp(card(Value1,Color),[[card(Value2,Color),card(Value1,Color)]|Temp],Lonely,Temp,[[card(Value2,Color),card(Value1,Color)]|Lonely]).
has_temp(card(Value1,Color1),[[card(Value2,Color2),card(Value3,Color2)]|Temp],Lonely,[[card(Value2,Color2),card(Value3,Color2)]|NewTemp],New_Lonely):-
   Min_value is Value1-2,
    (   Value2\=Min_value,! ;    Color2\=Color1) ,
   Value2>=Min_value,
   has_temp(card(Value1,Color1),Temp,Lonely,NewTemp,New_Lonely).

/*
 * Checks for lonely cards whether exists a card in rest list thet
 * creates an ascending pair with one skip.
 * Otherwise fails.
 * if there is -insert the pair to the temp list.
 * Stops the search when the lonely list is empty or when the next card in rest
 * is greater than the value of the single card +2
 */
has_card(card(Value1,Color),[card(Value2,Color)|_]-_-_,Temp,[[card(Value1,Color),card(Value2,Color)]|Temp]):-
    Expected_value is Value1+2,
    Expected_value==Value2.

has_card(card(Value1,Color),[card(Value2,_)|Rest]-Jokers-Num,Temp,New_temp):-
    Max_value is Value1+2,
    Value2=<Max_value,
    has_card(card(Value1,Color),Rest-Jokers-Num,Temp,New_temp).

% Success when It's worth continuing
heuristic_function(_,_):-
   level(Level),
   alpha(A,_,_),
   (A==0;Level==1,A==1),!,
   fail.
heuristic_function(Num_of_Lonely,Num_of_temp):-
    F is Num_of_Lonely+0.75*Num_of_temp, % Calculate the number of cards that will remain in the bestest case
    alpha(A,_,_),
    F=<A.

/*
 * Quick searching using difference list.
 *
 * quicksort1(List,Sorted-Jokers-Num)
 *  * List - list of cards to sort.
 *  * Sorted -list of cards sorted without jokers.
 *  * Jokers- jokers list.
 *  * Num of cards in the list (includes jokers).
 */
quicksort1(List,Sorted-Jokers-Num):-
	!,quicksort1(List,Sorted-[],Jokers-[],Num).
quicksort1([],T-T,J-J,0).
% Joker entered to another list.
quicksort1([card(joker,magenta)|Xs],Ys,[card(joker,magenta)|Jokers]-L,New_num):-!,
    quicksort1(Xs,Ys,Jokers-L,Old_num),
    New_num is Old_num+1.
% X\=card(joker,magenta)
quicksort1([X|Xs],Sort-Tail,Jokers-TailJ,Num) :-
   partition1(Xs,X,Small,Big),
   quicksort1(Big,Sort_big-B,JokersB-J1,NumB),
   quicksort1(Small,Sort_small-[X|S],JokerS-J2,NumS),
    append1(JokersB-J1,JokerS-J2,Jokers-TailJ),
   append1(Sort_small-S,Sort_big-B,Sort-Tail),
    Num is NumS +NumB+1.

partition1(Xs,card(joker,magenta),Xs,[]):-!.

partition1([card(joker,magenta)|Xs],card(Value2,Color2),Ls,[card(joker,magenta)|Rs]) :-!,
   partition1(Xs,card(Value2,Color2),Ls,Rs). % All cards are smaller than the joker
partition1([card(Value1,Color1)|Xs],card(Value2,Color2),[card(Value1,Color1)|Small],Big) :-
  Value2>= Value1,!,
  partition1(Xs,card(Value2,Color2),Small,Big).
partition1([card(Value1,Color1)|Xs],card(Value2,Color2),Small,[card(Value1,Color1)|Big]) :-
  partition1(Xs,card(Value2,Color2),Small,Big).
partition1([],_,[],[]).

append1(List1-List2,List2-L2,List1-L2).
/*
 * part of the user interface
 * allowing arrange the cards by color
 *
 * color_order(List,[Reds,Green,Yellow,Blue]-Jokers-Num)
 *  * List = Sorted_list-Jokers-Num.
 *  *  * Sorted_list- Sorted ascending list.
 */
color_order(Cards-Jokers-Num,[Reds,Green,Yellow,Blue]-Jokers-Num):-
    color_order(Cards,Reds,Green,Yellow,Blue).
color_order([card(Value,red)|List],[card(Value,red)|Reds],Green,Yellow,Blue):-
            color_order(List,Reds,Green,Yellow,Blue).
color_order([card(Value,green)|List],Reds,[card(Value,green)|Green],Yellow,Blue):-
            color_order(List,Reds,Green,Yellow,Blue).
color_order([card(Value,yellow)|List],Reds,Green,[card(Value,yellow)|Yellow],Blue):-
            color_order(List,Reds,Green,Yellow,Blue).
color_order([card(Value,blue)|List],Reds,Green,Yellow,[card(Value,blue)|Blue]):-
            color_order(List,Reds,Green,Yellow,Blue).
color_order([],[],[],[],[]).

/*
 * insert a new card to list
 */
% insert a card when the list is sorted by color
% The card enters the list with the appropriate color in the right place (Ascending).
insert(card(Value,red),[Reds,Green,Yellow,Blue]-Jokers-Num,[New,Green,Yellow,Blue]-Jokers-Num):-
    insert_card(card(Value,red),Reds,New),!.
insert(card(Value,green),[Reds,Green,Yellow,Blue]-Jokers-Num,[Reds,New,Yellow,Blue]-Jokers-Num):-
    insert_card(card(Value,green),Green,New),!.
insert(card(Value,yellow),[Reds,Green,Yellow,Blue]-Jokers-Num,[Reds,Green,New,Blue]-Jokers-Num):-
    insert_card(card(Value,yellow),Yellow,New),!.
insert(card(Value,blue),[Reds,Green,Yellow,Blue]-Jokers-Num,[Reds,Green,Yellow,New]-Jokers-Num):-
    insert_card(card(Value,blue),Blue,New),!.
insert(card(joker,magenta),List-Jokers-Num,List-[card(joker,magenta)|Jokers]-Num):-!.


% if the length of the list is bigger then 4 == the list is sorted by sorted  ascending
insert(Card,Cards,New_Cards):-
    insert_card(Card,Cards,New_Cards).

% insert joker
insert_card(card(joker,magenta),Cards-Jokers-Num,Cards-[card(joker,magenta)|Jokers]-Num):-!.
% Card to insert is not a joker
% find the right place by search
insert_card(Card,Cards-Jokers-Num,New_cards-Jokers-Num):-
    insert_card(Card,Cards,New_cards).
insert_card(Card,[],[Card]).
insert_card(card(Value1,Color1),[card(Value2,Color2)|Rest],[card(Value1,Color1),card(Value2,Color2)|Rest]):-
  Value1 =<Value2,!.
insert_card(Card1,[Card2|Rest],[Card2|NewRest]):-
    insert_card(Card1,Rest,NewRest).

/*
 * PC card download system.
 * drop(Lonely,Card).
 * * Lonely- a list of cards from which the download will be made.
 * * Card - card to download.
 *
 * The card selection is prioritized:
 * First priority - a card that is not part of a pair.
 * Second priority - a card that is part of a pair and not a joker (a joker to be accepted as a single card).
 * Of each priority group, the card that appears most frequently in the computer's memory list as a card that is not part of the sequence
 * is the selected card.
 *
 * The memory list is stored as one-field predicates save(save).
 */
drop(Lonely,Card):-
    save(_), % There are memories
    findall(Save,save(Save),Saves),
    counter(Lonely,Saves,Shows),
    max(Shows,Card),!.
drop(Lonely,Card):-
    drop(Lonely,Lonely,Card),!.
% If there are no memories
drop([Card|_],_,Card):-
    Card\=[_,_],!.
drop([_|Rest],Lonely,Card):-
    drop(Rest,Lonely,Card).
drop([],[[Card,_]|_],Card):-
    Card\=card(joker,magenta),!,
    alpha(A,Lonely,Order),  % We break down a sequence of 2 so we need to update the alpha accordingly
    NewA is A+0.5,
     updating_alpha(NewA,Lonely,Order).

drop([],[[_,Card]|_],Card):-
    Card\=card(joker,magenta),!,
    alpha(A,Lonely,Order),  % We break down a sequence of 2 so we need to update the alpha accordingly
    NewA is A+0.5,
    updating_alpha(NewA,Lonely,Order).

drop([],[_|Rest],Card):-
    drop([],Rest,Card).
/*
 * Count the number of times the card appears in the card list
 * counter(List,Saves,Resule)
 *  * List-the list of cards from which the card will be selected for download.
 *  * Saves -the save list
 *  * Resule - a list in which each organ is of the shape Card-Nunber_of_shows
 *
 */
counter([card(Value,Color)|List],Saves,[card(Value,Color)-Shows|Rest]):-
    count(card(Value,Color),Saves,Shows),!,
    counter(List,Saves,Rest),!.
counter([Sub_list|List],Saves,[New_sub_list|Rest]):-
    counter(Sub_list,Saves,New_sub_list),
    counter(List,Saves,Rest).
counter([],_,[]).
% count for one card the number of times it appears
count(Card,[Save|Saves],Num):-
    member(Card,Save),!,
    count(Card,Saves,Old_Num),
    Num is Old_Num+1.
count(Card,[Save|Saves],Num):-
    member([X,Y],Save),
    (Card==X;Card==Y),!,
    count(Card,Saves,Old_Num),
	Num is Old_Num+0.1.
count(Card,[_|Saves],Num):-
    count(Card,Saves,Num).
count(_,[],0.1).
/*
 * Finds the card whose number of occurrences is maximum.
 * max1 - Finds the card the number of times it appears is the maximum of non-paired cards.
 * max2 finds the card that is the maximum number of times it appears in the doubles group, and is not a joker
 */
max(Shows,Result):-
    max1(Shows,Result),!.
max(Shows,Result):-
    max2(Shows,Result),
   !,
    alpha(A,Lonely,Order),  % We break down a sequence of 2 so we need to update the alpha accordingly
    NewA is A+0.5,
    updating_alpha(NewA,Lonely,Order).

max1([Card-Num|Shows],Result):-!,
    max1([Card-Num|Shows],Num,Card,Result).
max1([[_-_,_-_]|Shows],Result):-
    max1(Shows,Result).

max1([Card2-Num2|Shows],Num1,_,Result):-
    Num2>Num1,!,
	max1(Shows,Num2,Card2,Result).
max1([_|Shows],Num1,Card1,Result):-
	max1(Shows,Num1,Card1,Result).
max1([],_,Card,Card).

max2([[Card1-Num1,_-Num2]|Shows],Result):-
    Num1>Num2,!,
    max2(Shows,Num1,Card1,Result).
max2([[_-_,Card2-Num2]|Shows],Result):-
    max2(Shows,Num2,Card2,Result).
max2([[Card2-Num2,Card3-Num3]|Shows],Num1,Card1,Result):-!,
    bigger(Card1-Num1,Card2-Num2,Card3-Num3,Result_card,Result_num),
    max2(Shows,Result_num,Result_card,Result).
max2([_|Shows],Num1,Card1,Result):-
    max2(Shows,Num1,Card1,Result).
max2([],_,Card,Card).
% Returns the card with the largest number of occurrences among three cards
bigger(Card1-Num1,_-Num2,_-Num3,Card1,Num1):-
    Num1>Num2,Num1>Num3,!.
bigger(_-_,Card2-Num2,_-Num3,Card2,Num2):-
    Num2>Num3,!.
bigger(_-_,_-_,Card3-Num3,Card3,Num3).

/*
 * Finds the card that needs to be deleted, and returns the list after deletion.
 * Execution by using search.
 * delete_card(Card,List-Jokers-Num,New_list-Jokers-Num).
 * * Card- card to downlled.
 * * List - downloadable list
 * * New_list - list after delete.
 *
 * Discount - card to download is not joker
 */
delete_card(Card,List-Jokers-Num,New_list-Jokers-Num):-
   delete_card(Card,List,New_list).
delete_card(Card,[Card|Rest],Rest):-!.
delete_card(Card,[[Card,Card2]|Rest],[Card2|Rest]):-!.
delete_card(Card,[[Card1,Card]|Rest],[Card1|Rest]):-!.
delete_card(Card,[X|Rest],[X|List]):-!,
    delete_card(Card,Rest,List).

/*
 * The player download and delete system.
 *
 * drop_player(List,Num,Newlist,Card)   - download from the list or download joker
 *                                      - list in ascending order ot color order
 * drop_player(List,Num,Return_num,Newlist,Card) - find the right cards to download
 *                                               - list in ascending order
 *                                               - when successful returns 1
 *                                               - other returns the last card position
 * * List - downloadable list.
 * * Num- location (index) of the card to download.
 * (* Return_num- Used for counting the number of cards we went through )
 * * Newlist - list after the download.
 * * Card - the card that has been downloaded.
 */
% Trying to download the card from the list of cards if fails downloading joker.
% list in ascending order
drop_player([card(Value,Color)|List]-Jokers,Num,Newlist-New_jokers,Card):-
    !,
    (
    drop_player([card(Value,Color)|List],Num,_,Result_list,Result_card),
    Result_card\=nil,
    Newlist=Result_list, New_jokers=Jokers, Card=Result_card,!
    ;
    Newlist=List, Card=card(joker,magenta),
    drop_player(Jokers,1,_,New_jokers,_)
    ).
% List sorted by color
drop_player([Sub_list|List]-Jokers,Num,New_list-New_jokers,Resuls_Card):-
    drop_player(Sub_list,Num,Return_num,New_sub_list,Card1),
    (   Card1\=nil,!,New_jokers=Jokers,Resuls_Card=Card1,
        New_list=[New_sub_list|List]

    ;
		drop_player(List-Jokers,Return_num,New_list2-New_jokers2,Card2),
        New_list=[Sub_list|New_list2],
        New_jokers=New_jokers2,
        Resuls_Card=Card2
    ).

drop_player([]-[card(joker,magenta)|Jokers],_,[]-Jokers,card(joker,magenta)).
drop_player([card(Value,Color)|List],1,1,List,card(Value,Color)):-!.
drop_player([],Num,Num,[],nil):-!.
drop_player([card(Value,Color)|List],Num,Return_num,[card(Value,Color)|Newlist],X):-
    !,
   New_num is Num-1,
   drop_player(List,New_num,Return_num,Newlist,X).

delete_card_player(card(joker,magenta),List-[card(joker,magenta)|Jokers],List-Jokers).
delete_card_player(Card,[card(Value,Color)|List]-Jokers,New_list-Jokers):-!,
     delete_card(Card,[card(Value,Color)|List],New_list).
delete_card_player(card(Value,red),[Reds,Green,Yellow,Blue]-Jokers,[New,Green,Yellow,Blue]-Jokers):-
    delete_card(card(Value,red),Reds,New),!.
delete_card_player(card(Value,green),[Reds,Green,Yellow,Blue]-Jokers,[Reds,New,Yellow,Blue]-Jokers):-
    delete_card(card(Value,green),Green,New),!.
delete_card_player(card(Value,yellow),[Reds,Green,Yellow,Blue]-Jokers,[Reds,Green,New,Blue]-Jokers):-
    delete_card(card(Value,yellow),Yellow,New),!.
delete_card_player(card(Value,blue),[Reds,Green,Yellow,Blue]-Jokers,[Reds,Green,Yellow,New]-Jokers):-
    delete_card(card(Value,blue),Blue,New).

/*
 * Lets user select difficulty level
 * The level is saved in a level predicate (and save the appropriate alpha)
 * returns the list of cards after level adjustment
*/
level_config(List,Final_list):-
    write('Please enter a level, you can choose:'),nl,
    tab(7),write("'1' - for easy"),nl,
    tab(7),write("'2' - for medium"),nl,
    tab(7),write("'3' - for hard"),nl,
    tab(7),write("'4' - for expert"),nl,
    read(Level),
    adapter_level(Level,List,Result_list),
    play_tree(Result_list),
    adapter_level(Result_list,Final_list).

adapter_level(exit,_,exit):-!.
adapter_level(e,_,exit):-!.
adapter_level(_,_,_):-    retractall(alpha(_,_,_)),fail. % Always done delete old alpha (if there is)
adapter_level(_,_,_):-    retractall(level(_)),fail.     % Always done delete old levle (if there is)

adapter_level(1,Cards-Jokers-Num,Cards-Jokers-Num):-!,
    assert(alpha(12.0,nil,nil)),
    assert(level(1)).
adapter_level(2,Cards-Jokers-Num,Cards-[card(joker,magenta)|Jokers]-New_num):-!,
    New_num is Num+1,
    assert(alpha(13.0,nil,nil)),
    assert(level(2)).
adapter_level(3,Cards-Jokers-Num,Cards-[card(joker,magenta),card(joker,magenta)|Jokers]-New_num):-!,
    New_num is Num+2,
    assert(alpha(14.0,nil,nil)),
    assert(level(3)).
adapter_level(4,Cards-Jokers-Num,Cards-[card(joker,magenta),card(joker,magenta),card(joker,magenta)|Jokers]-New_num):-!,
    New_num is Num+2,
    assert(alpha(15.0,nil,nil)),
    assert(level(4)).
/*
 * Unknown in-put.
 * Trying to find a match to valid input
 */
adapter_level(Unknown,List,Result):-
    atomic(Unknown),
    string_chars(Unknown,ListU),
    matching(ListU,[1,2,3,4,exit],Word-Num),Num>0,
    write("I couldn't understand you, did you mean:  "),write(Word),nl, % match has been found
    read(YON),nl, % Than a match
    (
       ( YON==e;YON==exit),Result==exit,! % returns exit for exit
     ;
    % Returns the value that returns with the corrected input
    (   YON=='yes';YON=='y';YON=='Y',YON=='YES',YON=='ye'),!, adapter_level(Word,List,Result)).

adapter_level(_,List,Result):- % All options failed. Trying again
    write("I couldn't understand you, please try again"),nl,
    read(Level),nl,
    adapter_level(Level,List,Result).
/*
 * adapter_level(List,Result)
 * Regulate the level.
 * Changes the level when A=<0.0 (0=\0.0) and Level >1
 * Result- The list of cards after level change.
 * Edit Level and alpha,
 * At the end the game_tree run and the level is checked again
 * Finishes only when A>1 or Level=1
 */
adapter_level(Cards-[card(joker,magenta)|New_jokers]-Num,Result):-
    alpha(A,_,_),
    level(Level),
    Level>1,A=<0.0,!,
    (   retract(alpha(_,_,_)),!;true),
    (   retract(level(_)),!;true),
    New_level is Level-1,
    New_num is Num-1,
    assert(level(New_level)),
    assert(alpha(4,nil,nil)),
    play_tree(Cards-New_jokers-New_num),
    adapter_level(Cards-New_jokers-New_num,Result).
% A>0 or Level=1
adapter_level(List,List).

/*
 *  Submitting player help
 * Initially, the player's values are removed and in place appropriate values come in,
 * When finished, the values are returned to the source.
 *
 * Important Note - The assistant makes no assumptions about the player and does not take into account previous tests performed.
 */
helper(Direction,ListPlayer):-
    write('---------------------HELP--------------------'),nl,
    retract(alpha(A,Lonely,Order)),
    retract(level(Level)),
    assert(alpha(12,_,_)),
    assert(level(1)),
    (
        ListPlayer\=[_,_,_,_]-_-_,List=ListPlayer,! ;
        change_order(ListPlayer,List)),
   play_tree(List),
   alpha(_,Player_loley,Player_order),
   write('You can produce this sequence:  '),printCards(Player_order),nl,
   write('And stay with:  '),printCards1(Player_loley),nl,
     (Direction==drop,!,drop(Player_loley,Drop_card),write('Consider downloading the card:  '),printCard(Drop_card),nl
      ;true),
   updating_alpha(A,Lonely,Order),
   retract(level(_)),
   assert(level(Level)).

/*
 * Lets identify user input
 */
% Returns the value contains the most identical characters to the input from the list of possible inputs
matching(ListU,[Word2|Rest_Legal],WordS-NumS):-
    string_chars(Word2,ListW),
    match(ListU,ListW,Num2),
    matching(ListU,Rest_Legal,Word1-Num1),
    (
    Num1>Num2,NumS is Num1,Word1 =WordS,! % Word1 is the more suitable
    ;
    NumS is Num2, Word2 =WordS). %  Word2 is the more suitable
matching(_,[],fail-0).

match([X|ListU],[X|ListW],Num):-!,
    match(ListU,ListW,Old_num),
    Num is Old_num+1.
match([X|ListU],[Y|ListW],Num):-
    % Selects the skipping that his result is the best
    match(ListU,[Y|ListW],Num1),
    match([X|ListU],ListW,Num2),
    (   Num1>Num2, Num is Num1,!;Num is Num2).
match([],_,0).
match(_,[],0).

in_short(yes,y).
in_short(yes,ye).
in_short(no,not).
in_short(no,n).
in_short(no,nop).
in_short(help,h).
in_short(aaa,aa).
in_short(abc,ab).
in_short(exit,e).
in_short(exit,ex).
in_short('no word fits',new).
in_short('no word fits',again).

% option_of_follow- Predicates guarding valid input, depending on the other variables.
option_of_follow('no',insert).
option_of_follow('yes',Direction):-
     Direction==insert;Direction==insert2.
option_of_follow('no',insert2).
option_of_follow('aaa',Direction):-Direction\=understand,Direction\=done,Direction\=drop1,Direction\=drop2.
option_of_follow('abc',Direction):-Direction\=understand,Direction\=done,Direction\=drop1,Direction\=drop2.
option_of_follow('yes',done).
option_of_follow('no',done).
option_of_follow('help',Direction):-Direction\=understand,Direction\=done.
option_of_follow('yes',understand).
option_of_follow(X,drop):- X=1;X=2;X=3;X=4;X=5;X=6;X=7;X=8;X=9;X=10;X=11.
option_of_follow(X,drop1):- (X=1;X=2;X=3;X=4;X=5;X=6;X=7;X=8;X=9;X=10;X=11;X=12;X=13).
option_of_follow(Color,drop2):-	(Color=red;Color=yellow;Color=blue;Color=green).
option_of_follow('exit',Direction):-Direction\=drop1,Direction\=drop2.
option_of_follow('again',Direction):-Direction\=drop1,Direction\=drop2.
option_of_follow('new',Direction):-Direction\=drop1,Direction\=drop2.

% member for list sorted by color
member1(card(Value,red),[Reds,_,_,_]):-
    member(card(Value,red),Reds).
member1(card(Value,green),[_,Green,_,_]):-
    member(card(Value,green),Green).
member1(card(Value,yellow),[_,_,Yellow,_]):-
    member(card(Value,yellow),Yellow).
member1(card(Value,blue),[_,_,_,Blue]):-
    member(card(Value,blue),Blue).


% updating_alpha(Num_Lonely,Lonely,Order).
updating_alpha(Num_Lonely,Lonely,Order):-
    retractall(alpha(_,_,_)),
    assert(alpha(Num_Lonely,Lonely,Order)).


/*
 * Card deck manager
 *
 * create_card-
 * Creates 106 cards.
 * A pair of cards from 1 to 13 in four colors: red, yellow and blue, and two gokers.
 * Each card saves as a 3-field predicate, value, color, and serial number.
 */
create_card:-
    (    retractall(card(_,_,_)),!;true),
    (    retract(card_manager(_)),!;true),
    create_card(1,red,1),
    assert(card(joker,magenta,105)),
    assert(card(joker,magenta,106)),
    assert(card_manager(106)).   % remember the number of the cards at any point in the game.

create_card(Value,Color, Serial_num):-
    Value<14,
    Serial_num2 is Serial_num+1,
    assert(card(Value,Color,Serial_num)),
    assert(card(Value,Color,Serial_num2)),
    New_Value is Value+1,
    New_Serial_num is Serial_num+2,
    create_card(New_Value,Color, New_Serial_num).

create_card(14,red, Serial_num):-    create_card(1,green, Serial_num).
create_card(14,green, Serial_num):-  create_card(1,yellow, Serial_num).
create_card(14,yellow, Serial_num):- create_card(1,blue, Serial_num).
create_card(14,blue,_).

/*
 * Selects a random card from the stack, and returns its value and color
 * The product remove this card from the memory,
 * and edit the stack card for the next Selects .
 * get_card(card(Value,Color)):-!,
 * get_card(card(Value,Color,_)).
 * Number of crds down one
 */
 % Entering only here
get_card(card(Value,Color)):-!,
    get_card(card(Value,Color,_)).

get_card(card(Value,Color,Serial_num)):-
    card_manager(N),
    random_between(1,N,Serial_num),!,
    (
    Serial_num==N,!, retract(card(Value,Color,Serial_num)) % the lest card raffle.
    ;
    /*  else- we remove the last card
     *   and place it in the place of the selected card
     */
    retract(card(Value,Color,Serial_num)),
    retract(card(Value1,Color1,N)),
    assert(card(Value1,Color1,Serial_num))
    ),
    New_N is N-1,
    (   New_N>1,!,
		 retractall(card_manager(_)),
         assert(card_manager(New_N))
    ;
         create_card
    ).
get_ten_card(ListPlayer,ListComputer):-
    get_ten_card(ListPlayer,ListComputer,10).
get_ten_card([],[],0):-!.

% Creates two random stacks of ten from a deck of cards.
get_ten_card([Card1|ListPlayer],[Card2|ListComputer],X):-
    NewX is X -1,
    get_card(Card1),
    get_card(Card2),
    get_ten_card(ListPlayer,ListComputer,NewX).

/*
 * Part of the user interface
 * Allows changing the order of the cards in O (n) time
 */
change_order([Reds,Green,Yellow,Blue]-Jokers-Num,New_order-Jokers-Num):-!,
    merge1(Reds,Green,List1),
    merge1(Yellow,Blue,List2),
    merge1(List1,List2,New_order).

change_order(Old_order,New_order):-
    color_order(Old_order,New_order).

merge1([card(Value1,Color1)|List1],[card(Value2,Color2)|List2],[card(Value1,Color1)|Result]):-
	Value1=<Value2,!,
    merge1(List1,[card(Value2,Color2)|List2],Result).
merge1([card(Value1,Color1)|List1],[card(Value2,Color2)|List2],[card(Value2,Color2)|Result]):-
    merge1([card(Value1,Color1)|List1],List2,Result).
merge1([],List2,List2).
merge1(List1,[],List1).
merge1([],[],[]).


/*
 * Printing system
 */
printCard(card(Value,Color)):-
    ansi_format([bold,fg(Color)], '  ~w   ', [Value]).
/*
 * Handles two cases:
 * Case 1 Standard List.
 * Case 2 list within list.
 */
printCards(Cards-Jokers-_):-
    !,printCards(Cards),
    write("  "),
    printCards(Jokers).

printCards([card(Value,Color)|Rest]):-
    !,
    printCard(card(Value,Color)),
    write(" "),
    printCards(Rest).
% A list within list.
printCards([X|Rest]):-
    !,
    printCards(X),
    write("   "),
    printCards(Rest).

printCards([]).
/*
 * Part of the user interface.
 * Performs printing for the assistant.
 * Prints first the lists that are in the list and then the organs that are non-lists.
*/
printCards1(List):-
    printCards2(List),
    write("   --  "),
    printCards3(List).

printCards2([[X,Y]|Rest]):-!,
    printCards([X,Y]),
     write("     "),
    printCards2(Rest).
printCards2([_|Rest]):-
    printCards2(Rest).
printCards2([]).

printCards3([card(Value,Color)|Rest]):-!,
printCard(card(Value,Color)),
    write("     "),
    printCards3(Rest).
printCards3([_|Rest]):-
    printCards3(Rest).
printCards3([]).

/*
 * Part of the user interface.
 * Printing the index of the card,
 * used to select a card to download.
*/
printNum(Num,joker):-
    !,
     ansi_format([underline,fg(black)], ' ~w ', ['']),
    ansi_format([underline,fg(black)], ' ~w   ', [Num]),
     ansi_format([underline,fg(black)], '   ~w  '  , ['']).

printNum(Num,Value):-
   Value<10,Num<10,!,
   ansi_format([bold,fg(black)], '  ~w   ', [Num]).

printNum(Num,Value):-
   Value<10,Num>=10,!,
   ansi_format([bold,fg(black)], ' ~w ', [Num]).


   %  Value>=10
printNum(Num,_):-
   Num>=10,!,
   ansi_format([bold,fg(black)], '   ~w ', [Num]).

%  Value>=10, Num<10
printNum(Num,_):-
   ansi_format([underline,fg(black)], '~w  ', ['']),
   ansi_format([bold,fg(black)], '  ~w   ', [Num]).

printNumbering(Cards-Jokers-_):-!,
    nl,
    printNumbering(Cards,1,Next),
    write("   "),
    printNumbering(Jokers,Next,_).

printNumbering(Cards-Jokers):-
    nl,
    printNumbering(Cards,1,Next),
    write("   "),
    printNumbering(Jokers,Next,_).
printNumbering([card(Value,_)|Rest],Num,Save):-
    !,
    printNum(Num,Value),
    Next is Num+1,
    write(" "),
    printNumbering(Rest,Next,Save).

printNumbering([X|Rest],Num,Save):-
    !,
    printNumbering(X,Num,Next),
    write("   "),
    printNumbering(Rest,Next,Save).

printNumbering([],Save,Save).

% Add one to number of cards
plus_one(Cards-Jokers-Num,Cards-Jokers-New_num):-
   New_num is Num +1.


