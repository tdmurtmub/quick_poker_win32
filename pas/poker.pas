{ (C) 2009 Wesley Steiner }

{$MODE FPC}
{$define VER1}

unit poker;

interface

uses
	cards;

type
	handrank=(RUNT,ONE_PAIR,TWO_PAIR,THREE_KIND,STRAIGHT,FLUSH,FULLHOUSE,FOUR_KIND,STRAIGHT_FLUSH,FIVE_KIND { only possible with wilds });

function EvaluateHand(c1:card;c2:card;c3:card;c4:card;c5:card):handrank;

(* DEPRECATED INTERFACE *)
	
const
	MAX_PLAYERS=6; { including the human }
	maxHandSize=11;

	ACEPIP			=TACE;
	TWOPIP			=TTWO;
	THREEPIP		=TTHREE;
	FOURPIP			=TFOUR;
	FIVEPIP			=TFOUR;
	SIXPIP			=TSIX;
	SEVENPIP		=TSEVEN;
	EIGHTPIP		=TEIGHT;
	NINEPIP			=TNINE;
	TENPIP			=TTEN;
	JACKPIP			=TJACK;
	QUEENPIP		=TQUEEN;
	KINGPIP			=TKING;

	CLUBSUIT		=TCLUB;
	DIAMONDSUIT		=TDIAMOND;
	HEARTSUIT		=THEART;
	SPADESUIT		=TSPADE;

	JOKERPIP		=TJOKER;

type
	pokerHandTypeSet=set of handrank;
	pokerGameId=(DRAW_POKER,STUD_POKER);
	playerIndex=1..MAX_PLAYERS;
	PlayerStatus=(NOSTATUS,PASSED,OPENED,FOLDED,RAISED,CALLED,CHECKED,BET);
	cardCountArray=array[TPip, TSuit] of 0..1;
	evaluationMode=(
		{$ifndef VER1}
		lowEval,
		{$endif}
		highEval
		);
	scoringType = (
		playHigh
		{$ifndef VER1}
		,playLow,playHighLow
		{$endif}
		);
	scrngDcsnTyp=record
		dcdd:boolean; { has decided? }
		scrMd:scoringType; { which score mode player is going for }
	end;

	indexArray=array[1..maxHandSize] of shortInt;
	{ used to point out specific cards from a hand }
	handIndexType=record
		n:shortInt; { how many values in "idxs" array }
		idxs:indexArray;
	end;

	PlayingCard=TCard;
	handType=record
		n:shortInt; { how many cards are currently in the hand }
		card:array[1..maxHandSize] of PlayingCard;
	end;

	pokerHandEvaluation=object
		rslt:handrank;
		wild:handType; { a copy of the playing hand with any wild cards
			replaced by their 'chosen' value. Most of the work is done
			with this hand. The original hand is only used for display. }
		set1:handIndexType; { card indexes that make up the "rslt", in rank order }
		set2:handIndexType; { remaining card indexes, in rank order}
		function pip:TPip;
		function pip2:TPip;
	end;

	pkrResultType=record
		eh, { high result when playing a HIGH or HIGH/LOW game or low result when playing a LOW game }
		el:pokerHandEvaluation; { low result when playing a HIGH/LOW game }
	end;

	WildType=(
		wildNone,
		wildjoker,
		wild2s,
		wild1ijs,
		wildmust,
		wild2s3s,
		wildKs
		{$ifndef VER1}
		,wildUser
		{$endif}
		);
		
	wildCardList=record
		wList:array[acePip..kingPip,ClubSuit..SpadeSuit] of boolean; { true if card is wild }
		jWild:boolean; { true if joker is wild }
	end;

const
	_wldTyp_:wildType=WildNone; { current wild card type }
	handSize:integer=5; { current hand size }
	shortHandFlag:boolean=false; { allow the calcuation of short hands
		like a '4-flush' or 'outside straight' when evaluating poker hands }

var
	{ ??? these should really be hidden from the callers }
	_wilds_:wildcardList; { current active wildcard list }
	the_current_bet:integer;
	totRaiseCnt:word; { counts the raises in each round }
	plrRaiseCnt:array[playerIndex] of word; { counts the raises in each round by each player }
	the_card_pool:array[playerIndex] of cardCountArray; { card count array (0 or 1) }

function cmpHands(r1,r2:pokerHandEvaluation):integer;
function getEvaluationMode:evaluationMode;
function getScoreMode:scoringType;
function IsWild(pc:playingCard):boolean;
function rank(p:TPip):integer;
function maxHandType:handrank;
function nextPokerHand(r:handrank):handrank;

procedure evaluate(hand:handType;var result:pokerHandEvaluation);
procedure pokerResult(hand:handType;var result:pkrResultType);
procedure sortIdx(var idx:handIndexType;hand:handType);
procedure otherIdx(i1:handIndexType;n:integer;var i2:handIndexType);
procedure setEvaluationMode(em:evaluationMode);
procedure evaluateBest(hand:handType;n:integer;var result:pokerHandEvaluation);
procedure extPokerResult(hand:handType;nCards:integer;var result:pkrResultType);
procedure setScoreMode(sm:scoringType);

implementation

uses
	std;
	
const
	_scrType_:scoringType=playhigh; { current game scoring type }
	_evalMode_:evaluationMode=HighEval; { current evaluation mode, High or Low, determined by current value of "_scrType_" variable }
	HighAce:boolean=true; { True when the ace ranks high in a High Score game. Always False in a lowball game  }
	keepGoingFlag:boolean=true;

var
	bestEval:pokerHandEvaluation;
	bestRank:integer;
	bestHand:handType; { The best hand evaluated so far with wildcards
		replaced by their assumed value. This hand is used by the main
		"pokerResult" function to assign wildcard pip values after all
		is done. }
	wildsInHand:boolean; { any wild cards in this hand }
	tempHand:handType;
	tempIndex:handIndexType;
	was_straight:boolean;
	was_twoPair:boolean;

function keepGoing(r:handrank):boolean;

	{ Return true if the "pokerResult" function should continue to
		evaluate the hand past type "r". 

		"r" is the point at which the evaluation function is at.

		This is used by the "extPokerresult" function to limit the amount
		of checking that the "pokerResult" function has to do since if
		the one we are checking ("r") is not even as good as the best we have
		so far then there is no need to continue.

    When "_evalMode_" is "lowEval" and the hand contains wild cards then
		we must always continue in order to ensure we make the lowest possible
		hand with the wilds .}

	begin

		keepGoing:=(
			keepGoingFlag
			or
			((_evalMode_=highEval) and (r>=bestEval.rslt))
			{$ifndef VER1}
			or
			((_evalMode_ = lowEval) and (r>=bestEval.rslt))
			{$endif}
			);

	end;

function bestCard(h:handType;x:handIndexType):integer;

{ Return the rank of the highest card of hand "h" thru "x" not including wilds. }

var
	i:shortInt;
	best:integer;
	c:playingCard;

begin
	best:=-1;
	for i:=1 to x.n do begin
		c:=h.card[x.idxs[i]];
			if (not isWild(c)) and (rank(CardPip(c))>best) then
			best:=rank(CardPip(c));
	end;
	bestCard:=best;
end;

function func_nKind(a:pointer;n:integer):boolean;

{ This function is called by the permutation generator.

	It checks for "n"-of-a-kind in "tempHand". "a" points to an array
	of "n" integers that are the indexes into "tempHand" of the cards
	to be checked.

	Return FALSE so that the next permutation will be generated
	automatically. When it is finished the variable "bestRank" will
	be > -1 if there *was* "n"-of-a-kind. }

var
	i:shortInt;
	c,t:playingCard;
	index:handIndexType;
	best:integer;

begin
	index.n:= n;
	for i:= 1 to n do index.idxs[i]:= intArray(a^)[i - 1];

	c:= temphand.card[index.idxs[1]];
	for i:= 2 to n do begin
		t:= temphand.card[index.idxs[i]];
		if (not isWild(t)) then begin
			if isWild(c) then
				c:=t
			else if (CardPip(t) <> CardPip(C)) then begin
				func_nKind:= false; { generate the next permutation }
				exit;
			end;
		end;
	end;

	{ falling thru to here means that the hand is "n" of a kind }
	{ keep this ranking and index if this is the best so far }
	best:= bestCard(tempHand,index);
	if best > bestRank then begin
		bestRank:=best;

		{ Assign the array of indexes "a" to "tempIndex". }

		tempIndex.n:=n;
		for i:=1 to n do tempIndex.idxs[i]:=intArray(a^)[i - 1];
		func_nKind:= (not wildsInHand);
	end
	else
		func_nKind:= false;
end;

function rank2pip(r:integer):TPip;

	{ Return the pip value of rank "r" with aces high or low
		depending on the state of the "HighAce" variable. }

	begin
		if (_evalMode_=highEval) then
			if r=rank(acePip) then
				rank2pip:=acePip
			else
				rank2pip:=TPip(r-1)
		else
			rank2pip:=TPip(13-r);
	end;

function is_nKind(n:integer;hand:handType;var index:handIndexType):boolean;

{ Return true if "hand" contains "n"-of-a-kind with wildcards. }

var
	i:integer;

begin
	bestRank:= -1;
	if hand.n >= n then begin
		tempHand:=hand;
		combinations(hand.n, n, @func_nKind);
		if bestRank > -1 then begin
			index:=tempIndex;
			sortIdx(index,hand);
		end;
	end;

	if bestRank > -1 then begin
		bestHand:=hand;
		for i:=1 to hand.n do if isWild(hand.card[i]) then
			bestHand.card[i]:=MakeCard(rank2pip(bestRank), spadeSuit);
	end;
	is_nKind:= (bestRank > -1);
end;

function is_noPair(hand:handType;var index:handIndexType):boolean;

	{ This function is only called when playing Lowball and when there
		are wild cards. The "hand" must be stripped of all wild cards. }

	var

 		i,j:integer;

 	begin

   	with hand do
    	for i:=1 to n do
				for j:=1 to n do if
					(i <> j)
					and
					(CardPip(card[i]) = CardPip(card[j]))
				then begin
					is_noPair:=false;
					exit;
				end;
	 is_noPair:=true;

	end;

function is_onePair(hand:handType;var index:handIndexType):boolean;

	begin
		is_onePair:=is_nKind(2, hand, index);
	end;

function func_twoPair(a:pointer;n:integer):boolean;

	{ Called by the permutation function with a set of 4 card indexes. }

	var

		i:integer;

	begin
		{HighAce:=true;}
		tempIndex.n:=4;
		for i:=1 to 4 do tempIndex.idxs[i]:=intArray(a^)[i - 1];
		sortIdx(tempIndex,tempHand);
		with tempHand do was_twoPair:=
			(CardPip(card[tempindex.idxs[1]]) = CardPip(card[tempindex.idxs[2]]))
			and
			(CardPip(card[tempindex.idxs[3]]) = CardPip(card[tempindex.idxs[4]]));
		func_twoPair:= was_twoPair;
	end;

function is_twoPair(hand:handType;var index:handIndexType):boolean;

	{ Return true if "hand" contains 2 pairs. the "hand" must have at
		least 4 cards.

		Note that this can't be done with wildcards since 1 wildcard in
		your hand would make 3-of-a-kind before it makes 2-pair. }

	begin
		{HighAce:=true;}
  	was_twoPair:=false;
  	if hand.n>=4 then begin
	  	tempHand:=hand;
			combinations(hand.n,4,@func_twoPair);
    end;
    index:=tempIndex;
    if was_twoPair then sortIdx(index,hand);
		is_twoPair:=was_twoPair;
	end;

function is_threeKind(hand:handType;var index:handIndexType):boolean;

	begin
		is_threeKind:=is_nKind(3,hand,index);
	end;

function isStraight(hand:handType;var index:handIndexType):boolean;

	var

		i,j:integer;
		temp:handIndexType;
		saveHighAce:boolean;

	begin

		saveHighAce:=HighAce;

		with index do begin
			n:=hand.n;
			for i:=1 to hand.n do idxs[i]:=i;
    end;
		HighAce:=true;
		sortIdx(index,hand);

		for i:=2 to hand.n do if
			rank(CardPip(hand.card[index.idxs[i]]))
			<>
			(rank(CardPip(hand.card[index.idxs[i-1]])) - 1)
		then begin
			HighAce:=false;
			sortIdx(index,hand);
			for j:=2 to hand.n do if
				rank(CardPip(hand.card[index.idxs[j]]))
				<>
				(rank(CardPip(hand.card[index.idxs[j-1]])) - 1)
			then begin
				isStraight:=false;
				HighAce:=saveHighAce;
				exit;
			end;
		end;

		HighAce:=saveHighAce;
		isStraight:=true;

	end;

var
	testHand:handType;
  bestsRank:integer;
  bestsHand:handtype;
  bestIndex,realIndex,wildIndex:handIndexType;

procedure otherIdx(i1:handIndexType;n:integer;var i2:handIndexType);

	{ Assign the remaining indexes of the hand to "i2". }

	var

		i:shortInt;

	function isin(t:shortInt):boolean;

		var

			i:integer;

		begin
			for i:=1 to i1.n do if i1.idxs[i]=t then begin
				isin:=true;
				exit;
			end;
			isin:=false;
		end;

	begin
		i2.n:=0;
		for i:=1 to n do if not isin(i) then begin
			inc(i2.n);
			i2.idxs[i2.n]:=i;
		end;
	end;

function f_straight(a:pointer;n:integer):boolean;

	var
  	i:integer;

	begin
	{ assign the pip values represented by "a^" to the wild cards in
    	the hand }
		for i:=1 to n do
			testHand.card[wildIndex.idxs[i]]:=MakeCard(TPip(intArray(a^)[i - 1] - 1), spadeSuit);
		if
			isStraight(testHand,tempIndex)
			and
			(rank(CardPip(testHand.card[tempIndex.idxs[1]])) >bestsRank)
		then begin
			bestsRank:= rank(CardPip(testHand.card[tempIndex.idxs[1]]));
			bestIndex:= tempIndex;
			bestsHand:= testHand; { keep the hand that made the best result }
			f_straight:= (not wildsInHand);
		end
		else
			f_straight:=false; { generate the next permutation }
  end;

function is_straight(hand:handType;var index:handIndexType):boolean;

	{ Return true if "hand" makes a STRAIGHT including wildcards. }

  var

	i:shortInt;

	begin

  	bestsRank:=-1;

    { test out the raw hand }

    if isStraight(hand,index) then begin
			bestsRank:=rank(CardPip(hand.card[index.idxs[1]]));
      bestIndex:=index;
    end;

    { now check out any wilds by creating an index to the wild cards in
			the hand }

    realIndex.n:=0;
	 for i:=1 to hand.n do if not isWild(hand.card[i]) then with realIndex do begin
		inc(n);
      idxs[n]:=i;
    end;
	 otherIdx(realIndex,hand.n,wildIndex);

	 { if there are wild cards then generate all combinations of
			"wildIndex.n" pips from all the possible pip values and see if
			they make a STRAIGHT }

    if wildIndex.n>0 then begin
    	testHand:=hand;
		combinations(ord(kingPip)+1,wildIndex.n,@f_straight);
    end;

    index:=bestIndex;
		bestHand:=bestsHand;

  	is_straight:=(bestsRank>-1);

	end;

function is_flush(hand:handType;var index:handIndexType):boolean;

	{ Return true if every card in "hand" is the same suit with	wildcards. }

	var

		i:shortInt;
    realHand:handType;
	 r:integer;

  function inBestHand(r:integer):boolean;

  	{ return true if rank "r" is in the best hand }

    var

    	i:shortInt;

	 begin

    	inBestHand:=false;
			with bestHand do for i:=1 to n do if rank(CardPip(card[i]))=r then begin
      	inBestHand:=true;
      	exit;
      end;

	 end;

	begin

  	{ make up a temp hand of only the real cards, if any }

    realHand.n:=0;
	 for i:=1 to hand.n do if not isWild(hand.card[i]) then with realHand do begin
    	inc(n);
	    card[n]:=hand.card[i];
    end;

    { if it is a flush then all the cards are involved }

		with index do begin
			n:=hand.n;
			for i:=1 to n do idxs[i]:=i;
		end;

		with realHand do if n>1 then
			for i:=2 to n do if (CardSuit(card[i]) <> CardSuit(card[1])) then begin
				is_flush:=false;
				exit;
			end;

		{ assign the best possible pip values to the wildcards in the
			hand without duplication }

		{HighAce:=true;}
    bestHand:=hand;
    for i:=1 to hand.n do if isWild(hand.card[i]) then begin
			r:=rank(acePip);
      while inBestHand(r) do dec(r);
			if realHand.n>0 then
				bestHand.card[i]:=MakeCard(rank2pip(r), CardSuit(realHand.card[1]))
			else
				bestHand.card[i]:=MakeCard(rank2pip(r),spadeSuit);
		end;
		sortIdx(index,bestHand);

		is_flush:=true;

	end;

function is_fullHouse(hand:handType;var index:handIndexType):boolean;

	{ Return true if "hand" makes a Full House with wildcards. }

	var

		i:integer;
		o:handIndexType;

	begin

		{HighAce:=true;}

		if (hand.n=5) and is_threeKind(hand,index) then begin
			otherIdx(index,hand.n,o);
			with hand do if
				CardPip(card[o.idxs[1]])
				=
				CardPip(card[o.idxs[2]])
			then begin
			with index do for i:=1 to o.n do idxs[n+i]:=o.idxs[i];
        index.n:=5;
        is_fullHouse:=true;
      end
	  	else
   			is_fullHouse:=false;
    end
  	else
    	is_fullHouse:=false;

	end;

function is_fourKind(hand:handType;var index:handIndexType):boolean;

	begin

		is_fourKind:=is_nKind(4,hand,index);

	end;

function is_straightFlush(hand:handType;var index:handIndexType):boolean;

	begin

		is_straightFlush:=(is_flush(hand,index) and is_straight(hand,index));

	end;

function is_fiveKind(hand:handType;var index:handIndexType):boolean;

	begin

		is_fiveKind:=is_nKind(5,hand,index);

	end;

function pokerHandEvaluation.pip:TPip;

	{ Return the pip rank of a poker result. }

	begin
		pip:= cards.CardPip(wild.card[set1.idxs[1]]);
	end;

function pokerHandEvaluation.pip2:TPip;

	{ Return the pip value of a poker result where applicable. }

	begin
		case rslt of
			RUNT:pip2:= cards.CardPip(wild.card[set2.idxs[1]]);
			TWO_PAIR:pip2:= cards.CardPip(wild.card[set1.idxs[3]]);
			FULLHOUSE:pip2:= cards.CardPip(wild.card[set1.idxs[4]]);
		end;
	end;

procedure pokerResult(hand:handType;var result:pkrResultType);

{ Returns a poker hand evaluation in "result" based on the current
	poker game parameters like wildcards, high play, low play , etc. }

begin
	case _scrType_ of
	    playHigh:begin
			_evalMode_:=highEval;
			evaluate(hand,result.eh);
		end;
		{$ifndef VER1}
		playHighLow:begin
			_evalMode_:=highEval;
			evaluate(hand,result.eh);
			_evalMode_:=lowEval;
			evaluate(hand,result.el);
		end;
		playLow:begin
			_evalMode_:=lowEval;
			evaluate(hand,result.eh);
		end;
		{$endif}
	end;

end;

function anyWilds(h:handType):boolean;

	{ Return true if hand "h" has one or more wildcards in it. }

  var

	i:integer;

	begin

  	anyWilds:=false;
  	with h do for i:=1 to n do if isWild(card[i]) then begin
    	anyWilds:=true;
      exit;
    end;

	end;

procedure evaluate(hand:handType;var result:pokerHandEvaluation);

	{ Evaluate the poker hand "hand" (5 cards maximum) according to the
		current setting of the "_evalMode_" variable with wildcards
		substitutions.

		Put the best result in "result".

		If the "shortHandFlag" variable is true on entry to this procedure
		then evaluate for short straights and flushes as well. }

	var

		i:integer;
		index,tidx:handIndexType;
	 tHand:handType;

	function XYZ:boolean;

		begin
			XYZ:=
				{$ifndef VER1}
				not (_EvalMode_ = LowEval);
				{$else}
				TRUE
				{$endif}
		end;

	procedure assign_noPair;

	{ The hand is a runt so assign the indexes and sort it }

		var

		i:integer;

		begin

			{HighAce:=true;}
			with index do begin
				n:=hand.n;
				for i:=1 to hand.n do idxs[i]:=i;
			end;
			sortIdx(index,hand);
			bestHand:=hand;
			if (hand.n=1) and isWild(hand.card[1]) then
				bestHand.card[1]:=MakeCard(acePip,spadeSuit);
			index.n:=1;
			result.rslt:=RUNT;

		end;

	function posIn(h:handType;pc:playingCard):integer;

		{ Return the position (1..n) of playing card "pc" in hand "h". }

		var
			i:integer;

		begin
			i:=1;
			while (h.card[i] <> pc)
			do inc(i);
			posIn:=i;
		end;

begin
	wildsinHand:=(_wldTyp_<>wildNone) and anyWilds(hand);
	result.wild:=hand;
	if (not wildsInHand) then begin
		if (xyz and keepGoing(STRAIGHT_FLUSH)) and ((shortHandFlag) or (hand.n=5)) and (is_straightFlush(hand,index)) then
			result.rslt:=STRAIGHT_FLUSH
		else if (keepGoing(FOUR_KIND)) and (hand.n>=4) and is_fourKind(hand,index) then
			result.rslt:=FOUR_KIND
		else if (keepGoing(FULLHOUSE)) and (hand.n=5) and is_fullHouse(hand,index) then
			result.rslt:=FULLHOUSE
		else if (xyz and keepGoing(flush)) and ((shortHandFlag) or (hand.n=5)) and (is_flush(hand,index)) then
			result.rslt:=flush
		else if (keepGoing(STRAIGHT)) and ((shortHandFlag) or (hand.n=5)) and (is_straight(hand,index)) then
			result.rslt:=STRAIGHT
		else if (keepGoing(THREE_KIND)) and (hand.n>=3) and is_threeKind(hand,index) then
			result.rslt:=THREE_KIND
		else if (keepGoing(TWO_PAIR)) and (hand.n>=4) and is_twoPair(hand,index) then
			result.rslt:=TWO_PAIR
		else if (keepGoing(ONE_PAIR)) and (hand.n>=2) and is_onePair(hand,index) then
			result.rslt:=ONE_PAIR
		else
			assign_noPair;
	end
	else if _evalMode_=highEval then begin
		HighAce:=true;
		if (keepGoing(FIVE_KIND)) and (hand.n=5) and is_fiveKind(hand,index) then
			result.rslt:=FIVE_KIND
		else if (keepGoing(STRAIGHT_FLUSH)) and ((shortHandFlag) or (hand.n=5)) and (is_straightFlush(hand,index)) then
			result.rslt:=STRAIGHT_FLUSH
		else if (keepGoing(FOUR_KIND)) and (hand.n>=4) and is_fourKind(hand,index) then
			result.rslt:=FOUR_KIND
		else if (keepGoing(FULLHOUSE)) and (hand.n=5) and is_fullHouse(hand,index) then
			result.rslt:=FULLHOUSE
		else if (keepGoing(flush)) and ((shortHandFlag) or (hand.n=5)) and (is_flush(hand,index)) then
			result.rslt:=flush
		else if (keepGoing(STRAIGHT)) and ((shortHandFlag) or (hand.n=5)) and (is_straight(hand,index)) then
			result.rslt:=STRAIGHT
	else if (keepGoing(THREE_KIND)) and (hand.n>=3) and is_threeKind(hand,index) then
			result.rslt:=THREE_KIND
	else if (keepGoing(TWO_PAIR)) and (hand.n>=4) and is_twoPair(hand,index) then
			result.rslt:=TWO_PAIR
	else if (keepGoing(ONE_PAIR)) and (hand.n>=2) and is_onePair(hand,index) then
			result.rslt:=ONE_PAIR
		else
			assign_noPair;

	end
{$ifndef VER1}
	else if _evalMode_ = lowEval then begin

		{ create a temporary hand that contains only the non-wild cards from
			the original hand }

		tHand.n:=0;
		for i:=1 to hand.n do if not isWild(hand.card[i]) then begin
			inc(tHand.n);
			tHand.card[tHand.n]:=hand.card[i];
		end;

		if is_noPair(thand,tidx) then
			result.rslt:=RUNT
		else if (keepGoing(ONE_PAIR)) and (thand.n>=2) and is_onePair(thand,tidx) then
			result.rslt:= ONE_PAIR
		else if (keepGoing(TWO_PAIR)) and (thand.n>=4) and is_twoPair(thand,tidx) then
			result.rslt:= TWO_PAIR
		else if (keepGoing(THREE_KIND)) and (thand.n>=3) and is_threeKind(thand,tidx) then
			result.rslt:= THREE_KIND
		else if (keepGoing(FOUR_KIND)) and (thand.n>=4) and is_fourKind(thand,tidx) then
			result.rslt:= FOUR_KIND;

		bestHand:=hand;

		{ assign the lowest possible pip values to the wild cards }

		assignLowPips(bestHand);

		if result.rslt=noPair then with hand do begin
			index.n:=n;
			for i:=1 to n do index.idxs[i]:=i;
			sortIdx(index,besthand);
			index.n:=1;
		end
		else begin

			{ now reassign the indexes back to the original hand }

			index.n:=tIdx.n;
			for i:=1 to tIdx.n do
				index.idxs[i]:=posIn(hand,tHand.card[tidx.idxs[i]]);
		end;

	end
{$endif};

	{ assign and sort the indexes of the cards in the hand that make
		up the result and the left over cards }
	result.set1:=index;

	with result do begin
		otherIdx(set1,hand.n,set2);
		sortIdx(set2,besthand);
		{ assign the hand with appropriate wildcard replacements }
		for i:=1 to hand.n do if isWild(hand.card[i]) then result.wild.card[i]:=bestHand.card[i];
	end;
end;

procedure sortIdx(var idx:handIndexType;hand:handType);

	{ Sort an index "idx" according to the rank of cards in "hand". }

	var

		i,j,k:integer;
		t:shortInt;

	begin

		{ Sort the indexes that make a poker hand by a simple "STRAIGHT
			selection" sort algorithm. }

		with idx do for i:=1 to n-1 do begin
			k:=i;
			t:=idxs[i];
			for j:=i+1 to n do
				if
					rank(cards.CardPip(hand.card[idxs[j]]))
					>
					rank(cards.CardPip(hand.card[t]))
				then begin { ???
					high/low 7-stud with no wilds got a range check here }
					k:=j;
					t:=idxs[j];
				end;
			idxs[k]:=idxs[i];
			idxs[i]:=t;
		end;

	end;

function isWild(pc:playingCard):boolean;

	{ Return true if "pc" is a wildcard. }

	begin
		isWild:=
			(_wldTyp_<>wildNone)
			and
			(
				(
					(CardPip(pc) = jokerPip)
					and
					_wilds_.jWild
				)
				or
				(
					(CardPip(pc)<>jokerPip)
					and
					_wilds_.wList[CardPip(pc), CardSuit(PC)]
				)
			);
	end;

function Rank(p:TPip):integer;

	{ Return the rank of pip "p" (Ace..King) as follows:

			A:1 (when "HighAce" = false )
			2:2
			3:3
			...
			J:11
			Q:12
			K:13
			A:14 (when "HighAce" = true ) }

	var
		r:integer;

	begin
		r:=ord(p)+1;
		if HighAce and (r=1) then r:=ord(kingPip)+2;
		rank:=r;
	end;

procedure setEvaluationMode(em:evaluationMode);

	{ Explicitly set the current ealuation mode. }

	begin
		{$ifndef VER1}
		if em = LowEval then HighAce:=False;
		{$endif}
		_evalMode_:=em;
	end;

function getEvaluationMode:evaluationMode;

	{ Return the current ealuation mode. }

	begin

		getEvaluationMode:=_evalMode_;

	end;

var
	extHand:handType;
	XBest:HandIndexType;
	XIndex,YIndex:handIndexType;

function cmpHands(r1,r2:pokerHandEvaluation):integer;

	{ Compare two poker evaluations "r1" and "r2" and return:

			-1 if h1<h2

			 0 if h1=h2

			+1 if h1>h2
			
		}

	var

  	i:integer;
		i1,i2:shortInt;
		p1,p2:TPip;

	begin

	if
			((_evalMode_=highEval) and (r1.rslt>r2.rslt))
			{$ifndef VER1}
			or
			((_evalMode_=lowEval) and (r1.rslt<r2.rslt))
			{$endif}
		then begin
			cmpHands:=1;
			exit;
		end;

    if
			((_evalMode_=highEval) and (r2.rslt>r1.rslt))
			{$ifndef VER1}
			or
			((_evalMode_=lowEval) and (r2.rslt<r1.rslt))
			{$endif}
		then begin
			cmpHands:=-1;
			exit;
		end;

		{ Falling thru to here means that the two hands are the same "Rslt"
			(i.e.: both are a Pair or a Flush etc.) so now we must check
			card for card thru the hands. }

		for i:=1 to min(5,min(r1.wild.n,r2.wild.n)) do begin
			if i>r1.set1.n then begin
				i1:=r1.set2.idxs[i-r1.set1.n];
				i2:=r2.set2.idxs[i-r1.set1.n];
			end
			else begin
				i1:=r1.set1.idxs[i];
				i2:=r2.set1.idxs[i]
			end;

			p1:=CardPip(r1.wild.card[i1]);
			p2:=CardPip(r2.wild.card[i2]);

			if _EvalMode_=HighEval then begin
				if rank(p1)>rank(p2) then begin
					cmpHands:=1;
					exit;
				end
				else if rank(p1)<rank(p2) then begin
					cmpHands:=-1;
					exit;
				end;
			end
			else begin
				if rank(p1)<rank(p2) then begin
					cmpHands:=1;
					exit;
				end
				else if rank(p1)>rank(p2) then begin
					cmpHands:=-1;
					exit;
				end;
			end
		end;

 		cmpHands:=0;

  end;

procedure setScoreMode(sm:scoringType);

	{ Sets the current poker hand scoring mode to "sm". }

	begin

		case sm of
			PlayHigh:_evalMode_:=HighEval;
			{$ifndef VER1}
			PlayLow:_evalMode_:=LowEval;
			{$endif}
		end;
		_scrType_:=sm;

  end;

function getScoreMode:scoringType;

	{ Return the current scoring mode. }

	begin

	getScoreMode:=_scrType_;

  end;

function maxHandType:handrank;

	{ Return the current maximum hand type that a hand can have in the
		current setup. }

	begin

		if (_wldTyp_=wildNone) then
			maxHandType:=STRAIGHT_FLUSH
		else
			maxHandType:=FIVE_KIND;

	end;

function nextPokerHand(r:handrank):handrank;

	{ Return the next better poker hand type for "r". }

	begin

		if _EvalMode_=highEval then
			if (r<maxHandType) then
				nextPokerHand:=succ(r)
			else
				NextPokerHand:=r
		else if (r>RUNT) then
			nextPokerHand:=pred(r)
		else
			NextPokerHand:=r;

	end;

function handCheckX(a:pointer;n:integer):boolean;

	var

		i:integer;
		tempHand:handType;
		tempEval:pokerHandEvaluation;

	begin

		{ setup an "n" card testhand from the original hand }

		TempHand.n:=n;
		for i:=1 to n do TempHand.card[i]:=extHand.card[intArray(a^)[i - 1]];

		evaluate(tempHand,tempEval);
		if cmpHands(tempEval,bestEval)>0 then begin

			{ Keep the indexes into the original hand in this global variable }

			XBest.N:=N;
			for i:=1 to N do XBest.Idxs[i]:=intArray(a^)[i - 1];

			bestEval:=tempEval;

			{ keep the indexes into the original hand of this winning
				combination of "n" cards }

			with tempEval.set1 do begin
				xIndex.n:=n;
				for i:=1 to n do xIndex.idxs[i]:=intArray(a^)[idxs[i] - 1];
			end;
			OtherIdx(XIndex,N,YIndex);
		end;

		handCheckX:=false;

	end;

procedure evaluateBest(hand:handType;N:integer;var result:pokerHandEvaluation);

	{ Return the best poker hand evaluation of any "n" cards from "hand"
		according to the current setting of the "_evalMode_" variable. }

	var

		i:shortInt;
		TempHand:HandType;

	begin

		extHand:=hand;
		result.wild:=hand;

		{ take the first "n" cards as the best yet }

		TempHand:=hand;
		TempHand.n:=n;
		evaluate(TempHand,bestEval);
		XBest.N:=N;
		for i:=1 to N do XBest.Idxs[i]:=i;

		keepGoingFlag:=false;
		combinations(hand.n,n,@handCheckX);
		keepGoingFlag:=true;

		with result do begin

			rslt:=bestEval.rslt;

			{ Convert the indexes from the best "N" card hand to the indexes 
				from the original "hand". Also replace the wildcards in the 
				evaluated "Wild" hand with those from the "BestEval" result. }

			set1.N:=BestEval.Set1.N;
			for i:=1 to Set1.N do begin
				Set1.Idxs[i]:=XBest.Idxs[BestEval.Set1.Idxs[i]];
				Wild.Card[XBest.Idxs[BestEval.Set1.Idxs[i]]]:=BestEval.Wild.Card[BestEval.Set1.Idxs[i]];
			end;
			set2.N:=BestEval.Set2.N;
			for i:=1 to Set2.N do begin
				Set2.Idxs[i]:=XBest.Idxs[BestEval.Set2.Idxs[i]];
				Wild.Card[XBest.Idxs[BestEval.Set2.Idxs[i]]]:=BestEval.Wild.Card[BestEval.Set2.Idxs[i]];
			end;
		end;

	end;

procedure extPokerResult(hand:handType;nCards:integer;var result:pkrResultType);

	{ Return the best poker hand evaluation of any "nCards" cards from "hand". }

	var

		i:shortInt;

	begin
		{$ifndef VER1}
		if _ScrType_ = PlayHighLow then SetEvaluationMode(HighEval);
		{$endif}
		EvaluateBest(Hand,NCards,Result.EH);
		{$ifndef VER1}
		if _ScrType_=PlayHighLow then begin
			SetEvaluationMode(LowEval);
			EvaluateBest(Hand,NCards,Result.EL);
		end;
		{$endif}
	end;

function EvaluateHand(c1:card;c2:card;c3:card;c4:card;c5:card):handrank;

var
	hand:handtype;
	evaluation:pokerHandEvaluation;
	
begin
	hand.n:=5;
	hand.card[1]:=c1;
	hand.card[2]:=c2;
	hand.card[3]:=c3;
	hand.card[4]:=c4;
	hand.card[5]:=c5;
	evaluate(hand,evaluation);
	EvaluateHand:=evaluation.rslt
end;

end.
