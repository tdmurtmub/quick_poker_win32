{ (C) 1999-2006 Wesley Steiner }

unit pokerlib;

interface

uses
	cards,
	poker;

const
	MinOddsTableN=100; { minimum usuable odds table sample size }

type
	pokerRaiseLimits=record
		lmtPerRnd:word; { max # of raises per round by all players }
		lmtPerPsn:word; { max # of raises per person per round }
	end;

const
	TotalHndCnt:longint=0; { how many hands we have generated so far for the current odds table }
	OddsTableLimit=5000; { stop automatically calculating the odds table
		when we have reached this many hands in order to improve
		general performance }
	hndOddsTbl:array[handrank] of longInt=(0,0,0,0,0,0,0,0,0,0); { see "buildOddsTable" }
	pokerHandDesc:array[handrank] of string[22]=(
		'Runt',
		'One Pair',
		'Two Pair',
		'Three of a Kind',
		'Straight',
		'Flush',
		'Full House',
		'Four of a Kind',
		'Straight Flush',
		'Five of a Kind'
		);

	wildTypeDesc:array[wildType] of string[16]=(
		'None',
		'Joker',
		'Deuces',
		'One-Eyed Jacks',
		'Mustached Kings',
		'Deuces and Treys',
		'Kings'
		{$ifndef VER1}
		,'Custom'
		{$endif}
		);

{ !!! may want to move this to a separate unit }
procedure buildOddsTable(howMany:integer);
procedure incOddsTable(howMany:integer);
procedure addOddsTable(e:pokerHandEvaluation);
procedure clearOddsTable;
function GetHandOdds(HSet:PokerHandTypeSet):real;

procedure ClearWildCards(var w:wildcardList);
procedure SetWildCards(wt:wildType;wl:wildCardList);
function DecidedHigh(g:scrngDcsnTyp):boolean;
{$ifndef VER1}
function DecidedLow(g:scrngDcsnTyp):boolean;
{$endif}

implementation

uses
	objects,std;

procedure addOddsTable(e:pokerHandEvaluation);

	{ Add "e" to the odds table "hndOddsTbl". }

	begin
		inc(hndOddsTbl[e.rslt]);
		inc(totalHndCnt);
	end;

procedure shuffleDeck(var Deck;nPacks,nTimes,Flags:Word);

	{(Deck:PDeck;nPacks,nTimes,Flags:Word);}

	var
		i,j,n:word;
		D:array[1..MaxPacks*TPackSizeMax] of TCard;

	procedure SwapCards(a,b:Word);

		{ Swap the "a"th and "b"th cards in the deck. }

		var
			t:TCard;

		begin
			t:=D[a];
			D[a]:=D[b];
			D[b]:=t;
		end;

	begin
	end;

procedure incOddsTable(howMany:integer);

	{ Add "howMany" more samples to the odds table "hndOddsTbl". }

	var
		t,i,j:integer;
		h:handType;
		e:pokerHandEvaluation;
		d:array[1..53] of playingCard;
		c:playingCard;
		p:TPip;
		s:TSuit;

	begin
		if TotalHndCnt=2000000 then Exit;
		{ make up the deck }
		t:=0;
		for p:=acePip to kingPip do
			for s:=clubSuit to spadeSuit do begin
				inc(t);
				d[t]:= MakeCard(p,s);
			end;
		c:= MakeCard(jokerPip, spadeSuit);
		if isWild(c) then begin
			inc(t);
			d[t]:=c;
		end;
		for j:=1 to howMany do begin
			{ make up a deck and shuffle it }
			shuffleDeck(d, 1, 20, 0);
			{ take the first "handSize" cards from the sample deck }
			h.n:=0;
			with h do for i:=1 to min(7,handSize) do begin
				inc(n);
				card[n]:=d[i];
			end;
			if h.n>5 then
				evaluateBest(h,5,e)
			else
				evaluate(h,e);
			inc(hndOddsTbl[e.rslt]);
			inc(totalHndCnt);
		end;
	end;

procedure clearOddsTable;

	{ Clear the odds table "hndOddsTbl". }

	begin
		totalHndCnt:=0;
		fillChar(hndOddsTbl,sizeof(hndOddsTbl),#0);
	end;

procedure buildOddsTable(howMany:integer);

	{ Add "howMany" more samples to the odds table "hndOddsTbl".

		The more times this procedure is called (by being scattered thru
		the application code) the more accurate will be the odds table. }

	begin
		clearOddsTable;
		incOddstable(howMany);
	end;

function GetHandOdds(HSet:PokerHandTypeSet):real;

	var
		H:handrank;
		A:LongInt;
	begin
		if TotalHndCnt=0 then
			BuildOddsTable(MinOddsTableN)
		else if TotalHndCnt<MinOddsTableN then
			IncOddsTable(MinOddsTableN-TotalHndCnt);
		A:=0;
		for H:= RUNT to FIVE_KIND do if (H in HSet) then A:=A+HndOddsTbl[H];
		GetHandOdds:=A/TotalHndCnt;
	end;

procedure clearWildCards(var w:wildcardList);

	{ Initialize "w" to no wildcards. }

	var

		p:TPip;
		s:TSuit;

	begin

		_wldTyp_:=wildNone;
		with w do begin
			for p:=acePip to kingPip do
				for s:=clubSuit to spadeSuit do
					wList[p,s]:=false;
			jWild:=false;
		end;

	end;

procedure setWildCards(wt:wildType;wl:wildCardList);

	{ Set the current wildcard list to wildcard type "wt". If "wt" is
		custom then use the wild card list "wl". }

	var

		p:TPip;
		s:TSuit;

	begin

		clearWildCards(_wilds_);

		_wldTyp_:=wt;
		with _wilds_ do begin
			case wt of
				wildJoker:jWild:=true;
				wild2s:for s:=clubSuit to spadeSuit do wList[twoPip,s]:=true;
				wild1ijs:begin
					wList[jackPip,heartSuit]:=true;
					wList[jackPip,spadeSuit]:=true;
				end;
				wildmust:begin { mustached kings }
					wList[kingPip,clubSuit]:=true;
					wList[kingPip,diamondSuit]:=true;
					wList[kingPip,spadeSuit]:=true;
				end;
				wild2s3s:begin
					for s:=clubSuit to spadeSuit do wList[twoPip,s]:=true;
					for s:=clubSuit to spadeSuit do wList[threePip,s]:=true;
				end;
				wildKs:
					for s:=clubSuit to spadeSuit do wList[kingPip,s]:=true;
				{$ifndef VER1}
				wildUser:
					_wilds_:=wl;
				{$endif}
			end;
		end;

	end;

procedure assignLowPips(var h:handType);

	{ Assign the lowest possible pip values to all the wild cards in
  	hand "h". }

  var

  	i:integer;

	function lowestPip:TPip;

  	{ Return the lowest pip not already in the hand, not including any
		wild cards. }

    var

			p:TPip;

    function notInBestHand:boolean;

    	{ return true if the pip value "p" is not already in the "bestHand"
				not including any wildcards. }

	    var

				j:integer;

			begin

				with h do for j:=1 to n do if (j<>i) and (CardPip(card[j]) = p) then begin
			notInBestHand:=false;
        	exit;
        end;
       	notInBestHand:=true;

    	end;

	begin

			for p:=acePip to kingPip do if notInBestHand then begin
       	lowestPip:=p;
        exit;
      end;

    end;

	begin
		{ replace the wild cards with the lowest possible pips }
		with h do for i:=1 to n do if isWild(card[i]) then
			card[i]:=MakeCard(lowestPip,spadeSuit);
	end;

function xxxx(g:scrngDcsnTyp;t:scoringType):boolean;
	{ Return true if "g" has not yet decided on which to go for or if
		he has decided then return true if he has decided on type "t". }
	begin
		xxxx:=
			(not g.dcdd)
			or
			(
				(g.dcdd)
				and
				(
				(g.scrMd=t)
				{$ifndef VER1}
				or
				(g.scrMd = playHighLow)
				{$endif}
				)
			);
	end;

function DecidedHigh(g:scrngDcsnTyp):boolean;
	begin
		DecidedHigh:=xxxx(g,playHigh);
	end;

{$ifndef VER1}
function DecidedLow(g:scrngDcsnTyp):boolean;
	begin
		DecidedLow:=xxxx(g, playLow);
	end;
{$endif}

end.


