(* (C) 2009 Wesley Steiner *)

{$MODE FPC}

{$I platform}

{$MACRO ON}
{$define deprecated_interface:=(* DEPRECATED INTERFACE *)}

{$inline on}

{$ifdef DEBUG}
{$define TEXT_CARDS}
{$endif}

unit cards;

interface

uses
	objects;

const
	NOCARD=$00;
	
type
	pip=(ACE=1,DEUCE,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,TEN,JACK,QUEEN,KING);
	suit=(CLUB=1,DIAMOND,HEART,SPADE);
	card=byte;

function CreateCard(p:pip;s:suit):card; inline;
function GetCardPip(card:card):pip; inline;
function GetCardSuit(card:card):suit; inline;

deprecated_interface

const
	TACE=0;TDEUCE=1;TTWO=TDEUCE;TTHREE=2;TFOUR=3;TFIVE=4;TSIX=5;TSEVEN=6;TEIGHT=7;TNINE=8;TTEN=9;TJACK=10;TQUEEN=11;TKING=12;TJOKER=13;
	TCLUB=0;TDIAMOND=1;THEART=2;TSPADE=3;
	TCLUBS=TCLUB;TDIAMONDS=TDIAMOND;THEARTS=THEART;TSPADES=TSPADE;
	MAXPACKS				= 8; { for type overide only }
	TPACKSIZE			= 52; { standard 52 cards in a deck }
	TPACKSIZEMAX		= TPackSize + 1; { with a TJOKER }
	NULL_CARD=$FF;
	NoJoker=False;
	FACEUP_BIT=$08;

type
	TSuit= TCLUB..TSPADE;
	TPip=TACE..TJOKER;
	TPips=set of TPip;
	PCard=^TCard;
	TCard=Byte; {
		Bit # 7 6 5 4 	-- pip (TACE..TKING, TJOKER) 0 <= Pip < 13
		Bit # 3 		-- 1 = card is faceup, 0 = card is facedown
		Bit # 2 		-- unused
		Bit # 1 0 		-- suit}

	PCardSet=^TCardSet;
	TCardSet=record
		{ a set of cards }
		Size:Word; { how many cards in the set }
		List:array[0..MaxPacks * TPackSizeMax] of TCard;
	end;

	PPileOfCards=^PileOfCards;
	PileOfCards=object
	{ A logical abstraction of a pile of cards. }
		Limit:Word; { max # of cards this pile can take }
		m_name:PString; { a logical name for the pile, "Tableau", "Foundation" etc... }
		ListPtr:PCardSet; { the array of cards }

		constructor Construct(aSize:Word);
		destructor Destruct; virtual;

		function CardIsPlaceHolder(aIndex:integer):boolean;
		function is3kind:boolean;
		function is4kind:boolean;
		function isRunInSuit:boolean;
		function IsEmpty:boolean;
		function isNextFacedown:boolean; virtual; { next card added to top is facedown? }
		function isFacedown(const cnum:Word):boolean;
		function isFaceup(const cnum:Word):boolean;
		function Gettop:TCard; virtual; { copy of the top card }
		function Get(const cnum:Word):TCard; virtual; { copy of the "cnum"th card (1..N) }
		function Remove(const cnum:Word):TCard; virtual; { removes the "cnum"th card }
		function Removetop:TCard; virtual; { removes the top card }
		function Ref(const cnum:Word):PCard; { pointer to "cnum"th card (1..N) }
		function Size:Word;
		function Discard(const iCard:integer):TCard;
		procedure Add(const aCard:TCard); virtual; { add a card to the top }
		procedure AddPacks(const WithJoker:boolean);
		procedure Shuffle; virtual;
		procedure Empty; { empty the pile }
		procedure Discardtop; virtual; { discards the top card }
		procedure snapAllTo(target:PPileOfCards);
		procedure moveAllTo(target:PPileOfCards);
		procedure AddPile(aPile:PPileOfCards); virtual;
		procedure flip; { physically flip the pile }
		procedure flipAll; { flip all cards over }
		procedure flipCard(const cnum:Word); virtual;
		procedure FlipFacedown(const cnum:Word);
		procedure Fliptop; virtual;
		procedure FliptopFacedown;
		procedure SettopFacedown;
		procedure SettopFaceup;
		procedure flipAllFaceup;
		procedure setAllFacedown;
		procedure swap(const index1, index2:word);
		procedure sort; { default sorting by Pip/Suit }
		procedure sortSuitPip; { sort by Suit/Pip }
		{$ifdef TEXT_CARDS}
		procedure Write;
		{$endif}

	private
		AllocSize:Word; { how many bytes allocated to the card list }
	end;

	PPack=^PackOfCards;
	PackOfCards=object
		Size:Byte; { how many cards currently in the pack }
		Cards:array[1..TPackSizeMax] of TCard;
		constructor Init(IfJoker:boolean);
		procedure AddCard(tc:TCard);
		procedure RemovePips(tps:TPips);
	end;

	PDeck=^DeckOfCards;
	DeckOfCards=object(PileOfCards)
		procedure Add(const aCard:TCard); virtual; { add a card to the top }
	end;

	PHand = ^THand;
	THand = object(PileOfCards)
	end;

{ [decks] }

const
	DECK_SIZE=52; { standard 52 cards in a deck }

type
	SuitColorType=(Black_Suit,Red_Suit);
	TSuitColor=SuitColorType;
	permCombFunc=function(a:pointer;n:integer):boolean; { this type of
		function gets called after each new combination/permutation is
		generated. The parameter "a" is an array of integers that represent
		the k-subset. Return false to continue with the generation of the
		next in the sequence otherwise return true. }

const
	PAceCh		='A';
	PTwoCh		='2';
	PThrCh		='3';
	PFouCh		='4';
	PFivCh		='5';
	PSixCh		='6';
	PSevCh		='7';
	PEigCh		='8';
	PNinCh		='9';
	PTenCh		='T';
	PJacCh		='J';
	PQueCh		='Q';
	PKinCh		='K';
	PipChr:array[TPip] of Char=(
		PAceCh,
		PTwoCh,
		PThrCh,
		PFouCh,
		PFivCh,
		PSixCh,
		PSevCh,
		PEigCh,
		PNinCh,
		PTenCh,
		PJacCh,
		PQueCh,
		PKinCh,
		'#');
	PipText:array[TACE..TJOKER] of PChar=(
		'Ace','Deuce','Three','Four','Five','Six','Seven','Eight','Nine','Ten','Jack','Queen','King','Joker');
	TCluCh='C';
	TDiaCh='D';
	THeaCh='H';
	TSpaCh='S';
	SuitChr:array[TCLUB..TSPADE] of Char=(TCluCh,TDiaCh,THeaCh,TSpaCh);
	SuitGChr:array[TCLUB..TSPADE] of Char=(#5,#4,#3,#6);

{$ifdef TEXT_CARDS}
procedure WriteCards(aCards:PileOfCards);
procedure WriteCard(aCard:TCard);
procedure WritePip(aPip:TPip);
procedure WriteSuit(aSuit:TSuit);
{$endif}

function CardFacedown(aCard:TCard):TCard;
function CardFaceup(aCard:TCard):TCard;
function CardIsFacedown(aCard:TCard):boolean;
function CardIsFaceup(aCard:TCard):boolean;
function CardIsPlaceHolder(card:TCard):boolean;
function CardsGet(const aPile:PileOfCards; cnum:Word):TCard; { copy of the "cnum"th card (1..N) }
function CardsGettop(aPile:PileOfCards):TCard; { copy of the top card }
function CardsRef(const aPile:PileOfCards; cnum:Word):PCard; { pointer to "cnum"th card (1..N) }
function CardsRemove(var aPile:PileOfCards; cnum:Word):TCard; { removes the "cnum"th card }
function CardsRemovetop(var aPile:PileOfCards):TCard; { removes the "cnum"th card }
function CardToTCard(card:Card):TCard;
function PipToTPip(p:pip):TPip;
function SuitToTSuit(s:suit):TSuit;
function TCardToCard(card:TCard):card;
procedure CardsAdd(var aPile:PileOfCards; aCard:TCard); { adds to the top of the pile }
procedure CardsAddPacks(var aPile:PileOfCards; WithJoker:boolean);
procedure CardsEmpty(var aPile:PileOfCards); { empty the pile }
procedure CardsInit(var aPile:PileOfCards; n:Word);
procedure CardsDone(var aPile:PileOfCards);
procedure CardsMovetop(var aSource:PileOfCards;var aTarget:PileOfCards);
procedure CardsMove(var aSource:PileOfCards; cnum:Word;var aTarget:PileOfCards);
procedure flipCard(var c:TCard);
procedure faceupCard(var c:TCard); // OBSOLETE! use CardFaceup
procedure CardsSetName(var aPile:PileOfCards; const aName:String);
procedure CardsShuffle(var aPile:PileOfCards; nTimes:Word);
function CardsSize(const aPile:PileOfCards):Word;
function CardPip(aCard:TCard):TPip; inline;
function CardSuit(aCard:TCard):TSuit; inline;
function SuitColor(const tc:TCard):TSuitColor;
function DeckPulltopCard(var Deck;nPacks:Word):TCard;
function DeckPullCard(var Deck;nPacks:Word;aCard:TCard):TCard;
function DeckPullPip(var Deck;nPacks:Word;aPip:TPip):TCard;
function PipCircSucc(aPip:TPip):TPip;
function PipCircPred(aPip:TPip):TPip;
function nPipCircSucc(aNum:word;aPip:TPip):TPip;
function nPipCircPred(aNum:word;aPip:TPip):TPip;
{function Text2Card(const aString:String):TCard;}
function TPipToPip(t:TPip):cards.pip; inline;
function TSuitToSuit(t:TSuit):cards.suit; inline;
function MakeCard(aPip:TPip;Suit:TSuit):TCard;
function nCombinations(n,r:word):longInt;
function nPermutations(n,r:word):longInt;
procedure combinations(n,r:word;func:permCombFunc);
function combArray(n,r:word;var nn:word):pointer;

implementation

uses
	{$ifdef TEST} cardsTests, {$endif}
	std,mathx;

function nPermutations(n,r:word):longInt;

	{ Return the number of permutations of "r" things from "n".


					n!
		P(n,r) = ------
						 (n-r)!

		}

	var

		acc:longInt;

	begin

	r:=n-r;
		if r=0 then r:=1;
		acc:=n;
		while n>r do begin
			dec(n);
			acc:=acc*n;
		end;

		nPermutations:=acc;

	end;

function nCombinations(n,r:word):longInt;

	{ Return the number of combinations of "r" things taken from "n".

						 n(n-1)(n-2)...(n-r+1)
		C(n,r) = ---------------------
											 r!

		}

	var

		acc:longInt;
		num:word;

	begin

		acc:=1;
		num:=n;
		while num>=n-r+1 do begin
			acc:=acc*num;
			dec(num);
	 end;

		nCombinations:=acc div Factorial(r);

	end;

procedure combinations(n,r:word;func:permCombFunc);

	{ Generate all the r-subset combinations of "n" things and call
		"func" after each combination is generated.

		This algorithm uses the techniques described in "Elements of Discrete
		Mathematics", Liu, pp 44-45. }

	var

		i:integer;
		m:integer;
		a:^intArray;

	procedure cleanup; begin freeMem(a,n*sizeOf(integer)); end;

	type
		xxxarray = array[1..32767] of integer;

	begin

		getMem(a,n*sizeOf(integer)); { array that we need }

		for i:=1 to r do xxxarray(a^)[i]:=i;

		m:=r;
		if func(a,r) then begin
			cleanup;
			exit;
		end;
		while (m>0) and (xxxarray(a^)[m]<n-r+m) do begin
			inc(xxxarray(a^)[m]);
			if m<=r-1 then for i:=m to r-1 do xxxarray(a^)[i+1]:=xxxarray(a^)[i]+1;
			if func(a,r) then begin
				cleanup;
				exit;
			end;
			m:=r;
			while (m>1) and (xxxarray(a^)[m-1]>xxxarray(a^)[m]) do dec(m);
			while (m>0) and (xxxarray(a^)[m]=n-r+m) do dec(m);
		end;
		cleanup;
	end;

var

	pIndex:integer;
	p:pointer;

function fillPermArray(a:pointer;n:integer):boolean;

	var

		i:integer;

	begin

		for i:=1 to n do begin
			intArray(p^)[pIndex]:=intArray(a^)[i];
			inc(pIndex);
		end;

		fillPermArray:=false;

	end;

function combArray(n,r:word;var nn:word):pointer;

	{ Generate an array in dynamic memory that contains all the
		combinations "r" things from "n".

		Return a pointer to the array and the number of entries
		combinations in "nn". }

	begin

		nn:=Word(nCombinations(n,r));
		getMem(p,nn*r*sizeOf(integer));
		pIndex:=1;
		combinations(n,r,@fillPermArray);

		combArray:=p;

	end;

function sort_func(a, b:byte):integer;

	{ return -(a<b), 0(a=b), +(a>b) }

	begin
		sort_func:= CardPip(a) - CardPip(b);
	end;

type
	indexArray=array[1..52] of shortInt;
	{ used to point out specific cards from a hand }
	handIndexType=record
		n:shortInt; { how many values in "idxs" array }
		idxs:indexArray;
	end;

	handType=record
		n:shortInt; { how many cards are currently in the hand }
		card:array[1..52] of TCard;
	end;

var
	tempHand:handType;
	tempIndex:handIndexType;

function func_nKind(a:pointer; n:integer):boolean;

	{ This function is called by the permutation generator.

		It checks for "n"-of-a-kind in "tempHand". "a" points to an array
		of "n" integers that are the indexes into "tempHand" of the cards
		to be checked.

		Return FALSE to generate the next permutation automatically.
		Return TRUE to stop the permutations. }

	var
		i:shortInt;
		c,t:TCard;
		index:handIndexType;

	begin
		index.n:=n;
		for i:=1 to n do index.idxs[i]:=intArray(a^)[i - 1];

		c:=temphand.card[index.idxs[1]];
		for i:=2 to n do begin
			t:=temphand.card[index.idxs[i]];
				if (CardPip(t) <> CardPip(c)) then begin
					func_nKind:=false; { generate the next permutation }
					exit;
				end;
		end;

		{ falling thru to here means that the hand is "n" of a kind }

		{ Assign the array of indexes "a" to "tempIndex". }

		tempIndex.n:= n;
		for i:=1 to n do tempIndex.idxs[i]:=intArray(a^)[i - 1];
		func_nKind:= true;
	end;

function is_nKind(n:integer;hand:handType;var index:handIndexType):boolean;

{ Return true if "hand" contains "n"-of-a-kind with wildcards. }

begin
	if (hand.n >= n) then begin
		tempHand:=hand;
		tempindex.n:= 0;
		combinations(hand.n, n, @func_nKind);
		index:=tempIndex;
		is_nKind:= (index.n > 0);
		{sortIdx(index,hand);}
	end
	else
		is_nKind:= false;
end;

function is_fourKind(hand:handType;var index:handIndexType):boolean;

begin
	is_fourKind:=is_nKind(4, hand, index);
end;

function is_threeKind(hand:handType;var index:handIndexType):boolean;

begin
	is_threeKind:=is_nKind(3, hand, index);
end;

function find4Kind(var theCards; { array of TCards } const n:word; var a_rResult):boolean;

	{ if "theCards" contains 4-of-a-kind then return in "a_rResult" the
		indexes (0..n-1) of the cards that make up the match }

	var
		h:handtype;
		idx:handindextype;
		i:integer;

	begin
		h.n:= n;
		move(bytePtr(theCards)^, h.card, n);
		if is_fourKind(h, idx) then begin
			for i:= 1 to idx.n do IntegerArray(a_rResult)[i - 1]:= idx.idxs[i];
			find4Kind:= true;
		end
		else
			find4Kind:= false;
	end;

function find3Kind(var theCards; { array of TCards } const n:word; var a_rResult):boolean;

	{ if "theCards" contains 3-of-a-kind then return in "a_rResult" the
		indexes (0..n-1) of the cards that make up the match }

	var
		h:handtype;
		idx:handindextype;
		i:integer;

	begin
		h.n:= n;
		move(bytePtr(theCards)^, h.card, n);
		idx.n:= 0;
		if is_threeKind(h, idx) then begin
			for i:= 1 to idx.n do IntegerArray(a_rResult)[i - 1]:= idx.idxs[i];
			find3Kind:= true;
		end
		else
			find3Kind:= false;
	end;

procedure sortIdx(var idx:handIndexType;hand:handType);

	{ Sort an index "idx" according to the rank of cards in "hand". }

	var

		i,j,k:integer;
		t:shortInt;

	begin

		{ Sort the indexes that make a poker hand by a simple "straight
			selection" sort algorithm. }

		with idx do for i:=1 to n-1 do begin
			k:=i;
			t:=idxs[i];
			for j:=i+1 to n do
				if CardPip(hand.card[idxs[j]]) > CardPip(hand.card[t]) then begin
					k:=j;
					t:=idxs[j];
				end;
			idxs[k]:=idxs[i];
			idxs[i]:=t;
		end;

	end;

function isStraight(hand:handType;var index:handIndexType):boolean;

	var
		i,j:integer;

	begin
		with index do begin
			n:=hand.n;
			for i:=1 to hand.n do idxs[i]:=i;
		end;
		sortIdx(index, hand);

		for i:=2 to hand.n do if
			CardPip(hand.card[index.idxs[i]])
			<>
			CardPip(hand.card[index.idxs[i-1]]) - 1
		then begin
			sortIdx(index,hand);
			for j:=2 to hand.n do if
				CardPip(hand.card[index.idxs[j]])
				<>
				CardPip(hand.card[index.idxs[j-1]]) - 1 then begin
				isStraight:= false;
				exit;
			end;
		end;

		isStraight:=true;
	end;

var
	runInSuit:boolean; { check for a run in the same suit }

function func_run(a:pointer; n:integer):boolean; 

	var
		i:shortInt;
		index:handIndexType;
		h:handtype;

	begin
		index.n:=n;
		h.n:= n;
		for i:=1 to n do begin
			index.idxs[i]:=intArray(a^)[i - 1];
			h.card[i]:= temphand.card[intArray(a^)[i - 1]];
		end;

		if runInSuit then begin
			for i:= 1 to n - 1 do if (CardSuit(h.card[i]) <> CardSuit(h.card[i+1])) then begin
				func_run:= false;
				exit;
			end;
		end;

		if isStraight(h, index) then begin
			tempIndex.n:= n;
			for i:=1 to n do tempIndex.idxs[i]:=intArray(a^)[i - 1];
			func_run:= true;
		end
		else
			func_run:= false;
	end;

function is_runOf(n:integer; hand:handType; var index:handIndexType):boolean;
begin
	if (hand.n >= n) then begin
		tempHand:=hand;
		tempindex.n:= 0;
		combinations(hand.n, n, @func_run);
		index:=tempIndex;
		is_runOf:= (index.n > 0);
		{sortIdx(index,hand);}
	end
	else
		is_runOf:= false;
end;

function findRunOf(const nRun:word; var theCards; { array of TCards } const n:word; var a_rResult; const flags:word):boolean;

	{ if "theCards" contains a run of "nRun" cards then return in "a_rResult" the
		indexes (1..n) of the cards that make up the run }

	var
		h:handtype;
		idx:handindextype;
		i:integer;

	begin
		runInSuit:= ((flags and $0001) <> 0);
		h.n:= n;
		move(bytePtr(theCards)^, h.card, n);
		{sortByte(h.card, n, @sort_func);}
		idx.n:= 0;
		if is_runOf(nRun, h, idx) then begin
			for i:= 1 to idx.n do IntegerArray(a_rResult)[i - 1]:= idx.idxs[i];
			findRunOf:= true;
		end
		else
			findRunOf:= false;
	end;

(*procedure ShuffleDeck;

	{(Deck:PDeck;nPacks,nTimes,Flags:Word);}

	var
		i,j,n:word;
		D:array[1..MaxPacks * TPackSizeMax] of TCard;

	procedure SwapCards(a,b:Word);

		{ Swap the "a"th and "b"th cards in the deck. }

		var
			t:TCard;

		begin
			with TOldDeck(Deck)[1]^ do begin
				t:=D[a];
				D[a]:=D[b];
				D[b]:=t;
			end;
		end;

	begin
		{ assign all the cards in the deck into the temporary array }
		n:=0;
		for i:=1 to nPacks do with TOldDeck(Deck)[i]^ do
			for j:=1 to Size do begin
				Inc(n);
				D[n]:=Cards[j];
			end;
		for i:=1 to nTimes do
			for j:=1 to (n div 2) do
				SwapCards(Random(n)+1,Random(n)+1);
		{ now assign them back to the original deck }
		n:=0;
		for i:=1 to nPacks do with TOldDeck(Deck)[i]^ do begin
			for j:=1 to Size do Cards[j]:=D[n+j];
			Inc(n,Size);
		end;
	end;*)

(*procedure RemovePips;

	{ remove all "tps" pip cards from "Deck". }

	var
		i:integer;

	begin
		for i:=1 to nPacks do TOldDeck(Deck)[i]^.RemovePips(tps);
	end;*)

(*function GetDeckCount;

	var
		i,n:integer;

	begin
		n:=0;
		for i:=1 to nPacks do Inc(n,TOldDeck(Deck)[i]^.Size);
		GetDeckCount:=n;
	end;*)

function DeckPulltopCard(var Deck;nPacks:Word):TCard;

	begin
		deckPulltopCard:= PDeck(deck)^.removetop;
	end;

function DeckPullCard(var Deck;nPacks:Word;aCard:TCard):TCard;

	{ remove the first occurance from the top down of "aCard"
		in the "Deck". The card must be in the deck. }

	var
		j:integer;

	begin
		with PDeck(deck)^ do begin
			j:= size;
			while (get(j) <> aCard) do dec(j);
			DeckPullCard:= remove(j);
		end;
	end;

(*function DeckPullIndex;

	{ remove the "aNum"th card from a "Deck". }

	var
		i,j,k,n:integer;

	begin
		n:=0; { card counter }
		for i:=1 to nPacks do with TOldDeck(Deck)[i]^ do if Size>0 then
			for j:=1 to Size do begin
				Inc(n);
				if n=aNum then begin
					{ got it }
					DeckPullIndex:=Cards[j];
					for k:=j+1 to Size do Cards[k-1]:=Cards[k];
					Dec(Size);
					Exit;
				end;
			end;
		DeckPullIndex:=0;
	end;
*)

function DeckPullPip(var Deck;nPacks:Word;aPip:TPip):TCard;

	{ remove the first occurance from the top down of "aPip"
		in the "Deck". The card must be in the deck. }

	var
		j:integer;

	begin
		with PDeck(deck)^ do begin
			j:= size;
			while (CardPip(get(j)) <> aPip) do dec(j);
			DeckPullPip:=get(j);
			remove(j);
		end;
	end;

function PipCircSucc(aPip:TPip):TPip;

	{ Return the pip value that is the circular successor of aPip }

	{var
		i:word;}

	begin
		if aPip = TKING then
			PipCircSucc:= TACE
		else
			PipCircSucc:= Succ(aPip);
	end;

function nPipCircSucc(aNum:word;aPip:TPip):TPip;

	{ Return the pip value that is "n" pips greater than "aPip"
		'around-the-corner' or circular. }

	var
		i:word;

	begin
		for i:=1 to aNum do aPip:=PipCircSucc(aPip);
		nPipCircSucc:=aPip;
	end;

function PipCircPred(aPip:TPip):TPip;

	{ Return the pip value that is the circular predecessor of "aPip" }

	begin
		if aPip = TACE then
			PipCircPred:= TKING
		else
			PipCircPred:=Pred(aPip);
	end;

function nPipCircPred(aNum:word;aPip:TPip):TPip;

	{ Return the pip value that is "n" pips less than "aPip"
		'around-the-corner' or circular. }

	var
		i:word;

	begin
		for i:=1 to aNum do aPip:=PipCircPred(aPip);
		nPipCircPred:=aPip;
	end;

function DeckPullColor(var Deck:DeckOfCards; nPacks:Word; aPip:TPip; aColor:TSuitColor):TCard;

	{ remove the first "aColor" of "aPip" from the top of the deck }

	var
		j:integer;

	begin
		for j:= deck.size downto 1 do if (CardPip(deck.get(j)) = aPip) and (SuitColor(deck.get(j)) = aColor) then begin
			{ got it }
			DeckPullColor:= deck.remove(j);
			Exit;
		end;
		DeckPullColor:= NULL_CARD;
	end;

function DeckPullRed(var Deck:DeckOfCards; nPacks:Word; aPip:TPip):TCard;

	{ remove the first red "aPip" from the top of the deck }

	begin
		DeckPullRed:= DeckPullColor(Deck, nPacks, aPip, Red_Suit);
	end;

(*
function Text2Pip(const aChar:Char):TPip;

	begin
		case UpCase(aChar) of
			PAceCh..PNinCh:
				Text2Pip:=TPip(Ord(aChar)-Ord(PAceCh));
			PTenCh:
				Text2Pip:=TTEN;
			PJacCh:
				Text2Pip:=TJACK;
			PQueCh:
				Text2Pip:=TQUEEN;
			PKinCh:
				Text2Pip:=TKING;
			else
				Text2Pip:=Low(TPip);
		end;
	end;
*)

function MakeCard(aPip:TPip;Suit:TSuit):TCard;
begin
	MakeCard:=(aPip shl 4) or Suit;
end;

function CardIsPlaceHolder(card:TCard):boolean;
begin
	CardIsPlaceHolder:=(card=NULL_CARD);
end;

procedure flipCard(var c:TCard);
begin
	c:=c xor FACEUP_BIT;
end;

procedure faceupCard(var c:TCard);
begin
	c:=c or FACEUP_BIT;
end;

procedure facedownCard(var c:TCard);
begin
	c:=c and (not FACEUP_BIT);
end;

function Suitcolor(const tc:TCard):TSuitColor;
begin
	case CardSuit(tc) of
		TCLUB,TSPADE:SuitColor:=Black_Suit;
		THEART,TDIAMOND:SuitColor:=Red_Suit;
	end;
end;

procedure PackOfCards.AddCard(tc:TCard);
{ add card "tc" to the top of the pack }
begin
	if Size < TPackSizeMax then begin
		Inc(Size);
		Cards[Size]:=tc;
	end;
end;

constructor PackOfCards.Init(IfJoker:boolean);
var
	aPip:TPip;Suit:TSuit;
begin
	Size:=0;
	for aPip:=TACE to TKING do
		for Suit:=TCLUB to TSPADE do AddCard(MakeCard(aPip,Suit));
	if IfJoker then AddCard(MakeCard(TJOKER,TSPADE));
end;

procedure CardsInit(var aPile:PileOfCards; n:Word);

	{ Initialize a pile of cards that will never have more
		than "n" cards in it. }

	begin
		with aPile do begin
			Limit:= n;
			AllocSize:= SizeOf(Word)+n;
			GetMem(ListPtr,AllocSize);
			with ListPtr^ do Size:= 0;
			m_name:= nil;
		end;
	end;

constructor PileOfCards.Construct(aSize:Word);

	begin
		CardsInit(Self,aSize);
	end;

procedure CardsDone(var aPile:PileOfCards);

	begin
		with aPile do begin
			FreeMem(ListPtr, AllocSize);
			if m_name<>nil then DisposeStr(m_name);
		end;
	end;

destructor PileOfCards.Destruct;

begin
		CardsDone(Self);
end;

procedure CardsAddPacks(var aPile:PileOfCards; WithJoker:boolean);

{(WithJoker:boolean)}

var
	Pip:TPip;Suit:TSuit;

begin
	with aPile do with ListPtr^ do while Size<Limit do begin
		for Pip:=TACE to TKING do
			for Suit:=TCLUB to TSPADE do CardsAdd(aPile,MakeCard(Pip,Suit));
		if WithJoker then CardsAdd(aPile,MakeCard(TJOKER, TSPADE));
	end;
end;

procedure PileOfCards.AddPacks(const WithJoker:boolean);
begin
	CardsAddPacks(Self, WithJoker);
end;

procedure CardsAdd(var aPile:PileOfCards; aCard:TCard); { adds to the top of the pile }
{ card is not added if the set is full }
begin
	with aPile do with ListPtr^ do if (Size <= Limit) then begin
		List[Size]:=aCard;
		Inc(Size);
	end;
end;

procedure PileOfCards.Add(const aCard:TCard);
begin
	CardsAdd(Self, aCard);
end;

function CardsRemovetop(var aPile:PileOfCards):TCard; { removes the "cnum"th card }
begin
	with aPile do with ListPtr^ do if Size>0 then begin
		Dec(Size);
		CardsRemovetop:=List[Size];
	end;
end;

function PileOfCards.Removetop:TCard;
begin
	Removetop:=CardsRemovetop(Self);
end;

procedure PileOfCards.Discardtop;
begin
	if Size > 0 then RemoveTop;
end;

procedure CardsEmpty(var aPile:PileOfCards); { empty the pile }
begin
	aPile.ListPtr^.Size:=0;
end;

procedure PileOfCards.Empty;
begin
	CardsEmpty(Self);
end;

function CardsRemove(var aPile:PileOfCards; cnum:Word):TCard; { removes the "cnum"th card }

var
	k:integer;

begin
	with aPile do with ListPtr^ do if cnum<=Size then begin
		CardsRemove:=List[cnum-1];
		{ move the rest down 1 }
		for k:=cnum to Size-1 do List[k-1]:=List[k];
		Dec(Size);
	end;
end;

function PileOfCards.Remove(const cnum:Word):TCard;

begin
	Remove:=CardsRemove(Self,cnum);
end;

procedure CardsShuffle(var aPile:PileOfCards; nTimes:Word);

	var
		i,j:word;

	procedure SwapCards(a,b:Word);

		{ Swap the "a"th and "b"th cards in the deck. }

		var
			t:TCard;

		begin
			with aPile.ListPtr^ do begin
				t:=List[a];
				List[a]:=List[b];
				List[b]:=t;
			end;
		end;

	begin
		with aPile.ListPtr^ do for i:=1 to nTimes do
			for j:=1 to (Size div 2) do
				SwapCards(Word(Random(Size)), Word(Random(Size)));
	end;

procedure PileOfCards.Shuffle;

	begin
		CardsShuffle(Self, 5);
	end;

function CardsSize(const aPile:PileOfCards):Word;

begin
	CardsSize:= aPile.ListPtr^.Size;
end;

function PileOfCards.Size:word;

begin
	Size:= ListPtr^.Size;
end;

function CardsRef(const aPile:PileOfCards; cnum:Word):PCard; { pointer to "cnum"th card (1..N) }

begin
	CardsRef:=@aPile.ListPtr^.List[cnum-1];
end;

function PileOfCards.Ref(const cnum:Word):PCard;

begin
	Ref:=CardsRef(Self,cnum);
end;

function CardsGet(const aPile:PileOfCards; cnum:Word):TCard; { copy of the "cnum"th card (1..N) }

begin
{	writeln('!CardsGet(Pile=?,', cnum, ')');}
	CardsGet:= aPile.ListPtr^.List[cnum - 1];
end;

function PileOfCards.Get(const cnum:Word):TCard;

begin
	Get:=CardsGet(Self,cnum);
end;

function CardsGettop(aPile:PileOfCards):TCard; { copy of the top card }

begin
	CardsGettop:=CardsGet(aPile,CardsSize(aPile));
end;

function PileOfCards.Gettop:TCard;

begin
	Gettop:=CardsGettop(Self);
end;

procedure CardsSetName(var aPile:PileOfCards; const aName:String);

begin
	with aPile do begin
		DisposeStr(m_name);
		m_name:=NewStr(aName);
	end;
end;

procedure CardsMove(var aSource:PileOfCards; cnum:Word;var aTarget:PileOfCards);
begin
	CardsAdd(aTarget,CardsRemove(aSource,cnum));
end;

procedure CardsMovetop(var aSource:PileOfCards;var aTarget:PileOfCards);
begin
	CardsMove(aSource,CardsSize(aSource),aTarget);
end;

function PileOfCards.CardIsPlaceHolder(aIndex:integer):boolean;
begin
	CardIsPlaceHolder:=cards.CardIsPlaceHolder(Get(aIndex));
end;

function PileOfCards.IsEmpty:boolean;

	function AllPlaceHolders:boolean;

	var
		iCard:integer;

	begin
		AllPlaceHolders:= false;
		for iCard:= 1 to Size do if not CardIsPlaceHolder(iCard) then Exit;
		AllPlaceHolders:= true;
	end;

begin
	IsEmpty:= (Size = 0) or (AllPlaceHolders);
end;

procedure PileOfCards.FlipCard(const cnum:Word);
var
	p:PCard;
begin
	p:= Ref(cnum);
	p^:= p^ xor FACEUP_BIT;
end;

procedure PileOfCards.FlipFacedown(const cnum:Word);
var
	p:PCard;
begin
	p:= Ref(cnum);
	p^:= p^ and (not FACEUP_BIT);
end;

procedure PileOfCards.Fliptop;
begin
	FlipCard(Size);
end;

procedure PileOfCards.FliptopFacedown;
begin
	FlipFacedown(Size);
end;

function PileOfCards.IsFacedown(const cnum:Word):boolean;
begin
	IsFacedown:= ((Get(cnum) and FACEUP_BIT) = 0);
end;

function PileOfCards.IsFaceup(const cnum:Word):boolean;
begin
	IsFaceup:= ((Get(cnum) and FACEUP_BIT) <> 0);
end;

function CardIsFacedown(aCard:TCard):boolean;
begin
	CardIsFacedown:=((aCard and FACEUP_BIT)=0);
end;

procedure DeckOfCards.Add(const aCard:TCard);
begin
	inherited Add(aCard);
	FliptopFacedown;
end;

function PileOfCards.Discard(const iCard:integer):TCard;

begin
	Discard:= Remove(iCard);
end;

function PileOfCards.isNextFacedown:boolean;
 { return true if the next card added to top of this pile should be facedown }
begin
	isNextFacedown:= true; { default }
end;

procedure PileOfCards.settopFacedown;
var
	p:PCard;
begin
	p:= Ref(size);
	p^:= p^ and (not FACEUP_BIT);
end;

procedure PileOfCards.settopFaceup;
var
	p:PCard;
begin
	p:= Ref(size);
	p^:= p^ or FACEUP_BIT;
end;

procedure PileOfCards.addPile(aPile:PPileOfCards);

begin
	while aPile^.size > 0 do add(aPile^.removetop);
end;

procedure PileOfCards.swap(const index1, index2:word);

var
	t:TCard;

begin
	t:= get(index2);
	ref(index2)^:= get(index1);
	ref(index1)^:= t;
end;

procedure PileOfCards.flipAllFaceup;

var
	i:integer;

begin
	for i:= 1 to size do faceupCard(ref(i)^);
end;

procedure PileOfCards.setAllFacedown;

var
	i:integer;

begin
	for i:= 1 to size do facedownCard(ref(i)^);
end;

procedure PileOfCards.snapAllTo(target:PPileOfCards);

var
	i:integer;

begin
	for i:= 1 to size do target^.add(get(i));
	empty;
end;

procedure PileOfCards.moveAllTo(target:PPileOfCards);

begin
	while size > 0 do target^.add(removetop);
end;

procedure PileOfCards.sort;

begin
	sortByte(listptr^.list, size, @sort_func);
end;

function sort_func2(a, b:byte):integer;

{ return -(a<b), 0(a=b), +(a>b) }

begin
	if CardSuit(a) < CardSuit(b) then
		sort_func2:= -1
	else if CardSuit(a) > CardSuit(b) then
		sort_func2:= +1
	else
		sort_func2:= CardPip(a) - CardPip(b);
end;

procedure PileOfCards.sortSuitPip;
begin
	sortByte(listptr^.list, size, @sort_func2);
end;

function PileOfCards.is3kind:boolean;
begin
	is3kind:=
		(size = 3)
		and
		(CardPip(get(1)) = CardPip(get(2)))
		and
		(CardPip(get(2)) = CardPip(get(3)));
end;

function PileOfCards.is4kind:boolean;
begin
	is4kind:=
		(size = 4)
		and
		(CardPip(get(1)) = CardPip(get(2)))
		and
		(CardPip(get(2)) = CardPip(get(3)))
		and
		(CardPip(get(3)) = CardPip(get(4)));
end;

function PileOfCards.isRunInSuit:boolean;
var
	i:integer;
	a:array[0..20] of integer;
begin
	runInSuit:= true;
	temphand.n:= size;
	for i:= 1 to size do begin
		temphand.card[i]:= get(i);
		a[i - 1]:= i;
	end;
	isRunInSuit:= (size > 1) and func_run(@a, size);
end;

procedure PileOfCards.flip;
{ adjust the cards in the pile for a physical flip }
var
	i:integer;
	temp:TCard;
begin
	for i:= 1 to size div 2 do begin
		temp:= listPtr^.list[i - 1];
		listPtr^.list[i - 1]:= listPtr^.list[size - i];
		listPtr^.list[size - i]:= temp;
	end;
	flipAll;
end;

procedure PileOfCards.flipAll;
var
	i:integer;
begin
	for i:= 1 to size do flipCard(i);
end;

function CardPip(aCard:TCard):TPip;
begin
	CardPip:= (aCard shr 4);
end;

function CardSuit(aCard:TCard):TSuit;
begin
	CardSuit:=(aCard and $03);
end;

function CardIsFaceup(aCard:TCard):boolean;
begin
	CardIsFaceup:=((aCard and FACEUP_BIT)<>0);	
end;

function CardFaceup(aCard:TCard):TCard;
begin
	CardFaceup:=(aCard or FACEUP_BIT);	
end;

function CardFacedown(aCard:TCard):TCard;
begin
	facedownCard(aCard);
	CardFacedown:=aCard;
end;

function TPipToPip(t:TPip):cards.pip;
begin
	TPipToPip:=cards.pip(Ord(t)+1);
end;

procedure PackOfCards.RemovePips(tps:TPips);
var
	i,j:integer;
begin
	i:=1;
	while i<=Size do begin
		if (CardPip(Cards[i]) in tps) then begin
			for j:=i+1 to Size do Cards[j-1]:=Cards[j];
			Dec(Size);
		end
		else
			Inc(i);
	end;
	end;

{$ifdef TEXT_CARDS}

procedure WritePip(aPip:TPip);

	begin
		Write(PipChr[aPip]);
	end;

procedure WriteSuit(aSuit:TSuit);

	begin
		Write(SuitGChr[aSuit]);
	end;

procedure WriteCard(aCard:TCard);

	begin
		WritePip(CardPip(aCard));
		WriteSuit(CardSuit(aCard));
		Write(' ');
	end;

procedure WriteCards(aCards:PileOfCards);

	var
		i:integer;

	begin
		with aCards do begin
			if m_name<>nil then system.Write(m_name^);
			system.Write(': ');
			if aCards.IsEmpty then
				system.Write('--' )
			else
				for i:=1 to CardsSize(aCards) do WriteCard(CardsGet(aCards,i));
		end;
	end;

procedure PileOfCards.Write;
begin
	WriteCards(PileOfCards(Self));
end;

{$endif}

(*
function Text2Suit(const aChar:Char):TSuit;

	begin
		case UpCase(aChar) of
			TCluCh:
				Text2Suit:=TCLUB;
			TDiaCh:
				Text2Suit:=TDIAMOND;
			THeaCh:
				Text2Suit:=THEART;
			TSpaCh:
				Text2Suit:=TSPADE;
			else
				Text2Suit:=Low(TSuit);
		end;
	end;

function Text2Card;

	{ converts a text string card specification (AC,Kd,qh...) into
		a "TCard".

		 "aString" is treated case-insensitive }

	begin
		Text2Card:=MakeCard(Text2Pip(aString[1]),Text2Suit(aString[2]));
	end;
*)

function CreateCard(p:pip;s:suit):card;
begin
	CreateCard:=(Ord(s) shl 4) + Ord(p);
end;

function GetCardPip(card:card):pip;
begin
	GetCardPip:=pip((Ord(card) and $0F));
end;

function GetCardSuit(card:card):suit;
begin
	GetCardSuit:=suit(card shr 4);
end;

function TCardToCard(card:TCard):card;
begin
	TCardToCard:=NOCARD;
	if (card<>NULL_CARD) then TCardToCard:=CreateCard(TPipToPip(CardPip(card)),TSuitToSuit(CardSuit(card)));
end;

function TSuitToSuit(t:TSuit):cards.suit;
begin
	TSuitToSuit:=cards.suit(t+1);
end;

function PipToTPip(p:pip):TPip;
begin
	PipToTPip:=Ord(p)-1;
end;

function SuitToTSuit(s:suit):TSuit;
begin
	SuitToTSuit:=Ord(s)-1;
end;

function CardToTCard(card:Card):TCard;
begin
	CardToTCard:=NULL_CARD;
	if (card<>NOCARD) then CardToTCard:=MakeCard(PipToTPip(GetCardPip(card)),SuitToTSuit(GetCardSuit(card)));
end;

end.
