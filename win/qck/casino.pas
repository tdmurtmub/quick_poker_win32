{ (C) 1998 Wesley Steiner }

{$MODE FPC}

{$define RUNTIME_ERR}

unit casino;

{$R casino.res}

interface

uses
	objects,
	cards,
	winqcktbl;

const
	DefaultDecks = 6;
	DefaultPurse = {$ifdef TEST_BROKE} 55.0 {$else} 500.0 {$endif}; { player starts with this many $'s }
	DEFAULTMINBET = 5;
	DefaultMaxBet = 100;
	BJ_MinBetLimit = 100; { maximum min bet allowed }
	BJ_MaxBetFactor = 10; { max bet must be at least this many times the min bet }
	BJ_MaxBetLimit = 2000; { maximum max bet allowed }

const
	MAX_PACKS 			= 6;
	MAX_CARDSPERHAND 	= 10;
	DEFAULT_NUMPACKS 	= 6;
	MAXSEATS 			= 12; { for any game in the casino }
	BJ_HouseCeiling 	= 20000.0; { when someone's pot exceeds this amount
		they are barred from the casino }
	BJ_MAX 				= 21; { max players allowed at a table }
	BJ_MAXSEATS 		= 7; { # of seats at a bj table, must be odd num }
	BJ_MIDSEAT 			= (BJ_MAX div 2 + 1);
	BJ_LARGESTBET 		= 100;

type
	DoubleDownMode= (DDM_11, DDM_10_AND_11, DDM_9_10_11, DDM_ANYTHING);
	DrawTblRange = 4..21;
	SeatIndex = 1..MAXSEATS;

const
	DefaultDDRule = DDM_9_10_11;

type
	TypeOfChip = (Chip1, Chip2, Chip3, Chip4, Chip5, Chip6);

	TAction = (
		ACTION_NONE,
		ACTION_HIT, ACTION_STAND, ACTION_DBLDOWN, ACTION_SPLIT, ACTION_SURRENDER);

	PCasinoChip = ^TCasinoChip;
	TCasinoChip = object(TObject)
		Denomination:TypeOfChip;
		constructor Init(const Denom:TypeOfChip);
		function DollarValue:Real;
	end;

	PBundleOfChips = ^TBundleOfChips;

	{ a pile of chips of differing denominations }

	PPileOfChips = ^TPileOfChips;
	TPileOfChips = object(TCollection)
		constructor Init(TypicalN:integer);
		function DollarValue:real;
		procedure AddAmount(Amt:real);
		procedure RemoveAmount(aAmt:real);
		procedure TransferToBundle(Target:PBundleOfChips);
		procedure double; { double the pile }
		procedure minimize; { minimum chip count }
	end;

	{ a single stack of chips all of the same denomination }

	PStackOfChips = ^TStackOfChips;
	TStackOfChips = object(TCasinoChip)
		Size:integer;
		constructor Init(const Denom:TypeOfChip; n:integer);
		procedure Discard; { discard the chips }
		procedure AddChips(nChips:integer);
		procedure RemoveChips(nChips:integer);
		function DollarValue:Real; { current dollar value }
	end;

	PBlackJackPlayer = ^TBlackJackPlayer;

	PBJBet = ^TBJBet;
	TBJBet = object(TPileOfChips)
		constructor Init(aOwner:PBlackJackPlayer);
		destructor Done; virtual;
		procedure AddChips(toc:TypeOfChip; N:integer);
	private
		_Owner:PBlackJackPlayer;
	end;

	TBundleOfChips = object
		Stacks:array[TypeOfChip] of PStackOfChips;
		constructor Init(DollarAmt:Real);
		destructor Done; virtual;
		function DollarValue:Real; { current dollar value }
		procedure TossIn(Receiver:PBJBet; Denomination:TypeOfChip);
		procedure AddChip(d:TypeOfChip);
		procedure SetAmount(Amt:Real);
		procedure RemoveAmount(aAmt:Real);
		procedure MaximizeChips;
		procedure Discard;
	private
		procedure _setval(const aAmount:Real);
	end;

	PCasinoPatron = ^TCasinoPatron;

	PCasinoSeat = ^TCasinoSeat;
	TCasinoSeat = object
		ThePlayer:PCasinoPatron;
		TheChips:PBundleOfChips;
		constructor Init(aPlayer:PCasinoPatron; const Chips:Real { initial allocation of chips });
		destructor Done; virtual;
	end;

	PCasinoGame = ^TCasinoGame;
	TCasinoGame = object(winqcktbl.Game)
		NumPlayers:Word;
		Seat:array[1..MAXSEATS] of PCasinoSeat;
		MinBet, MaxBet:Word;
		constructor Init(MaxPlayers:Word;tabletop:PTabletop);
		destructor Done; virtual;
		function TakeASeat(const aSeatNum:Word; aPlayer:PCasinoPatron; const Chips:Real):boolean;
		function Sit(const iSeat:Word; aPlayer:PCasinoPatron; const Chips:Real):boolean; virtual;
		procedure unSeat(const iSeat:Word); virtual;
	private
		_MaxPlayers:Word;
	end;

	PCasinoCardGame = ^TCasinoCardGame;

	{ any casino game that needs a card deck of multiple packs with a suffle point can use this object }

	PCasinoDeck = ^TCasinoDeck;
	TCasinoDeck = object(DeckOfCards)
		ShufflePoint:integer; { percentage (20-90) penetration of the current deck to shuffle at }
		ShuffleAt:integer;
		constructor Init(p_owner:PCasinoCardGame);
		procedure setShufflePoint(const aPercent:integer);
		function timeToShuffle:boolean; { true if at or past the shuffle point }
	private
		_owner:PCasinoCardGame;
	end;

	PBlackJackTable = ^TBlackJackTable;

	PPlayerNode = ^TPlayerNode;
	TPlayerNode = object(TObject)
		constructor Init(aNickName:PChar; const Flags:Word);
	end;

	TCasinoPatron = object(TPlayerNode)
		Table:PCasinoGame; { table this patron is seated at }
		SeatNum:integer; { which seat this player is assigned to }
		constructor Init(aNickName:PChar; const aPurse:Real; const Flags:Word);
		function Join(aTable:PCasinoGame; const aSeatNum:Word; const Chips:Real):boolean; virtual;
		procedure leave; virtual;
	end;

	PCasinoEmployee = ^TCasinoEmployee;
	TCasinoEmployee = object(TObject)
	end;

	PCasinoDealer = ^TCasinoDealer;
	TCasinoDealer = object(TCasinoEmployee)
	end;

	PBaccaratDealer = ^TBaccaratDealer;
	TBaccaratDealer = object(TCasinoDealer)
	end;

	PBlackJackDealer = ^TBlackJackDealer;
	TBlackJackDealer = object(TCasinoDealer)
	end;

	TCasinoCardGame=object(TCasinoGame)
		NumDecks:integer;
		Deck:PCasinoDeck; { deck to deal from }
		constructor Init(MaxPlayers:Word;tabletop:PTabletop);
		destructor Done; virtual;
		function PileRows:word; virtual;
		function PileColumns:word; virtual;
		procedure Open(aDealer:PCasinoDealer; BankRoll:Real); virtual;
		procedure SetNumPacks(n:Word); virtual;
		procedure CollectDiscards;
		procedure Close; virtual;
	private
		_Disc:PDeck; { discard pile }
	end;

	PBlackJackHand = ^TBlackJackHand;
	TBlackJackHand = object(THand)
		IsDone:boolean;
		constructor Init(aTable:PBlackJackTable);
		destructor Done; virtual;
		function IsBJ:boolean; virtual;
		function IsPair:boolean;
		function Value:Word;
		function IsHardHand:boolean;
		function IsSixTwo:boolean;
		function Busted:boolean;
		procedure Add(const aCard:TCard); virtual;
		procedure Hit;
		procedure Stand;
		procedure Discard; virtual;
	private
		_table:PBlackJackTable;
	end;

	PBJHouseHand = ^TBJHouseHand;
	TBJHouseHand = object(TBlackJackHand)
	end;

	PBJPlayerHand = ^TBJPlayerHand;
	TBJPlayerHand = object(TBlackJackHand)
		_Owner:PBlackJackPlayer;
		TheBet:PBJBet;
		Surrendered:boolean;
		AllowSplitting:boolean; { splitting is allowed for this hand }
		constructor Init(aPlayer:PBlackJackPlayer { player that owns this hand });
		destructor Done; virtual;
		function Action:TAction;
		function IsBJ:boolean; virtual;
		procedure DDHit; { double down hit }
		procedure Lose;
		procedure PostLose;
		procedure Win;
		procedure PostWin;
		procedure WinBJ;
		procedure PostWinBJ;
		procedure Push;
		procedure PostPush;
		function ShouldSurrender:boolean;
		function ShouldInsure:boolean; { should this hand take insurance }
		function IsSplitHand:boolean;
		function SurrenderLookup:integer;
		function SurrLateLookup:integer;
		function CanDoubleDown:boolean;
	end;

	TBlackJackPlayer = object(TCasinoPatron)
		Hand:PBJPlayerHand;
		SplitHand:PBJPlayerHand;
		BundleOfChips:PBundleOfChips;
		HandsPlayed:LongInt;
		Wins:LongInt;
		Pushes:LongInt;
		BJacks:LongInt; { how many black jacks }
		Pairs:LongInt;
		KnowsCardCount:boolean;
		AtTable:PBlackJackTable;
		constructor Init(aTable:PBlackJackTable);
		destructor Done; virtual;
		function BetAction:Real;
		procedure Bet(Amt:Real);
		procedure Split;
		function Purse:Real;
		function SplitHands:boolean; { originally dealt hand was split }
	end;

	TBlackJackTable = object(TCasinoCardGame)
		Player:array[1..BJ_MAX] of PBlackJackPlayer;
		HouseHand:PBJHouseHand;
		CurrentPlayer:integer;
		NumSeats:Word;
		LowCardsSeen:integer; { 2..6 }
		EvenCardsSeen:integer; { 7,8,9 }
		HighCardsSeen:integer; { 10, J.. A }
		DoubleDownRule:DoubleDownMode;
		DoubleDownSplit:boolean; { allow doubling down after splits }
		DDHardHandsOnly:boolean; { true if player's can only double down a hard hand }
		AllowSurrender:boolean; { surrendering is allowed at this table }
		constructor Init(MaxPlayers:integer;tabletop:PTabletop);
		destructor Done; virtual;
		procedure Open(aDealer:PCasinoDealer; BankRoll:Real); virtual;
		procedure AssignDealer(aDealer:PCasinoDealer);
		procedure AddHumanPlayer(const aSeatNum:integer; aPlayer:PBlackJackPlayer);
		procedure SetNumPacks(n:Word); virtual;
		function QuarterDecksRem:integer; { how many 1/4 decks remain }
		function NumDecksRem:integer; { how many decks remain }
		function TimeToShuffle:boolean; { is it time to shuffle the deck }
		procedure ShuffleDeck;
		function GetShoeCard:TCard;
		function HandsRemaining:Word;
		procedure NextPlayer;
		function ShouldDealerPlay:boolean;
		procedure PreDeal; virtual; { must be called before the deal }
		procedure PostDeal; virtual; { must be called after the deal is finished }
		procedure HandEnd; virtual;
		procedure HandTally;
		function OddsDealerHolds(count:integer):Real;
		function GetCardCount:integer;
		function GetTrueCount:integer;
		function CardsSeen:integer;
		procedure CardCounting(aCard:TCard);
		function EvaluateHand(aHand:PBlackJackHand):integer; { evaluates aHand against the house hand }
		procedure HouseEnd;
		function SingleDeck:boolean;
	end;

const
	BJTblDrawHard:array[DrawTblRange, 2..11] of Byte =
		{ Hard Hand Draw Table, Single or Multiple decks }
		(
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
		(1, 1, 0, 0, 0, 1, 1, 1, 1, 1), { 12 }
		(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
		(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
		(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
		(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
		);

	BJTblDrawSoft:array[13..21, 2..11] of Byte = { Soft Hand Draw Table }
		(
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),	{ 13 }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),	{ 14 }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),	{ 15 }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),	{ 16 }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),	{ 17 }
		(0, 0, 0, 0, 0, 0, 0, 1, 1, 9),	{ 18 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),	{ 19 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),	{ 20 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 	{ 21 }
		);

	BJTblDDHard:array[4..21, 2..11] of Byte = { Hard Hand Double Down Table }
		(
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),	{ 4 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 5 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),	{ 6 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),	{ 7 }
		(0, 0, 0, 2, 2, 0, 0, 0, 0, 0), { 8 }
		(1, 1, 1, 1, 1, 0, 0, 0, 0, 0), { 9 }
		(1, 1, 1, 1, 1, 1, 1, 1, 0, 0), { 10 }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), { 11 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 12 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 13 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 14 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 15 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 16 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 17 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 18 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 19 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), { 20 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) { 21 }
		);

	BJTblDDSoft:array[13..20, 2..11] of Byte = { Soft Hand Double Down Table }
		(
		(0, 0, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 13 }
		(0, 0, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 14 }
		(0, 0, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 15 }
		(0, 0, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 16 }
		(1, 1, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 17 }
		(0, 1, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 18 }
		(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),  { 19 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 	{ 20 }
		);

	BJTblDDSoftMult:array[13..20, 2..11] of Byte = { Soft Hand Double Down Multiple decks }
		(
		(0, 0, 0, 1, 1, 0, 0, 0, 0, 0), 	{ 13 }
		(0, 0, 0, 1, 1, 0, 0, 0, 0, 0), 	{ 14 }
		(0, 0, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 15 }
		(0, 0, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 16 }
		(0, 1, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 17 }
		(0, 1, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 18 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ 19 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)   { 20 }
		);

	BJTblSplit:array[2..11, 2..11] of Byte = { split table }
		(
(*
		(2, 1, 1, 1, 1, 1, 0, 0, 0, 0), 	{ 2,2 }
		(2, 2, 1, 1, 1, 1, 0, 0, 0, 0), 	{ 3,3 }
		(0, 0, 2, 2, 2, 0, 0, 0, 0, 0), 	{ 4,4 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ 5,5 }
		(1, 1, 1, 1, 1, 2, 0, 0, 0, 0), 	{ 6,6 }
		(1, 1, 1, 1, 1, 1, 2, 0, 0, 0), 	{ 7,7 }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 	{ 8,8 }
		(1, 1, 1, 1, 1, 0, 1, 1, 0, 0), 	{ 9,9 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ T,T }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)	{ A,A }
*)
		(1, 1, 1, 1, 1, 1, 0, 0, 0, 0), 	{ 2,2 }
		(1, 1, 1, 1, 1, 1, 0, 0, 0, 0), 	{ 3,3 }
		(0, 0, 1, 1, 1, 0, 0, 0, 0, 0), 	{ 4,4 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ 5,5 }
		(1, 1, 1, 1, 1, 1, 0, 0, 0, 0), 	{ 6,6 }
		(1, 1, 1, 1, 1, 1, 1, 0, 0, 0), 	{ 7,7 }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 	{ 8,8 }
		(1, 1, 1, 1, 1, 0, 1, 1, 0, 0), 	{ 9,9 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ T,T }
		(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)	{ A,A }
		);

	BJTblSurrLate:array[1..5, 2..11] of Byte = { single deck late surrender }
		(
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 	{ 7,7 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 	{ 9,6 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 	{ 10,5 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 	{ 9,7 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 1) 	{ 10,6 }
		);

	BJTblSurrLateMult:array[1..4, 2..11] of Byte = { multiple deck late surrender }
		(
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 	{ 9,6 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 	{ 10,5 }
		(0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 	{ 9,7 }
		(0, 0, 0, 0, 0, 0, 0, 1, 1, 1) 	{ 10,6 }
		);

	BJTblSurrEarlyMult:array[1..14, 2..11] of Byte = { multiple deck late surrender }
		(
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 	{ 5 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 	{ 6 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 	{ 7 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ 8 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ 9 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ 10 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 	{ 11 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 	{ 12 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 	{ 13 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 1), 	{ 14 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 1), 	{ 15 }
		(0, 0, 0, 0, 0, 0, 0, 0, 1, 1), 	{ 8,8 }
		(0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 	{ 16 }
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 1) 	{ 17 }
		);

function SoftPipVal(P:TPip):integer;
function ChipDollarValue(toc:TypeOfChip):Real;
function ChipUnitValue(toc:TypeOfChip):word;

implementation

uses
	{$ifdef TEST} punit, {$endif}
	std;

const
	ChipDenomVal:array[TypeOfChip] of Real = (0.5, 1.0, 5.0, 25.0, 100.0, 500.0);

var
	_Pack:PackOfCards; { all games initialize there decks from this standard pack }

constructor TCasinoGame.Init(MaxPlayers:Word;tabletop:PTabletop);
var
	i:integer;
begin //writeln('TCasinoGame.Init(MaxPlayers:Word)');
	inherited Construct(tabletop);
	_MaxPlayers:= MaxPlayers;
	NumPlayers:= MaxPlayers;
	MinBet:= DefaultMinBet; { $ }
	MaxBet:= DefaultMaxBet; { $ }
	for i:= 1 to MAXSEATS do Seat[i]:= nil;
end;

destructor TCasinoGame.Done;
var
	i:integer;
begin
	for i:= 1 to MAXSEATS do if (Seat[i] <> nil) then
		Dispose(Seat[i], Done); { added 990410 }
end;

constructor TCasinoCardGame.Init(MaxPlayers:Word;tabletop:PTabletop);
var
	i:integer;
begin
	inherited Init(MaxPlayers,tabletop);
	Deck:=nil;
	_Disc:=New(PDeck,Construct(Max_Packs*52));
end;

destructor TCasinoCardGame.Done;
var
	i:integer;
begin
	Dispose(_Disc, Destruct);
end;

procedure TCasinoCardGame.SetNumPacks(n:Word);

var
	i, j:integer;

begin
	n:=Max(Min(Integer(n), Integer(MaxPacks)), 1);
	Deck^.Empty;
	_Disc^.Empty;
	for i:= 1 to n do for j:=1 to 52 do Deck^.Add(_Pack.Cards[j]);
	NumDecks:= n;
	Deck^.SetShufflePoint(Deck^.ShufflePoint);
	{ShuffleDeck;}
end;

procedure TBlackJackTable.SetNumPacks(n:Word);

begin
	inherited SetNumPacks(n);

	{ adjust the strategy tables appropriately }

	BJTblDrawSoft[18, 11]:= QInteger(SingleDeck, 1, 0);

	BJTblDDHard[11, 11]:= QInteger(SingleDeck, 1, 0);
	BJTblDDHard[9, 2]:= QInteger(SingleDeck, 1, 0);
	BJTblDDHard[8, 6]:= QInteger(SingleDeck, 2, 0);
	BJTblDDHard[8, 5]:= QInteger(SingleDeck, 2, 0);

	BJTblSplit[7, 8]:= QInteger(SingleDeck, 2, 0);
	BJTblSplit[6, 7]:= QInteger(SingleDeck, 2, 0);
	BJTblSplit[6, 2]:= QInteger(SingleDeck, 1, 2);
	BJTblSplit[4, 4]:= QInteger(SingleDeck, 2, 0);
	BJTblSplit[2, 3]:= QInteger(SingleDeck, 1, 2);
end;

procedure TBlackJackTable.HouseEnd;
begin //writeln('TBlackJackTable.HouseEnd');
	with HouseHand^ do
		while (Size > 0) do _Disc^.Add(Removetop);
end;

procedure TBlackJackTable.HandEnd;
var
	i:integer;
begin //writeln('TBlackJackTable.HandEnd');
	HouseEnd;
	for i:= 1 to NumPlayers do
		if (Player[i] <> nil) then begin
			Player[i]^.Hand^.Discard;
			Player[i]^.SplitHand^.Discard;
		end;
end;

constructor TBlackJackTable.Init(MaxPlayers:integer;tabletop:PTabletop);
var
	i:integer;
begin
	inherited Init(MaxPlayers,tabletop);
	DoubleDownRule:= DefaultDDRule;
	DoubleDownSplit:= True;
	DDHardHandsOnly:= False;
	AllowSurrender:= True;
	LowCardsSeen:= 0;
	EvenCardsSeen:= 0;
	HighCardsSeen:= 0;
	NumSeats:= 7;
	HouseHand:= nil;
	for i:= 1 to NumPlayers do Player[i]:= nil; { no one at the table yet }
end;

destructor TBlackJackTable.Done;
var
	i:integer;
begin
	for i:= 1 to NumPlayers do begin
		if (Player[i] <> nil) then Dispose(Player[i], Done);
	end;
	if (HouseHand <> nil) then Dispose(HouseHand, Done);
end;

procedure TCasinoCardGame.Open(aDealer:PCasinoDealer; BankRoll:Real);
{ Open this table.

		The dealer brings his pack of cards and bank roll with him to the
		table. }

	begin
		Deck:= New(PCasinoDeck, Init(@Self));
		SetNumPacks(DEFAULT_NUMPACKS);
	end;

procedure TCasinoCardGame.Close;

	var
		i:integer;

	begin
		for i:= 1 to MAXSEATS do if (Seat[i] <> nil) then
			Dispose(Seat[i], Done);
		if (Deck <> nil) then Dispose(Deck, Destruct);
	end;

procedure TBlackJackTable.Open(aDealer:PCasinoDealer; BankRoll:Real);

	{ Open this table by assigning a dealer to this table.

		The dealer brings his pack of cards and chip bank with him to the table. }

	begin
		inherited Open(aDealer, BankRoll);
		HouseHand:= New(PBJHouseHand, Init(@Self));
	end;

constructor TPlayerNode.Init(aNickName:PChar; const Flags:Word);
begin
	inherited Init;
end;

constructor TCasinoPatron.Init(aNickName:PChar; const aPurse:Real; const Flags:Word);

	begin
		inherited Init(aNickName, Flags);
		Table:= nil;
	end;

constructor TBlackJackPlayer.Init(aTable:PBlackJackTable);

begin
	AtTable:= aTable;
	Hand:= New(PBJPlayerHand, Init(@Self));
	SplitHand:= New(PBJPlayerHand, Init(@Self));
	BundleOfChips:= New(PBundleOfChips, Init(1000));
	HandsPlayed:= 0;
	Wins:= 0;
	Pushes:= 0;
	BJacks:= 0;
	Pairs:= 0;
	KnowsCardCount:= TRUE;
end;

destructor TBlackJackPlayer.Done;
begin
	Dispose(BundleOfChips, Done);
	Dispose(SplitHand, Done);
	Dispose(Hand, Done);
end;

function SoftPipVal(P:TPip):integer;
begin
	case P of
		TDeuce..TTen:
			SoftPipVal:= P + 1;
		TAce:
			SoftPipVal:= 11;
		else
			SoftPipVal:= 10;
	end;
end;

var
	_hardval:boolean;

function TBlackJackHand.Value:Word;

	{	Return the usable value of a BlackJack hand.
		Uses a recursive algorithm. }

	function ValueOf(T, i:integer):integer;

		var
			B1, B2, M:integer;

		begin
			if (CardPip(Get(i)) = TACE) then begin
				_hardval:= False;
				if (i < size) then
					M:= ValueOf(T + 11, i + 1)
				else
					M:= T + 11;
				if (M > 21) then begin
					if (i < size) then
						ValueOf:= ValueOf(T + 1, i + 1)
					else
						ValueOf:= T + 1;
					_hardval:= True;
				end
				else
					ValueOf:=M;
			end
			else if (i < Size) then
				ValueOf:= ValueOf(T + SoftPipVal(CardPip(Get(i))), i + 1)
			else
				ValueOf:= T + SoftPipVal(CardPip(Get(i)));
		end;

	begin
		_hardval:= True;
		if Size > 0 then
			Value:= ValueOf(0, 1)
		else
			Value:= 0;
	end;

function TBlackJackHand.IsBJ:boolean;

	begin
		IsBJ:= (Size = 2) and (Value = 21);
	end;

function TBJPlayerHand.IsBJ:boolean;

	begin
		IsBJ:= (not IsSplitHand) and (inherited IsBJ);
	end;

function TBlackJackHand.IsHardHand:boolean;

	var
		i:integer;

	begin
		Value;
		IsHardHand:= _hardval;
		{for i:= 1 to Size do if (CardPip(Get(i)) = TACE) then begin
			IsHardHand:= False;
			Exit;
		end;
		IsHardHand:= True;}
	end;

function TBlackJackHand.IsPair:boolean;

	begin
		IsPair:=
			(
				(Size = 2)
				and
				( SoftPipVal(CardPip(get(1))) = SoftPipVal(CardPip(get(2))) )
			);
	end;

function TBlackJackHand.IsSixTwo:boolean;

	{ return true if hand is composed of only a 6 and a 2 }

	begin
		IsSixTwo:=
			(Size = 2)
			and
			(
				((CardPip(Get(1)) = TSIX) and (CardPip(Get(2)) = TDEUCE))
				or
				((CardPip(Get(2)) = TSIX) and (CardPip(Get(1)) = TDEUCE))
			);
	end;

function TBlackJackHand.Busted:boolean;

	begin
		Busted:= (Value > 21);
	end;

function TBlackJackTable.TimeToShuffle:boolean;

	begin
		TimeToShuffle:= (Deck^.Size <= Deck^.ShuffleAt);
	end;

function TBlackJackTable.GetShoeCard:TCard;

	begin
		{$ifdef RUNTIME_ERR}
		if TimeToShuffle then RunError(1001);
		{$endif}
		GetShoeCard:= Deck^.Removetop;
	end;

procedure TBJPlayerHand.DDHit;

	begin
		with _owner^ do begin
			BundleOfChips^.RemoveAmount(TheBet^.DollarValue);
			TheBet^.AddAmount(TheBet^.DollarValue);
		end;
		Hit;
		IsDone:=True;
	end;

procedure TBlackJackHand.Stand;

	begin
		IsDone:=True;
	end;

procedure TBlackJackTable.NextPlayer;

	begin
		Inc(CurrentPlayer);
		if CurrentPlayer > NumPlayers then CurrentPlayer:= 1;
	end;

function TBlackJackTable.ShouldDealerPlay:boolean;

	{ return true if the dealer shouldplay out his hand }

	var
		i:integer;

	begin
		ShouldDealerPlay:= False;
		for i:= 1 to NumPlayers do if (Player[i] <> nil) then with Player[i]^.Hand^ do begin
			if (not Surrendered) and (not Busted) and (not IsBJ) then begin
				ShouldDealerPlay:= True;
				exit;
			end;
		end;
	end;

const
	DDAfterSplit:boolean = False;

function TBJPlayerHand.SurrenderLookup:integer;

	function IsPairOf(v1, v2:integer):boolean;

	begin
		IsPairOf:=
			((SoftPipVal(CardPip(Get(1))) = v1) and (SoftPipVal(CardPip(Get(2))) = v2))
			or
			((SoftPipVal(CardPip(Get(1))) = v2) and (SoftPipVal(CardPip(Get(2))) = v1));
	end;

	begin
		if (IsPairOf(10, 6)) then
			SurrenderLookup:= 5
		else if (IsPairOf(9, 7)) then
			SurrenderLookup:= 4
		else if (IsPairOf(10, 5)) then
			SurrenderLookup:= 3
		else if (IsPairOf(9, 6)) then
			SurrenderLookup:= 2
		else if (IsPairOf(7, 7)) then
			SurrenderLookup:= 1
		else
			SurrenderLookup:= 0;
	end;

function TBJPlayerHand.ShouldSurrender:boolean;

	{ return true if this hand should sutrrender according to the tables }

	begin
		if _table^.SingleDeck then
			ShouldSurrender:=
				(SurrenderLookup > 0)
				and
				(BJTblSurrLate[SurrenderLookup, SoftPipVal(CardPip(_Table^.HouseHand^.Gettop))] <> 0)
		else
			ShouldSurrender:=
				(SurrLateLookup > 0)
				and
				(BJTblSurrLateMult[SurrLateLookup, SoftPipVal(CardPip(_Table^.HouseHand^.Gettop))] <> 0);
	end;

function TBJPlayerHand.Action:TAction;

	begin
		if Value = 21 then begin
			Action:= ACTION_STAND;
			Exit;
		end;

		if (not Busted) then begin

			{ surrender }

			if
				(_table^.AllowSurrender)
				and
				(Size = 2)
				and
				(not IsSplitHand)
				and
				(ShouldSurrender)
			then begin
				Action:= ACTION_SURRENDER;
				Exit;
			end;

			{ Split Strategy }

			if
				(
				AllowSplitting
				and
				IsPair
				)
			then begin case BJTblSplit
					[SoftPipVal(CardPip(Gettop)),
					SoftPipVal(CardPip(_Table^.HouseHand^.Gettop))] of
					1:begin
						Action:= ACTION_SPLIT;
						Exit;
					end;
					2:begin
						if DDAfterSplit then begin
							Action:= ACTION_SPLIT;
							Exit;
						end;
					end;
				end;
			end;

			{ Double Down Strategy }

			if CanDoubleDown then begin
				if IsHardHand then
					case BJTblDDHard[Value, SoftPipVal(CardPip(_Table^.HouseHand^.Gettop))] of
						2:
							if (not IsSixTwo) then begin { except on a 6,2 }
								Action:= ACTION_DBLDOWN;
								Exit;
							end;
						1:begin
							Action:= ACTION_DBLDOWN;
							Exit;
						end;
					end { case }
				else begin
					if _table^.SingleDeck then
						case BJTblDDSoft[Value, SoftPipVal(CardPip(_Table^.HouseHand^.Gettop))] of
							1:begin
								Action:= ACTION_DBLDOWN;
								Exit;
							end;
						end
					else
						case BJTblDDSoftMult[Value, SoftPipVal(CardPip(_Table^.HouseHand^.Gettop))] of
							1:begin
								Action:= ACTION_DBLDOWN;
								Exit;
							end;
						end;
				end;
			end;

			{ <<< hit/stand strategy >>> }

			if IsHardHand then
				case BJTblDrawHard[Value, SoftPipVal(CardPip(_table^.HouseHand^.Gettop))] of
					1:
						Action:= ACTION_HIT;
					0:
						Action:= ACTION_STAND;
				end
			else
				case BJTblDrawSoft[Value, SoftPipVal(CardPip(_Table^.HouseHand^.Gettop))] of
					1:
						Action:= ACTION_HIT;
					0:
						Action:= ACTION_STAND;
				end
		end

		else begin
			Action:= ACTION_NONE;
			IsDone:=True;
		end;
	end;

procedure TBlackJackTable.PreDeal;

	begin
	end;

procedure TBlackJackTable.PostDeal;

	var
		i:integer;

	begin
		for i:= 1 to NumPlayers do if (Player[i] <> nil) then with Player[i]^.Hand^ do begin
			IsDone:= False;
			AllowSplitting:= TRUE;
			surrendered:= False;
		end;
		CurrentPlayer:= 1;
	end;

function CompareHands(H1:PBlackJackHand; H2:PBlackJackHand):integer;

	{	-1 H1 Loses to H2
		 0 push
		+1 H1 Wins over H2 }

	var
		V1, V2:Word;

	begin
		{writeln('comparing..');}
		if H1^.Busted then
			CompareHands:= -1
		else if H2^.Busted then
			CompareHands:= 1
		else if H1^.IsBJ and H2^.IsBJ then
			CompareHands:= 0
		else if H1^.IsBJ and (not H2^.IsBJ) then
			CompareHands:= 2
		else if H2^.IsBJ and (not H1^.IsBJ) then
			CompareHands:= -1
		else begin
			V1:= H1^.Value;
			V2:= H2^.Value;
			if V1 < V2 then
				CompareHands:= -1
			else if V1 = V2 then
				CompareHands:= 0
			else
				CompareHands:= 1;
		end;
	end;

function TCasinoChip.DollarValue:Real;

	begin
		DollarValue:= ChipDenomVal[Denomination];
	end;

constructor TBundleOfChips.Init(DollarAmt:Real);

	var
		toc:TypeOfChip;

	begin
		for toc:= High(TypeOfChip) downto Low(TypeOfChip) do Stacks[toc]:= New(PStackOfChips, Init(toc, 0));
		_setval(DollarAmt);
	end;

destructor TBundleOfChips.Done;

	var
		d:TypeOfChip;

	begin
		for d:= High(TypeOfChip) downto Low(TypeOfChip) do Dispose(Stacks[d], Done);
	end;

constructor TCasinoChip.Init(const Denom:TypeOfChip);

	begin
		inherited Init;
		Denomination:= Denom;
	end;

function TBlackJackTable.OddsDealerHolds(count:integer):Real;

	{ return the odds that the dealer holds a hand of "count" }

	var
		i, ncounts:integer;
		h:TBlackJackHand;

	begin
		OddsDealerHolds:= 0.0;
		h.Init(nil);
		ncounts:= 0;
		h.Add(HouseHand^.Gettop);
		with Deck^ do if (Size > 0) then begin
			for i:= 1 to Size do begin
				h.Add(Get(i));
				if (h.Value = count) then Inc(ncounts);
				h.Removetop;
			end;
			OddsDealerHolds:= ncounts / Size;
		end;
	end;

procedure TBlackJackTable.CardCounting(aCard:TCard);

	begin
		case SoftPipVal(CardPip(aCard)) of
			2..6:Inc(LowCardsSeen);
			10, 11:Inc(HighCardsSeen);
			else Inc(EvenCardsSeen);
		end;
	end;

{function TBJDeck.Removetop:TCard;

	begin
		CardCounting(Gettop);
		Removetop:= inherited Removetop;
	end;}

function TBlackJackTable.GetCardCount:integer;

	begin
		GetCardCount:= LowCardsSeen - HighCardsSeen;
	end;

function TBlackJackTable.QuarterDecksRem:integer;

	begin
		QuarterDecksRem:= (Deck^.Size + 7) div 13;
	end;

function TBlackJackTable.NumDecksRem:integer;

	begin
		NumDecksRem:= (Deck^.Size + 26) div 52;
	end;

function TBlackJackTable.GetTrueCount:integer;

	begin
		GetTrueCount:= Round(GetCardCount * 52.0 / Deck^.Size);
	end;

constructor TBlackJackHand.Init(aTable:PBlackJackTable);

	begin
		inherited Construct(MAX_CARDSPERHAND);
		_table:= aTable;
	end;

destructor TBlackJackHand.Done;

	begin
		inherited Destruct;
	end;

constructor TBJPlayerHand.Init(aPlayer:PBlackJackPlayer);

	begin
		inherited Init(aPlayer^.AtTable);
		_owner:= aPlayer;
		{AllowSplitting:= TRUE;}
		{TheBet:= New(PBJBet, Init(aPlayer));}
		TheBet:= New(PBJBet, Init(@Self));
	end;

destructor TBJPlayerHand.Done;

	begin
		Dispose(TheBet, Done);
		inherited Done;
	end;

procedure TBlackJackPlayer.Split;

	begin
		SplitHand^.Add(Hand^.Removetop);
		Hand^.Hit;
		SplitHand^.Hit;
		SplitHand^.TheBet^.AddAmount(Hand^.TheBet^.DollarValue);
		BundleOfChips^.RemoveAmount(Hand^.TheBet^.DollarValue);
	end;

function TBundleOfChips.DollarValue:Real;

	{ current dollar value }

	var
		Total:Real;
		d:TypeOfChip;

	begin
		Total:= 0.0;
		for d:= High(TypeOfChip) downto Low(TypeOfChip) do
			Total:=Total + Stacks[d]^.DollarValue;
		DollarValue:= Total;
	end;

constructor TStackOfChips.Init(const Denom:TypeOfChip; n:integer);

	begin
		inherited Init(Denom);
		Size:= n;
	end;

procedure TBundleOfChips.TossIn(Receiver:PBJBet; Denomination:TypeOfChip);

	begin
		Dec(Stacks[Denomination]^.Size);
		Receiver^.Insert(New(PCasinoChip, Init(Denomination)));
	end;

function TStackOfChips.DollarValue:Real;

	begin
		DollarValue:= Size * inherited DollarValue;
	end;

procedure TBlackJackTable.HandTally;

	var
		i:integer;

	begin
		CardCounting(HouseHand^.Get(1));
		for i:= 1 to NumPlayers do if (Player[i] <> nil) then with Player[i]^ do begin
			Inc(HandsPlayed);
			case CompareHands(Player[i]^.Hand, HouseHand) of
				-1:
					Player[i]^.Hand^.Lose;
				0:
					Player[i]^.Hand^.Push;
				1:
					Player[i]^.Hand^.Win;
				2:
					Player[i]^.Hand^.WinBJ;
			end;
			if SplitHand^.Size > 0 then begin
				Inc(HandsPlayed);
				case CompareHands(Player[i]^.SplitHand, HouseHand) of
					-1:
						Player[i]^.SplitHand^.Lose;
					0:
						Player[i]^.SplitHand^.Push;
					1:
						Player[i]^.SplitHand^.Win;
					2:
						Player[i]^.SplitHand^.WinBJ;
				end;
			end;
		end;
	end;

function BiggestDenom(var A:Real):TypeOfChip;

	{ return the largest denomination chip that ... }

	var
		d:TypeOfChip;

	begin
		for d:= High(TypeOfChip) downto Low(TypeOfChip) do begin
			if (A >= ChipDenomVal[d]) then begin
				A:= A - ChipDenomVal[d];
				BiggestDenom:= d;
				Exit;
			end;
		end;
	end;

function TPileOfChips.DollarValue:Real;

	var
		t:Real;

	procedure AddEmUp(Item:Pointer);

		begin
			t:= t + PCasinoChip(Item)^.DollarValue;
		end;

	begin
		t:= 0.0;
		ForEach(@AddEmUp);
		DollarValue:= t;
	end;

procedure TBlackJackPlayer.Bet(Amt:Real);

	var
		A:Real;

	begin
		A:= Amt;
		while Hand^.TheBet^.DollarValue < Amt do BundleOfChips^.TossIn(Hand^.TheBet, BiggestDenom(A));
	end;

procedure TBJPlayerHand.PostLose;

	begin
	end;

procedure TBJPlayerHand.Lose;

	begin
		TheBet^.FreeAll;
	end;

procedure TPileOfChips.AddAmount(Amt:Real);
var
	A:Real;
	d:TypeOfChip;
begin //writeln('TPileOfChips.AddAmount(',Amt,')');
	A:= Amt;
	while A > 0.0 do begin
		d:= BiggestDenom(A);
		Insert(New(PCasinoChip, Init(d)));
	end;
end;

procedure TBundleOfChips.AddChip(d:TypeOfChip);

	begin
		Inc(Stacks[d]^.Size);
	end;

procedure TBJPlayerHand.PostWin;

	begin
		Inc(_owner^.Wins);
	end;

procedure TBJPlayerHand.Win;

	procedure PayChip(Item:Pointer);

		begin
			_owner^.BundleOfChips^.AddChip(PCasinoChip(Item)^.Denomination);
		end;

	begin
		with TheBet^ do begin
			AddAmount(DollarValue);
			ForEach(@PayChip);
			FreeAll;
		end;
		PostWin;
	end;

procedure TBJPlayerHand.PostPush;

	begin
		Inc(_owner^.Pushes);
	end;

procedure TBJPlayerHand.Push;

	procedure PayChip(Item:Pointer);

		begin
			_owner^.BundleOfChips^.AddChip(PCasinoChip(Item)^.Denomination);
		end;

	begin
		with TheBet^ do begin
			ForEach(@PayChip);
			FreeAll;
		end;
		PostPush;
	end;

procedure TBJPlayerHand.PostWinBJ;

	begin
		PostWin
	end;

procedure TBJPlayerHand.WinBJ;

	procedure PayChip(Item:Pointer);

		begin
			_owner^.BundleOfChips^.AddChip(PCasinoChip(Item)^.Denomination);
		end;

	begin
		with TheBet^ do begin
			AddAmount(DollarValue * 3 / 2);
			ForEach(@PayChip);
			FreeAll;
		end;
		PostWinBJ;
	end;

procedure TBundleOfChips.RemoveAmount(aAmt:Real);

begin
	{$ifdef DEBUG}
	if aAmt > DollarValue then begin
		WriteLn('TBundleOfChips.RemoveAmount(',aAmt,'): DollarValue=',DollarValue);
		RunError(201);
	end;
	{$endif}
	_setval(DollarValue - aAmt);
end;

constructor TBJBet.Init(aOwner:PBlackJackPlayer);
begin
	inherited Init(10);
	_owner:= aOwner;
end;

destructor TBJBet.Done;
begin
	inherited Done;
end;

constructor TPileOfChips.Init(TypicalN:integer);
begin
	inherited Init(TypicalN, TypicalN div 10);
end;

function TBlackJackPlayer.BetAction:Real;

	begin
		with AtTable^ do if (not KnowsCardCount) then
			BetAction:= MinBet
		else if (GetTrueCount <= 0) then
			BetAction:= MinBet
		else
			BetAction:= MinBet * (GetTrueCount + 1);
	end;

function ChipDollarValue(toc:TypeOfChip):Real;

begin
	ChipDollarValue:= ChipDenomVal[toc];
end;

procedure TBJBet.AddChips(toc:TypeOfChip; N:integer);
var
	i:integer;

begin
	for i:= 1 to N do Insert(New(PCasinoChip, Init(toc)));
end;

function TBlackJackTable.EvaluateHand(aHand:PBlackJackHand):integer;
{ evaluates aHand against the house hand }
begin
	EvaluateHand:= CompareHands(aHand, HouseHand);
end;

function TBlackJackTable.HandsRemaining:Word;

{ return then # of hands remaining at the table }

var
	i, n:integer;

begin
	n:= 0;
	for i:= 1 to NumSeats do if (Player[i] <> nil) then with Player[i]^ do begin
		with Hand^ do if (Size > 0) and (not Busted) {and (not IsBJ)} then Inc(n);
		with SplitHand^ do if (Size > 0) and (not Busted) {and (not IsBJ)} then Inc(n);
	end;
	HandsRemaining:= n;
end;

procedure TPileOfChips.RemoveAmount(aAmt:Real);

{ add/remove chips to egual an amount }

var
	d:TypeOfChip;
	n:integer;
	i:integer;
	a:Real;

begin
	if aAmt > DollarValue then exit;

	a:= DollarValue - aAmt;
	DeleteAll;

	{ Make change for "DollarAmt" by maximizing each denomination }

	for d:= High(TypeOfChip) downto Low(TypeOfChip) do begin
		n:= Trunc(a / ChipDenomVal[d]); { how many of this denomination }
		for i:= 1 to Abs(n) do Insert(New(PCasinoChip, Init(d)));
		a:= a - n * ChipDenomVal[d];
	end;
end;

procedure TBlackJackHand.Discard;
begin
	while (Size > 0) do _table^._Disc^.Add(Removetop);
end;

function TBJPlayerHand.CanDoubleDown:boolean;

function LowDD:integer;

	begin
		case _table^.DoubleDownRule of
			DDM_11:LowDD:= 11;
			DDM_10_AND_11:LowDD:= 10;
			DDM_9_10_11:LowDD:= 9;
			DDM_ANYTHING:LowDD:= 4;
		end;
	end;

function HighDD:integer;

	begin
		case _table^.DoubleDownRule of
			DDM_11:HighDD:= 11;
			DDM_10_AND_11:HighDD:= 11;
			DDM_9_10_11:HighDD:= 11;
			DDM_ANYTHING:HighDD:= 20;
		end;
	end;

begin
	CanDoubleDown:=
		(Size = 2)
		and
		((not _Owner^.SplitHands) or (_Owner^.SplitHands and _Owner^.AtTable^.DoubleDownSplit))
		and
		((not _table^.DDHardHandsOnly) or (_table^.DDHardHandsOnly and IsHardHand))
		and
		(Value >= LowDD)
		and
		(Value <= HighDD);
end;

function TBlackJackPlayer.Purse:Real;

begin
	Purse:= BundleOfChips^.DollarValue;
end;

procedure TBlackJackHand.Add(const aCard:TCard);

begin
	inherited Add(aCard);
	IsDone:= (Value = 21);
end;

procedure TBlackJackHand.Hit;

begin
	with _Table^ do begin
		if TimeToShuffle then begin
			CollectDiscards;
			ShuffleDeck;
		end;
		Add(GetShoeCard);
	end;
end;

procedure TBlackJackTable.ShuffleDeck;
begin
	Deck^.Shuffle;
	LowCardsSeen:= 0;
	EvenCardsSeen:= 0;
	HighCardsSeen:= 0;
end;

function TBlackJackTable.SingleDeck:boolean;
begin
	SingleDeck:= (NumDecks < 4);
end;

function TBJPlayerHand.ShouldInsure:boolean;
{ should this hand take insurance }
begin
	with _Owner^.AtTable^ do if SingleDeck then
		ShouldInsure:=
			((CardsSeen <= 13) and (GetTrueCount >= 2))
			or
			((CardsSeen > 13) and (GetTrueCount >= 1))
	else
		ShouldInsure:= (GetTrueCount >= 3);
end;

function TBJPlayerHand.IsSplitHand:boolean;

	begin
		IsSplitHand:= (@self = _owner^.SplitHand); {(not AllowSplitting);}
	end;

function TBlackJackTable.CardsSeen:integer;

	begin
		CardsSeen:= LowCardsSeen + EvenCardsSeen + HighCardsSeen;
	end;

procedure TStackOfChips.Discard;

	begin
		Size:= 0;
	end;

procedure TStackOfChips.AddChips(nChips:integer);

	begin
		Inc(Size, nChips);
	end;

procedure TStackOfChips.RemoveChips(nChips:integer);

	begin
		Dec(Size, nChips);
		{$ifdef DEBUG}
		if Size < 0 then begin
			WriteLn('Runtime Error in TStackOfChips.RemoveChips(',nChips,'): Size=',Size);
			RunError(201);
		end;
		{$endif}
	end;

procedure TBundleOfChips._setval(const aAmount:Real);

	var
		total:integer;
		toc:TypeOfChip;
		n, rem:integer;

	begin
		{ first the 50 cent pieces }

		Stacks[Chip1]^.Size:= 0;
		n:= Round(aAmount / ChipDollarValue(Chip1)) mod 2;
		Stacks[Chip1]^.Size:= n;

		total:= Round(aAmount - n * ChipDollarValue(Chip1));

		for toc:= Chip2 to High(TypeOfChip) do Stacks[toc]^.Size:= 0;
		for toc:= Chip2 to High(TypeOfChip) do begin
			if total = 0 then break;
			if (toc = High(TypeOfChip)) then
				n:= total div Round(ChipDollarValue(toc))
			else begin
				rem:= total - Round(ChipDollarValue(Succ(toc))) * (total div Round(ChipDollarValue(Succ(toc))));
				if rem = 0 then
					n:= Round(ChipDollarValue(Succ(toc))) div Round(ChipDollarValue(toc))
				else
					n:= rem div Round(ChipDollarValue(toc))
			end;
			Stacks[toc]^.Size:= n;
			total:= total - n * Round(ChipDollarValue(toc));
		end;
	end;

procedure TBundleOfChips.MaximizeChips;

begin
	_setval(DollarValue);
end;

function TBJPlayerHand.SurrLateLookup:integer;

	function IsPairOf(v1, v2:integer):boolean;

		begin
			IsPairOf:=
				((SoftPipVal(CardPip(Get(1))) = v1) and (SoftPipVal(CardPip(Get(2))) = v2))
				or
				((SoftPipVal(CardPip(Get(1))) = v2) and (SoftPipVal(CardPip(Get(2))) = v1));
		end;

	begin
		if (IsPairOf(10, 6)) then
			SurrLateLookup:= 4
		else if (IsPairOf(9, 7)) then
			SurrLateLookup:= 3
		else if (IsPairOf(10, 5)) then
			SurrLateLookup:= 2
		else if (IsPairOf(9, 6)) then
			SurrLateLookup:= 1
		else
			SurrLateLookup:= 0;
	end;

procedure TPileOfChips.TransferToBundle(Target:PBundleOfChips);

begin
	while Count > 0 do begin
		Target^.Stacks[PCasinoChip(At(0))^.Denomination]^.AddChips(1);
		AtFree(0);
	end;
end;

procedure TBlackJackTable.AddHumanPlayer(const aSeatNum:integer; aPlayer:PBlackJackPlayer);

begin
	Player[aSeatNum]:= aPlayer;
end;

procedure TBundleOfChips.Discard;

var
	toc:TypeOfChip;

begin
	for toc:= High(TypeOfChip) downto Low(TypeOfChip) do Stacks[toc]^.Discard;
end;

procedure TBundleOfChips.SetAmount(Amt:Real);

begin
	_setval(Amt);
end;

procedure TCasinoCardGame.CollectDiscards;
begin
	while _Disc^.Size > 0 do begin
		Deck^.Add(_Disc^.Removetop);
	end;
end;

function TBlackJackPlayer.SplitHands:boolean;

	begin
		SplitHands:= (SplitHand^.Size > 0);
	end;

procedure TBlackJackTable.AssignDealer(aDealer:PCasinoDealer);

	begin
		Open(aDealer, 100.0);
	end;

constructor TCasinoSeat.Init(aPlayer:PCasinoPatron; const Chips:Real);

	begin
		ThePlayer:= aPlayer;
		TheChips:= New(PBundleOfChips, Init(Chips));
	end;

destructor TCasinoSeat.Done;

	begin
		Dispose(TheChips, Done);
	end;

function TCasinoGame.TakeASeat(const aSeatNum:Word; aPlayer:PCasinoPatron; const Chips:real):boolean;

	begin
		if (Seat[aSeatNum] = nil) then begin
			Seat[aSeatNum]:= New(PCasinoSeat, Init(aPlayer, Chips));
			aPlayer^.Table:= @self;
			aPlayer^.SeatNum:= aSeatNum;
			TakeASeat:= True;
		end
		else
			TakeASeat:= False;
	end;

function TCasinoGame.Sit(const iSeat:Word; aPlayer:PCasinoPatron; const Chips:real):boolean;

	begin
		Sit:= TakeASeat(iSeat, aPlayer, Chips);
	end;

procedure TCasinoGame.unSeat(const iSeat:Word);

	begin
		{dispose(seat[iSeat], done);}
		seat[iSeat]:= nil;
	end;

procedure TPileOfChips.Double;

	var
		stop, i:integer;

	begin
		stop:= Count - 1;
		for i:= 0 to stop do begin
			Insert(New(PCasinoChip, Init(PCasinoChip(At(i))^.Denomination)));
		end;
	end;

procedure TCasinoDeck.setShufflePoint(const aPercent:integer);
begin
	ShufflePoint:= aPercent;
	ShuffleAt:= (100 - ShufflePoint) * Size div 100;
end;

constructor TCasinoDeck.Init(p_owner:PCasinoCardGame);
begin
	inherited Construct(Max_Packs * 52);
	_owner:= p_owner;
	setShufflePoint(80); {% penetration }
end;

function TCasinoDeck.timeToShuffle:boolean;
begin
	TimeToShuffle:= (Size <= ShuffleAt);
end;

function TCasinoPatron.Join(aTable:PCasinoGame; const aSeatNum:Word; const Chips:Real):boolean;

{ player joins a table }

begin
	Table:= aTable;
	Join:= aTable^.Sit(aSeatNum, @self, Chips);
end;

procedure TCasinoPatron.leave;
begin
	table^.unSeat(seatnum);
end;

procedure TPileOfChips.minimize;
{ minimize the number of chips in the pile }
var
	t:real;

begin
	t:= dollarValue;
	removeAmount(t);
	addAmount(t);
end;

function ChipUnitValue(toc:TypeOfChip):word;
begin
	System.Assert(toc <> CHIP1, 'ChipUnitValue: toc=CHIP1');
	ChipUnitValue:= Round(ChipDollarValue(toc));
end;

{$ifdef TEST}

procedure TestChipUnitValue;
begin
	punit.Assert.Equal(5, ChipUnitValue(CHIP3));
	punit.Assert.Equal(100, ChipUnitValue(CHIP5));
end;

{$endif TEST}

function TCasinoCardGame.PileRows:word;
begin
	PileRows:=6;
end;

function TCasinoCardGame.PileColumns:word; 
begin
	PileColumns:=8;
end;

begin
	{$ifdef TEST}
	Suite.Add(@TestChipUnitValue);
	Suite.Run('casino');
	{$else}
	_Pack.Init(False);
	Randomize;
	{$endif}
end.
