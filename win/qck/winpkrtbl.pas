{ (C) 2009 Wesley Steiner }

{$MODE FPC}

{$ifdef DEBUG}
{define TEST_ALLPASS}
{define TEST_DEALER}
{define TEST_OPEN_BUTTON}
{$endif}

unit winpkrtbl;

{$I platform}
{$I punit.inc}

interface

uses
	objects,
	windows,
	std,
	owindows,
	odlg,
	cards,
	casino,
	poker,
	quick,
	stdwin,
	toolbars,
	quickWin,
	qcktbl,
	winqcktbl,
	winCardFactory,
	casview,
	winTabletopChips,
	pokerlib,
	drawplib,
	studplib;
	
const
	CM_OPTIONSDRAW 		=250;
	CM_OPTIONSSTUD 		=251;
	CM_GAMEDRAW 		=252;
	CM_GAMESTUD 		=253;
	CM_STATS 			=254;
	{$ifndef VER1}
	CM_HISTORY 			=255;
	CM_ODDS 			=256;
	{$endif}
	CM_DEAL				=257;
	CM_OPEN				=258;
	CM_CANCELBET		=259;
	CM_PASS				=260;
	CM_CALL				=261;
	CM_FOLD				=262;
	CM_RAISE			=263;

	WM_DEALBUTTON 		=WM_USER+0;
	WM_PASS 				=WM_USER+1;
	WM_OPEN 				=WM_USER+2;
	WM_CALL 				=WM_USER+3;
	WM_RAISE 			=WM_USER+4;
	WM_FOLD				=WM_USER+5;
	WM_NEXTOPEN 		=WM_USER+6;
	WM_DEAL				=WM_USER+7;
	WM_NEXTBET			=WM_USER+8;
	WM_STUDDEAL 		=WM_USER+9;
	WM_CANCELBET		=WM_USER+11;
	WM_NEXTDEALER		=WM_USER+12;
	WM_NEXTDRAW 		=WM_USER+14; { NEXT PLAYER TO "DRAW" IN DRAW }
	WM_SHOWDOWN 		=WM_USER+15;
	WM_ENABLEDEALBUTTON=WM_USER+16;
	NoneYet=-1;

type
	point32=packed record 
		x,y:int16;
	end;
	assignedArray=array[1..maxHandSize] of boolean;
	deckArray = array[1..60] of TCard;
	ViewP=^View;
	PokerPlayerP=^PokerPlayer;

	PNote=^TNote;
	TNote=object
		handle:HWND;
		FixedWd:boolean; { true if the window width is fixed }
		constructor Init(a_pParentWindow:PWindow;a_win_title_p:PChar;x,y:integer);
		procedure Show; virtual;
		procedure Hide; virtual;
		function IsVisible:boolean;
		procedure SetText(aTextString:PChar); virtual;
	end;

	PStatusNote=^TStatusNote;
	TStatusNote=object(TNote)
		PlrNo:playerIndex;
		constructor Init(aParent:PWindow;pn:playerIndex);
		procedure Show; virtual;
	end;

	OPlayerStatusWart=object(OPropwart)
		function GetContent:string; virtual;
	end;
	PPlayerStatusWart=^OPlayerStatusWart;
	
	PokerHandProp=object(OCardpileProp)
		constructor Construct(aIndex:playerIndex);
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
	private
		myPlayerId:playerIndex;
		procedure CopyFromHandData(const rHand:handType;aFaceupFlag:boolean);
	end;
	PPokerHandProp=^PokerHandProp;

	PlayerChipsPropP=^PlayerChipsProp;
	PlayerChipsProp=object(TSeatChipsView)
		constructor Construct(pView:ViewP; pOwner:PokerPlayerP; seat_num:SeatIndex);
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
		function PopChip(aChipType:TypeOfChip):TypeOfChip; virtual;
		procedure OnEnabled; virtual;
		procedure OnStackClicked(aChipType:TypeOfChip); virtual;
	private
		Owner:PokerPlayerP;
		myTable:ViewP;
		function CurrentBetLimit:word; test_virtual
		procedure CalcAnchorPt(var aAnchorPt:TPoint;aNewWd,aNewHt:word);
		procedure Toss(aChipType:TypeOfChip); test_virtual
		procedure CueBadClick; test_virtual
	end;

	OPlayerNameTag=object(OPropwart)
		constructor Construct(parent_prop:PHotspot; nickname:string);
		function GetContent:string; virtual;
	private
		nickname:string;
	end;
	PPlayerNameTag=^OPlayerNameTag;

	{$ifdef TEST}
	testable_OPlayerChipsProp=object(PlayerChipsProp)
		bundle:TBundleOfChips;
		constructor Construct;
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure Refresh; virtual;
		procedure RefreshRect(const rPrevSpan:TRect); virtual;
	end;

	FakePlayerChipsProp=object(testable_OPlayerChipsProp)
	end;
	{$endif}

	View=object(winqcktbl.OTabletop)
		hand_props:array[playerIndex] of PokerHandProp;
		constructor Construct(background_color:TColorRef;bg_image:HBITMAP;use_image:boolean);
		destructor Done; virtual;
		function Create(frame:HWND;w,h:number):HWND; virtual;
		function CurrentPot(aPlayerId:playerIndex):word; test_virtual
		function OnSize(resizeType:uint;newWidth,newHeight:integer):LONG; virtual;
		procedure NextDealer(currentDealer:playerIndex);
		procedure  NextDraw(aPlayerId:playerIndex);
		procedure OnCall;
		procedure OnFold;
		procedure OnNextBet(aPlayerId:playerIndex);
		procedure OnNextOpen(aPlayerId:playerIndex);
		procedure OnOpen;
		procedure OnPass;
		procedure OnRaise;
		procedure OnCancel;
		procedure OnChipTossed(aPlayersBet:word);
		procedure OnShowdown;
		procedure OnStudDeal;
		procedure Paint(PaintDC:HDC; var PaintInfo:TPaintStruct); virtual;
		procedure PlayerRaise(aPlayerId:playerIndex;raise_amount:number);
		procedure StartDraw;
		procedure Startup;
		procedure StartStud;
	private
		function CallEnabled:boolean; test_virtual
		function NamePlateX(aPlayerId:playerIndex):integer;
		function NamePlateY(aPlayerId:playerIndex):integer;
		function OpenEnabled:boolean; test_virtual
		function PlayerBetLimit(aPlayerId:playerIndex):word; test_virtual
		function RaiseAllowed(aPlayerId:playerIndex;var limits:pokerRaiseLimits):boolean; test_virtual
		function RaiseEnabled:boolean; test_virtual
		procedure DrawDealerPlate(adc:HDC;aPlayerId:playerIndex;adjustX,adjustY:integer);
		procedure EnableFold(value:boolean); test_virtual		
		procedure EnableOpen(value:boolean); test_virtual
		procedure EnablePass(value:boolean); test_virtual
		procedure EnableCall(value:boolean); test_virtual
		procedure EnableRaise(value:boolean); test_virtual
		procedure GetNamePlateRect(var aRect:tRect;aPlayerId:playerIndex);
		procedure HidePlayerPrompt; test_virtual
		procedure PostCommand(aCommand:UINT;wParam:WPARAM;lParam:LPARAM); test_virtual
		procedure ProcessRaiseByPlayer(aPlayerId:playerIndex;amount:word);
		procedure RestoreButtonStates;
	end;
	
	PokerBetPropP=^PokerBetProp;
	PokerBetProp=object(PlayerBetProp)
		constructor Construct(pPlayer:PokerPlayerP);
	private
		Player:PokerPlayerP;
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
		function SeatNum:SeatIndex;
		procedure CalcNewAnchorPt(aNewWd,aNewHt:word; var rResult:TPoint);
		procedure OnValueChanged; virtual;
		procedure SnapToPot;
	end;

	{$ifdef TEST}
	testable_OBetProp=object(PokerBetProp)
		pile:TPileOfchips;
		constructor Construct;
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure Refresh; virtual;
	end;

	FakeBetProp=object(testable_OBetProp)
	end;
	{$endif}

	PokerPlayer=object(quickWin.Player)
		participating:boolean; { true if this position is playing }
		pno:shortInt; { position around the table }
		cno:shortInt; { player # of the database of computer players, or for the human the "UserIcon" # }
		status:playerStatus;
		GoForBlf:boolean; { player is going for a bluff on this round }
		skill:skillLevel;
		smplDpth:integer; { how many samples this player will look ahead with when estimating odds }
		potOddsFactor:real; { fraction of the pot to add to the pot odds when determining odds of winning. }
		goForIt:real; { 0.0-1.0 % of times player will stay in after he has determined that he is beat by all his predicted minimums }
		winLimit:real; { see "wlm" }
		raiseLmt:real; { prob of win must be >= this to raise }
		orgMny:integer; { original amount of money player has when starting }
		myChips:PlayerChipsProp;
		myBet:PokerBetProp;
		my_house_loan:integer;
		constructor Init(nickName:string;g:gender;posn:shortInt;p:boolean;sk:skillLevel;wl:real);
		function Bet:PokerBetPropP; test_virtual
		function Chips:PlayerChipsPropP; test_virtual
		function CurrentBet:word; test_virtual
		function IsDealer:boolean;
		function IsPlaying:boolean;
		function HouseLoan:integer;
		function NetWinnings:integer;
		function NetWinningsAsText:string;
		function Purse:integer; test_virtual
		function ShouldOpen:boolean;
		function ShowStatus:boolean;
		procedure Borrow(n:number);
		procedure ExposeSortedHand;
		procedure Raise(amount:number);
	private
		procedure SafeInitialize;
		procedure CommitBetToPot; test_virtual
	end;

	{$ifdef TEST}
	testable_OPokerPlayer=object(PokerPlayer)
		fake_chips:FakePlayerChipsProp;
		fake_bet_prop:FakeBetProp;
		constructor Construct;
		function Bet:PokerBetPropP; virtual;
		function Chips:PlayerChipsPropP; virtual;
	end;
	{$endif}
	
	pokerOptionsType=record
		game_id:pokerGameId;
		speedDeal:boolean; { deal em fast }
	end;

	PotProp=object(OChipstackProp)
		constructor Construct;
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
		procedure OnValueChanged; virtual;
	end;

	PDealButton=^TDealButton;
	TDealButton=object(TBarButton)
		procedure OnEnabled; virtual;
	end;

	TDrawDlg=object(ODialog)
		constructor Init;
		function OnInitDialog:boolean; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		function WMDrawItem(controlId:UINT;aDrawItemStruct:PDrawItemStruct):LONG;
		procedure SetState(i:integer;State:boolean);
	end;

const
	the_toolbar:PlaybarP=NIL;
	theDealerIndex:integer=0;
	MAXSAMPLEN:integer=200; { Sample this many hands when searching for a
		hand to meet the prediction parameters. According to my analysis if
		you haven't found after this many hands then you aren't going to
		find it by searching any longer. }
	Started:boolean=FALSE; { cheat }
	the_show_stats_flag:boolean=TRUE;
	STATUS_TEXT:array[PASSED..BET] of string[5]=('Pass','Open','Fold','Raise','Call','Check','Bet');
	statusCode:array[PASSED..BET] of char=('P','O','F','R','C','K','B'); { codes for the history string }
	scoringDesc:array[scoringType] of string[15]=(
		'High'
		{$ifndef VER1}
		,
		'Low',
		'High/Low'
		{$endif}
		);
	the_poker_options:pokerOptionsType=(game_id:DRAW_POKER;speedDeal:FALSE);
	RoundCount:word=0; { how many rounds played so far }
	{$ifdef ANAL}
	USER_ID:playerIndex=0;
	{$else}
	USER_ID:playerIndex=1;
	{$endif}

var
	the_short_hand_valuations:array [playerIndex] of PokerHandEvaluation;
	nCardsDrwn:array [playerIndex] of word; { how many cards each player drew }
	the_pot:PotProp;
	CompIcons:boolean;
	rndHistory:string[255]; { history of what occurred during the current
		round. Format is "#:<entry>;|$;..." where # is the player number,
		<entry> is a single char code that represents what they did and
		$ means start a new line. }
	whoBetLast:playerIndex; { who last OPENED the betting }
	the_last_raise_amount:integer;
	the_players:array[playerIndex] of PokerPlayer;
	theHands:array[playerIndex] of handType; { cards that were dealt in the order they were dealt }
	DiscardPileN:integer; { how many in it so far }
	sure:array[playerIndex,1..maxHandSize] of boolean; { true if the card is known by all players }
	gngFr:array[playerIndex] of scrngDcsnTyp; { scoring mode that each player is going for }
	assignedTo:array[playerIndex] of array[TPip,TSuit] of playerIndex; { player # that this card has been assigned to }
	deckIndex:integer; { current index to the top of the deck }
	theDeck:deckArray;
	shouldRaise:boolean; { true if current player should raise }
	{ what each computer player predicts about the other player's hands }
	predict:array[playerIndex] of array[playerIndex] of record
		isBlffng:boolean; { I think this player is bluffing }
		isGngFr:scrngDcsnTyp; { what I think this player is going for, HIGH or LOW }
		h:handType; { the minimum hand I predict he has }
		eval:pkrResultType; { current the_hand_valuations of "h" }
		assigned:assignedArray; { true if each card in the hand "h" has been assigned (predicted or known) }
	end;
	knows:array[playerIndex,playerIndex] of record
		playsSWS:boolean; { this player plays the safe-way-system }
		estSkill:skillLevel; { estimated skill level }
	end;
	WhoOpened:integer;
	DrawStarted:boolean; { true if the draw has started }
	studRndNo:integer; { the round number currently in play (1..n) }
	WhoLastBet:integer;
	barbDeal:PDealButton;
	barbOpen:PBarButton;
	barbPass:PBarButton;
	barbCall:PBarButton;
	barbRaise:PBarButton;
	barbFold:PBarButton;
	theMessageLine:TBarTextBox;
	barbCancel:PBarButton;
	savedOpenState,
	savedPassState,
	savedCallState,
	savedRaiseState,
	savedFoldState:boolean;

function PotAmt:integer;
procedure EndTheDraw;
procedure SetupNewRound;
procedure SetGlobals;
procedure HumanDraw;
procedure SetupForOdds;
procedure predictHand(po,px:playerIndex;var e:pokerHandEvaluation; { put the result in here } lowResult:handrank;lowPip:TPip;highResult:handrank;highPip:TPip;ignore:pokerHandTypeSet { ignore these hand type });
function PlayerHistory(po:playerIndex;s:playerStatus):boolean;
procedure DrawPokerRound;
function PlayingStudPoker:boolean;
function PlayingDrawPoker:boolean;
function AnythingOpensVar:boolean;
function AfterTheDraw:boolean;
function frstRnd:integer;
function PassAllowed:boolean;
procedure Deal;
procedure ClearAllStatus;
procedure studPokerResult(h:handType;var e:pkrResultType);
procedure showDown(doHigh,doLow:boolean);
procedure setWildCardType(wt:wildType);
function NetAmt(P:playerIndex):integer;
function PileValue(const chg:TSeatChipsView):integer;
function lastRnd:integer;
procedure UpdateEvaluation;
procedure makePredHand(po,px:playerIndex;var hnd:handType);
function HighestShowing(f,l:integer):playerIndex;

procedure AbortCurrentRound;
procedure StartRound;
procedure PlayerCall(pn:playerIndex);
procedure PlayerOpen(pn:playerIndex;amount:integer);
procedure PreGame;
 
implementation

uses
	{$ifdef TEST} punit, {$endif}
	strings,
	stringsx,
	windowsx,
	sdkex,
	gdiex,
	pokerTable;
	
const
	INFINITY=9999; { for calculating the odds }
	frame_handle:HWND=NULL_HANDLE;
	
{$ifdef TEST}
type
	testable_OView=object(View)
		myCommand:UINT;
		constructor Construct;
		procedure PostCommand(aCommand:UINT;wParam:WPARAM;lParam:LPARAM); virtual;
	end;

constructor testable_OView.Construct;
begin 
end;

procedure testable_OView.PostCommand(aCommand:UINT;wParam:WPARAM;lParam:LPARAM); 
begin 
	myCommand:=aCommand; 
end;
{$endif}

var
	the_status_notes:array [playerIndex] of PStatusNote;
	the_hand_valuations:array [playerIndex] of pkrResultType;
	the_view:ViewP;
	thnkMsg,BulbCursor:hCursor;
	theBets:array[playerIndex] of integer; { amount each player has contributed so far in the current betting interval }
	
function DELAY_ACTION:integer; 
begin 
	DELAY_ACTION:=(BASEDELAY*40);
end;

procedure ThinkMsgOn;
begin
	BulbCursor:=LoadCursor(hInstance,MakeIntResource(401));
	ThnkMsg:=SetCursor(BulbCursor);
end;

procedure ThinkMsgOff;
begin
	SetCursor(ThnkMsg);
	DestroyCursor(BulbCursor);
end;

function MessageLine:string;
begin
	MessageLine:='Pot '+NumberToString(PotAmt)
end;

{$ifdef TEST}
procedure InitializeTestPlayer(api:playerIndex);
begin
	the_status_notes[api]:=NIL;
	the_players[api].myBet.Construct(nil);
end;

procedure InitializeTestPlayers;
var
	api:playerIndex;
begin
	for api:=Low(playerIndex) to High(playerIndex) do InitializeTestPlayer(api);
end;

procedure SetupTestPlayersBet(api:playerIndex;aAmount:integer;isParticipating:boolean);
begin
	the_players[api].Participating:=isParticipating;
	the_players[api].myBet.TheChips^.AddAmount(aAmount);
end;

procedure Test_MessageLine;
	procedure SetupBetAndPot(aCurrentBet,aPotAmt:integer);
	begin
		the_current_bet:=aCurrentBet;
		the_pot.Construct;
		the_pot.TheChips^.AddAmount(aPotAmt);
	end;
begin
	SetupBetAndPot(0,0);
	InitializeTestPlayers;
	punit.Assert.EqualStr('Pot 0', MessageLine);
	SetupBetAndPot(1,2);
	SetupTestPlayersBet(1,11,TRUE);
	SetupTestPlayersBet(2,22,FALSE);
	SetupTestPlayersBet(3,33,TRUE);
	punit.Assert.EqualStr('Pot 46', MessageLine);
end;
{$endif TEST}

procedure UpdateMessageLine;
var
	buffer:stringBuffer;
begin
	theMessageLine.Update(StrPCopy(buffer,MessageLine));
end;

procedure UpdateMessageLineProxy;
begin
	{$ifndef TEST}
	UpdateMessageLine;
	{$endif}
end;

procedure SaveButtonStates;
begin
	savedOpenState:=barbOpen^.IsEnabled;
	savedPassState:=barbPass^.IsEnabled;
	savedCallState:=barbCall^.IsEnabled;
	savedRaiseState:=barbRaise^.IsEnabled;
	savedFoldState:=barbFold^.IsEnabled;
end;

procedure AddOpponent(posn,plr:integer);
{ Logically add computer player # "plr" to game position "posn". }
begin // writeln('AddOpponent(posn=',posn,',plr=',plr,')');
	with the_opponent_pool[plr] do begin
		the_players[posn].init(pnn,gndr,shortInt(posn),true,slv,wlm);
		the_players[posn].raiseLmt:=rlm;
		the_players[posn].potOddsFactor:=pof;
		the_players[posn].smplDpth:=smpd;
		the_players[posn].goForIt:=gfi;
	end;
	the_players[posn].myChips.Construct(the_view,@the_players[posn],SeatIndex(posn));
	the_players[posn].myBet.Construct(@the_players[posn]);
	the_view^.hand_props[posn].Show;
	the_players[posn].myChips.Show;
	the_players[posn].myBet.Show;
end;

function SelectRandomOpponents:opponentPoolIndexSet;
var
	i:playerIndex;
	j:opponentPoolIndex;
	opponents:opponentPoolIndexSet;
	indexes:array[opponentPoolIndex] of byte;
begin
	for j:=Low(opponentPoolIndex) to High(opponentPoolIndex) do indexes[j]:=j;
	RandomShuffleBytes(@indexes[Low(opponentPoolIndex)],High(opponentPoolIndex));
	opponents:=[];
	for i:=1 to MAX_PLAYERS-1 do include(opponents,indexes[i]);
	SelectRandomOpponents:=opponents;
end;

{$ifdef TEST}
procedure Test_SelectRandomOpponents;
	function CountElementsInSet(opponents:opponentPoolIndexSet):integer;
	var
		count:long;
		i:opponentPoolIndex;
	begin
		count:=0;
		for i:=Low(opponentPoolIndex) to High(opponentPoolIndex) do if i in opponents then Inc(count);
		CountElementsInSet:=Integer(count);
	end;
begin
	AssertAreEqual(MAX_PLAYERS-1,CountElementsInSet(SelectRandomOpponents));
end;
{$endif}

procedure RandomShuffleByteArray(var aByteArray:array of byte;aSize:integer);
var
	i,i1,i2:integer;
	t:byte;
begin
	for i:=1 to aSize*5 do begin
		i1:=Integer(random(aSize));
		i2:=Integer(random(aSize));
		t:=aByteArray[i1];
		aByteArray[i1]:=aByteArray[i2];
		aByteArray[i2]:=t;
	end;
end;

procedure AddRandomOpponents;
var
	posn:integer;
	i:integer;
	opponents:set of opponentPoolIndex;
	indexes:array[1..MAX_PLAYERS-1] of byte;
	procedure ConvertSetToByteArray;
	var
		i:opponentPoolIndex;
		n:integer;
	begin
		n:=1;
		for i:=Low(opponentPoolIndex) to High(opponentPoolIndex) do if i in opponents then begin
			indexes[n]:=i;
			Inc(n);
		end;
	end;
begin
	opponents:=SelectRandomOpponents;
	ConvertSetToByteArray;
	RandomShuffleBytes(@indexes[1],High(indexes));
	posn:=USER_ID;
	for i:=1 to sizeof(indexes) do begin
		Inc(posn);
		AddOpponent(posn,indexes[i]);
	end;
end;

procedure DefaultPokerGame;
var
	pn,pm:playerIndex;
begin //writeln('DefaultPokerGame');
	the_players[USER_ID].init('YOU',male,1,true,0,0);
	the_players[USER_ID].CNo:=1;
	the_players[USER_ID].myChips.Construct(NIL, @the_players[1], 1);
	the_players[USER_ID].myBet.Construct(@the_players[1]);
	AddRandomOpponents;
	{ initialize the skill level that each player estimates about all the other players }
	for pn:=1 to MAX_PLAYERS do with the_players[pn] do if isPlaying then
		for pm:=1 to MAX_PLAYERS do with knows[pm,pn] do begin
			playsSWS:=false;
			estSkill:=0; { average }
		end;
end;

const
	InitOddsGen=100; { generate this many random hands for the odds table
		on each increment. }

type
	HDataRec=record
		HType:handrank;
		OData:longInt; { from the odds table }
	end;

var
	cgWilds:WildCardList;
	cgHandSize:integer;

procedure SetupForOdds;

	{ This procedure keeps track of the global variables that control
		when it should start the odds over in case something changes.
		In this way each time it is called it can check to see if any
		of the variables have changed since the last time it was called
		and if so reset the odds table to 0 before starting again. }

	begin
		cgWilds:=_wilds_;
		cgHandSize:=HandSize;
	end;

procedure shuffleDeck(var p_deck:deckArray; p_n:integer; p_times:integer);

var
	k, i, i1, i2:integer;
	t:byte;

begin
	for k:= 1 to p_times do for i:= 1 to p_n * 3 do begin
		i1:= random(p_n) + 1;
		i2:= random(p_n) + 1;
		t:= p_deck[i1];
		p_deck[i1]:= p_deck[i2];
		p_deck[i2]:= t;
	end;
end;

function useShortHands:boolean;
{ Return true if we are concerned about short value hands. }
begin
	useShortHands:=theDrawPokerOptions.m_aScoringMode in [playHigh {$ifndef VER1}, playHighLow {$endif}];
end;

procedure pokerShuffle;
begin
	delay(DELAY_ACTION);
	shuffleDeck(theDeck, deckIndex, 3);
end;

function NextClockwiseFrom(pn:playerIndex):playerIndex;
begin
	repeat
		if pn = MAX_PLAYERS then pn:= 1 else inc(pn);
	until the_players[pn].participating;
	NextClockwiseFrom:=pn;
end;

procedure SetupNewRound;
{ Initialize things for a new round. }
var
	pn:playerIndex;
	p:TPip;
	s:TSuit;
	c:playingCard;
begin
	for pn:=1 to MAX_PLAYERS do with the_players[pn] do begin
		GoForBlf:=False;
		fillChar(the_card_pool,sizeOf(the_card_pool),#0);
	end;
	deckIndex:=0;
	for p:=acePip to kingPip do
		for s:=clubSuit to spadeSuit do begin
			inc(deckIndex);
			theDeck[deckIndex]:= MakeCard(p,s);
			{ adjust each player's card count array }
			for pn:=1 to MAX_PLAYERS do inc(the_card_pool[pn][p,s]);
		end;
	c:= MakeCArd(tjoker, tspades);
	if isWild(c) then begin
		theDeck[deckIndex]:= c;
		for pn:=1 to MAX_PLAYERS do inc(the_card_pool[pn][jokerPip,spadeSuit]);
	end;
	pokerShuffle;
	rndHistory:='';
	pn:=playerIndex(theDealerIndex);
	repeat
		pn:=NextClockwiseFrom(pn);
		with the_players[pn] do begin
			status:=NOSTATUS;
		end;
	until pn=theDealerIndex;
end;

procedure DrawPokerDeal;
{ Deal out a hand in Draw Poker. }
var
	pn,i,j:integer;
	px:playerIndex;
	pip:TPip;
	h:handrank;
	tempDeck:deckArray;
	p:TPip;
	s:TSuit;
begin
	discardPileN:=0;
	delay(cardPlayDelay);
	{ deal the cards }
	for pn:=1 to MAX_PLAYERS do with the_players[pn] do if participating then begin
		theHands[pn].n:=0;
		the_view^.hand_props[pn].CopyFromHandData(theHands[pn], true);
		for i:=1 to maxHandSize do sure[pn,i]:=false;
	end;
	for j:=1 to handSize do begin
		pn:=theDealerIndex;
		repeat
			pn:=Integer(NextClockwiseFrom(playerIndex(pn)));
			with the_players[pn] do begin
				inc(theHands[pn].n);
				theHands[pn].card[j]:=theDeck[deckIndex];
				with the_view^.hand_props[pn] do begin
					if pn = USER_ID
						then AddCard(theHands[pn].card[j] or FACEUP_BIT)
						else AddCard(theHands[pn].card[j]);
				end;
				dec(deckIndex);
				dec(the_card_pool[pn][cards.CardPip(theHands[pn].card[j]),CardSuit(theHands[pn].card[j])]);
			end;
			the_view^.hand_props[pn].Refresh;
			SndCardFlick;
			if the_poker_options.speedDeal then delay(cardPlayDelay);
		until pn=theDealerIndex;
		{delay(cardPlayDelay);}
	end;
	for pn:=1 to MAX_PLAYERS do with the_players[pn] do if participating then begin
		{ for each paricipating player }
		pokerResult(theHands[pn],the_hand_valuations[pn]);
		addOddsTable(the_hand_valuations[pn].eh);
		if useShortHands then begin
			shortHandFlag:=true;
			EvaluateBest(theHands[pn],4,the_short_hand_valuations[pn]);
			shortHandFlag:=false;
		end;
		{ Decide if player PN should go for the bluff or not }
		if
			(GetScoreMode=PlayHigh)
			and
			(the_hand_valuations[PN].EH.Rslt=RUNT)
			and
			(the_short_hand_valuations[PN].Rslt=RUNT)
			and
			(Random(1000)<5)
			then
			the_players[PN].GoForBlf:=True;
		{$ifndef VER1}
		if getScoreMode = playHighLow then begin
			{ setup the obvious HIGH or LOW cases }
			if the_hand_valuations[pn].eh.rslt>ONE_PAIR then with gngFr[pn] do begin
				{ this player already knows he's going for high }
				dcdd:=true;
				scrMd:=playHigh;
			end
			else if the_hand_valuations[pn].el.rslt=RUNT then with gngFr[pn] do begin
				{ this player already knows he's going for low }
				dcdd:=true;
				scrMd:=playLow;
			end;
		end;
		{$endif}
		for i:=1 to maxHandSize do sure[pn,i]:=false;
		{ make a temporary deck "tempDeck" from "pn"'s card pool. }

		i:=0;
		for p:=acePip to kingPip do for s:=clubSuit to spadeSuit do if the_card_pool[pn][p,s]>0 then begin
			inc(i);
			tempDeck[i]:= makecard(p,s);
		end;
		shuffleDeck(tempDeck, i, 3); { give it a good shuffle }
		{ for each opponent }
		j:=1; { index into "tempDeck" }
		for px:=1 to MAX_PLAYERS do if px<>pn then with predict[pn,px] do begin
			isBlffng:=false;
			isGngFr.dcdd:=false;
			for i:=1 to maxHandSize do assigned[i]:=false;
			{ distribute the cards remaining in "pn"'s card pool amongst his
				opponents ("px") so that he has something to go by when calculating
				the odds for openers. These predictions will be adjusted after
				each opponent does something like PASS, OPEN, CALL etc. }
			h.n:=0;
			for i:=1 to handSize do with h do begin
				inc(n);
				card[n]:=tempDeck[j];
				inc(j);
			end;
			pokerResult(h,eval);
			addOddsTable(eval.eh);
		end; { each opponent }
	end; { each player }
	{$ifndef ANAL}
	delay(DELAY_ACTION);
	{$endif}
	{$ifdef PAUSE} doPause; {$endif}
end;

function PileValue(const chg:TSeatChipsView):integer;

{ Return the value of a pile of chips. }

begin
	pileValue:= Round(chg.Gross);
end;

procedure borrowMoney(po:playerIndex;n:integer);
begin
	the_players[po].myChips.Borrow(n);
	the_players[po].Borrow(n);
end;

procedure VerifyPlayersCredit(po:playerIndex; n:word);
begin
	with the_players[po] do if pileValue(myChips) < n then borrowMoney(po, n - pileValue(myChips));
end;

procedure anteUp(nUnitChips:word);
begin
	VerifyPlayersCredit(playerIndex(theDealerIndex),nUnitChips);
	the_pot.AddAmount(the_players[theDealerIndex].myChips.PopUnits(nUnitChips));
end;

function GetDrawAnte:Word;
begin
	with theDrawPokerOptions do GetDrawAnte:=AnteChips[gType];
end;

function GetStudAnte:Word;
begin
	with theStudPokerOptions do GetStudAnte:=AnteChips[gType];
end;

function PlayingDrawPoker:boolean;
begin
	PlayingDrawPoker:=the_poker_options.game_id=DRAW_POKER;
end;

function PlayingStudPoker:boolean;

begin
	PlayingStudPoker:=the_poker_options.game_id=STUD_POKER;
end;

function NobodyOpenedyet:boolean;

	{ Return true if no one has OPENED the betting yet. }

	begin
		NobodyOpenedYet:=(WhoOpened=NoneYet);
	end;

var
	opntNo,
	callerNo:playerIndex;

function BeforeTheDraw:boolean;

begin
	BeforeTheDraw:=not DrawStarted;
end;

function AfterTheDraw:boolean;

begin
	AfterTheDraw:=(not BeforeTheDraw);
end;

function CurrentAnte:word;

begin
	if PlayingDrawPoker then
		CurrentAnte:=GetDrawAnte
	else
		CurrentAnte:=GetStudAnte;
end;

procedure NewBettingInterval;

begin
	FillChar(theBets,SizeOf(theBets),#0);
end;

function PassIsCheck:boolean;

{ True if a Pass is called a Check. }

begin
	PassIsCheck:=
		PlayingStudPoker
		or
		(PlayingDrawPoker and AfterTheDraw);
end;

function nextPlaying(po:playerIndex):playerIndex;

	{ Return the next player number to the left of player number "po"
		that has not yet FOLDED. }

	var
		p:playerIndex;
	begin
		p:=playerIndex(po);
		repeat
			p:=NextClockwiseFrom(p);
		until the_players[p].status<>FOLDED;
		nextPlaying:=p;
	end;

function PotAmt:integer;

var
	player:playerIndex;
	total:real;

begin
	total:=the_pot.Value;
	for player:=Low(playerIndex) to High(playerIndex) do
		if (the_players[player].Participating) then total:=total+the_players[player].myBet.Value;
	potAmt:=Trunc(total);
end;

{$ifdef TEST}

procedure Test_PotAmt;

begin
	the_pot.Construct;
	InitializeTestPlayers;
	SetupTestPlayersBet(1, 100, true);
	AssertAreEqual(100, PotAmt);
	SetupTestPlayersBet(5, 20, true);
	AssertAreEqual(120, PotAmt);
	SetupTestPlayersBet(3, 30, false);
	AssertAreEqual(120, PotAmt);
	the_pot.TheChips^.AddAmount(35);
	AssertAreEqual(155, PotAmt);
end;

{$endif}

function MaxChipStack:integer;
{ max chips you can display in a stack, must be greater
	than "chipDenomFactor" }
begin
	{$ifdef ANAL}
	maxChipStack:=12;
	{$else}
	maxChipStack:=17;
	{$endif}
end;

function GetWildCardType:wildType;

{ Return the wild card type for the current game. }

begin
	case the_poker_options.game_id of
		DRAW_POKER:getWildCardType:=theDrawPokerOptions.wType;
		STUD_POKER:getWildCardType:=theStudPokerOptions.wType;
	end;
end;

procedure setWildCardType(wt:wildType);

{ Set the wildcard type flag for the current game to "wt". }

begin
	case the_poker_options.game_id of
		DRAW_POKER:theDrawPokerOptions.wType:=wt;
		STUD_POKER:theStudPokerOptions.wType:=wt;
	end;
end;

const
	chipDenomFactor=5; { mult factor for each chip denomination }

function nPlayers:integer;

{ Return the number of players still in the current round. }

var
	d:integer;
	pn:playerIndex;
begin
	d:=0;
	for pn:=1 to MAX_PLAYERS do if the_players[pn].isPlaying then inc(d);
	nPlayers:=d;
end;

function nPlay:integer; { number of players at the table }

var
	d:integer;
	pn:playerIndex;

begin
	d:=0;
	for pn:=1 to MAX_PLAYERS do if the_players[pn].participating then inc(d);
	nPlay:=d;
end;

procedure DiscardHand(po:playerIndex);
begin //writeln('DiscardHand(',po,');');
	theHands[po].n:=0;
	the_view^.hand_props[po].ThePile^.Empty;
end;

procedure ClearHand(po:playerIndex);
var
	aSpan:TRect;
begin
	the_status_notes[po]^.Hide;
	the_players[po].status:=NOSTATUS;
	the_view^.hand_props[po].GetSpanRect(aSpan);
	DiscardHand(po);
	the_view^.RefreshRect(aSpan);
end;

procedure joinGame(po,plr:integer);

{ Logically and Visually add computer player # "plr" to game position "po". }

begin
	AddOpponent(po,plr);
	the_view^.hand_props[po].Refresh;
	rndHistory:='';
end;

procedure leaveGame(po:playerIndex);

{ Take player # "po" out of the game. }

begin
	the_status_notes[po]^.Hide;
	the_players[po].Participating:=false;
	the_view^.hand_props[po].Refresh;
	rndHistory:='';
end;

function RandomRotation:integer;

begin
	RandomRotation:=Random(4)+1;
end;

procedure ShowPlayerStatus(po:playerIndex);

begin
	with the_players[po] do if ShowStatus then the_status_notes[po]^.Show;
end;

procedure updatePot(po:playerIndex;n:word);
begin //writeln('updatePot(',po,',',n,')');
	VerifyPlayersCredit(po, n);
	if n>0 then the_players[po].myBet.AddAmount(the_players[po].myChips.PopUnits(n));
	ShowPlayerStatus(po);
end;

function prevPlayerNo(player:playerIndex):playerIndex;

	{ Return the player number to the right of player number "po". }

	begin

	if player=1 then
			prevPlayerNo:=MAX_PLAYERS
	 else
		prevPlayerNo:=player-1;

	end;

function prevPlayer(po:integer):integer;

	{ Return the previous player number to the right of player number "po". }

	begin

		repeat
			if po=1 then
				po:=MAX_PLAYERS
			else
				dec(po);
		until the_players[po].participating;

		prevPlayer:=po;

	end;

function prevPlaying(pn:integer):integer;

	{ Return the previous player number to the right of player number "pn"
		that has not yet FOLDED. }

	var

		p:playerIndex;

	begin

		p:=playerIndex(pn);
		repeat
			p:=playerIndex(prevPlayer(Integer(p)));
		until the_players[p].status<>FOLDED;

		prevPlaying:=p;

	end;

function everyoneExcept(ignore:playerIndex;s:playerStatus):boolean;

	{ Return true if all players that are participating in the game and
	still in the round (not FOLDED) other than "ignore" have status "s". }

	var

		i:integer;

	begin

		everyoneExcept:=true;
		for i:=1 to MAX_PLAYERS do if (i<>ignore) then with the_players[i] do
			if participating and (status<>FOLDED) and (status<>s) then begin
				everyoneExcept:=false;
				exit;
			end;

	end;

function potOdds(betAmt:integer):real;

{ Return the current pot odds of betting "BetAmt" unit chips. }

begin
	potOdds:=(potAmt+betAmt)/betAmt;
end;

procedure makeHandDesc(var s:string;v:pokerHandEvaluation);

	{ Create a description of a poker result "v".

		Put the poker hand description in s. }

	begin

		if (v.rslt=STRAIGHT_FLUSH) and (v.pip=acePip) then
			s:='ROYAL FLUSH'
		else
			s:=pokerHandDesc[v.rslt];

		{
		case v.rslt of
			RUNT:
				s:=pokerHandDesc[v.rslt];
			ONE_PAIR:
				s:='PAIR OF '+playingCardPipDesc[v.pip]+#39's';
			TWO_PAIR:
				s:='TWO PAIRS ';
				+playingCardPipChr[v.pip]+','+playingCardPipChr[v.pip2];
			THREE_KIND:
				s:='THREE '+playingCardPipDesc[v.pip]+#39's';
			FULLHOUSE:
				s:=pokerHandDesc[v.rslt];
			FOUR_KIND:
				s:='FOUR '+playingCardPipDesc[v.pip]+#39's';
			STRAIGHT_FLUSH:
				if (v.pip=acePip) then
					s:='ROYAL FLUSH'
				else
					s:=pokerHandDesc[v.rslt];
			FIVE_KIND:
				s:='FIVE '+playingCardPipDesc[v.pip]+#39's';
		end;
		}

	end;

procedure	addStatus(po:playerIndex;s:string);
{ add "s" to the history string for player "po" }
begin
	rndHistory:=rndHistory+int2str(po)+':'+s+';';
end;

procedure newHistory; { start a new line in the history record }
begin
	rndHistory:=rndHistory+'$;';
end;

procedure DspPlrMsg(p:playerIndex;s:string);
var
	pstr:stringBuffer;
begin
	StrPCopy(pstr,s);
	if the_status_notes[p]<>NIL then with the_status_notes[p]^ do begin
		SetText(pstr);
		Show;
	end;
end;

const
	ClearStatus:boolean=False; { true when the next status update should clear any exisiting status notes.}

procedure ClearAllStatus;

var
	player:playerIndex;

begin
	for player:=1 to MAX_PLAYERS do with the_players[player] do
		if (Participating and (the_status_notes[player]<>NIL) and (the_status_notes[player]^.IsVisible)) then the_status_notes[player]^.Hide;
	ClearStatus:=False;
end;

procedure CollectAllBets;
var
	player:playerIndex;
begin
	for player:=1 to MAX_PLAYERS do with the_players[player] do if (Participating and (myBet.Value>0)) then CommitBetToPot;
end;

procedure UpdatePlayerStatus(p:playerIndex;s:playerStatus);
var
	ss:string;
begin
	if ClearStatus then ClearAllStatus;
	if s in [OPENED,BET,RAISED] then WhoBetLast:=p;
	with the_players[p] do begin
		{ setup the new status }
		status:=s;
		addStatus(p,statusCode[s]); { update the history string }
		ss:=STATUS_TEXT[status];
		if s=Called then
			ss:='In for '+int2str(the_current_bet)
		else if s in [OPENED,PlayerStatus(BET)] then
			ss:=ss+' '+int2str(the_current_bet)
		else if s=RAISED then
			ss:=ss+' '+int2str(the_last_raise_amount);
		if ShowStatus then DspPlrMsg(p,ss);
		delay(200);
	end;
end;

procedure EveryonePassedMsg;

{ Indicate that everyone PASSED then wait for the user to go for the deal again. }

begin
	MessageBox(frame_handle, 'Everyone PASSED!', 'Hand Over', mb_IconInformation or mb_OK);
end;

function frstRnd:integer;
{ Return the first betting round. }
begin
	case theStudPokerOptions.gType of
		stud5card:frstRnd:=2;
		stud6card:frstRnd:=2;
		stud7card
		{$ifndef VER1}
		,baseball
		{$endif}:
			frstRnd:=3;
	end;
end;

function lastRnd:integer;
{ Return the number of rounds. }
begin
	case theStudPokerOptions.gType of
		stud5card:lastRnd:=5;
		stud6card:lastRnd:=6;
		stud7card
		{$ifndef VER1}
		,baseball
		{$endif}:
			lastRnd:=7;
	end;
end;

procedure ClearHands;

var
	p:playerIndex;

begin
	for p:=1 to MAX_PLAYERS do if the_players[p].participating then ClearHand(p);
end;

procedure AllPassed;

	{ What to do if everyone PASSED or checked. }

	var
		pn:playerIndex;

	begin
		ClearAllStatus;
		if
			(PlayingDrawPoker and AfterTheDraw)
			or
			(PlayingStudPoker and (StudRndNo=LastRnd))
		then
			PostMessage(frame_handle,WM_SHOWDOWN,0,0)
		else if PlayingDrawPoker then begin
			if BeforeTheDraw then begin
				(*Raspberry;*)
				if USER_ID > 0 then begin
					EveryonePassedMsg;
					barbDeal^.Enable;
					SendMessage(frame_handle,WM_ENABLEDEALBUTTON,0,0); 
					ClearHands;
				end;
				if USER_ID = 0 then begin
					Delay(300);
					PostMessage(frame_handle, WM_DEALBUTTON, 0, 0);
				end;
			end;
		end
		else begin
			Inc(StudRndNo);
			for pn:=1 to MAX_PLAYERS do with the_players[pn] do if Participating and (Status=Checked) then Status:=NOSTATUS;
			PostMessage(frame_handle,WM_STUDDEAL,0,0);
		end
	end;

procedure EndBettingRound;

var
	buffer:stringBuffer;

begin
	ClearAllStatus;
	CollectAllBets;
	NewHistory;
	the_current_bet:=1;
	UpdateMessageLineProxy;
	if PlayingDrawPoker then begin
		if BeforeTheDraw then
			PostMessage(frame_handle,wm_NextDraw,NextClockwiseFrom(playerIndex(theDealerIndex)),0)
		else
			PostMessage(frame_handle,WM_SHOWDOWN,0,0);
	end
	else if StudRndNo<LastRnd then begin
		Inc(StudRndNo);
		PostMessage(frame_handle,WM_STUDDEAL,0,0);
	end
	else
		PostMessage(frame_handle,WM_SHOWDOWN,0,0);
end;

function BetsEqualized:boolean;

{ Return true if all player's have bet the same amount during this betting round. }

var
	pid:playerIndex;

begin
	for pid:=1 to MAX_PLAYERS do if (the_players[pid].IsPlaying) and (theBets[pid]<>the_current_bet) then begin
		BetsEqualized:=false;
		exit;
	end;
	BetsEqualized:=true;
end;

function Everyone(s:playerStatus):boolean;

	{ Return true if all players that have not FOLDED have status "s". }

	var
		i:integer;

	begin
		everyone:=true;
		for i:=1 to MAX_PLAYERS do with the_players[i] do
			if participating and (status<>FOLDED) and (status<>s) then begin
				everyone:=false;
				exit;
			end;
	end;

procedure DrawEvalFoldBeforeBy(po:playerIndex);

{ Evaluate what a FOLD by player "po" means to his opponents.

	When player "po" folds he takes his hole cards with him.
	Unfortunately these are not known to his opponents so they must
	put any cards they have assigned to "po" back in their card pool. }

var
	pn:playerIndex;
	i:integer;

begin
	for pn:=1 to MAX_PLAYERS do if (pn<>po) and the_players[pn].isPlaying then
		with predict[pn,po] do
			for i:=1 to h.n do if (not sure[po,i]) and (assigned[i]) then
				inc(the_card_pool[pn][CardPip(h.card[i]), CardSuit(h.card[i])]);
				{ 09-03-06 rcvd runtime error 201 here during debug run }
end;

procedure DrawEvalFoldAfterBy(pn:playerIndex);

	begin
		DrawEvalFoldBeforeBy(pn);
	end;

procedure StudEvalFoldBy(po:playerIndex);

	{ Evaluate what a FOLD by player "po" means to his opponents.

		When player "po" folds he takes his hole cards with him.
		Unfortunately these are not known to his opponents so they must
		put any cards they have assigned to "po" back in their card pool. }

	var
		pn:playerIndex;
		i:integer;
	begin
		{
		for pn:=1 to MAX_PLAYERS do if (pn<>po) and the_players[pn].isPlaying then
			with predict[pn,po] do
				for i:=1 to h.n do if (not sure[po,i]) and assigned[i] then begin
					with h.card[i] do inc(the_card_pool[pn][getPip,getSuit]);
				end;
		}
	end;

procedure EvaluateFoldBy(pn:playerIndex);
begin
	if PlayingDrawPoker then
		if BeforeTheDraw then
			DrawEvalFoldBeforeBy(pn)
		else
			DrawEvalFoldAfterBy(pn)
	else
		StudEvalFoldBy(pn);
end;

procedure PlayerFolded(po:playerIndex);

{	In Draw poker this means putting theirhand away into a waste pile that will
	be used again for the draw if needed. In Stud poker we leave their cards on
	the table but face down. }

var
	i:integer;
	aSpan:TRect;

begin
	{$ifndef ANAL}
	the_status_notes[po]^.Hide;
	{$endif}
	updatePlayerStatus(po,FOLDED);
	{$ifndef CHEAT}
	the_view^.hand_props[po].GetSpanRect(aSpan);
	if the_poker_options.game_id = DRAW_POKER
		then DiscardHand(po)
		else for i:=1 to theHands[po].n do begin
			sure[po, i]:= false;
			the_view^.hand_props[po].ThePile^.setAllFacedown;
		end;
	the_view^.RefreshRect(aSpan);
	ShowPlayerStatus(po);
	cardPlayDelay;
	{$endif}
	if (NPlayers=1) then
		PostMessage(frame_handle,WM_SHOWDOWN,0,0)
	else if Everyone(PASSED) or Everyone(Checked) then begin
		AllPassed;
		barbDeal^.Enable;
		SendMessage(frame_handle,WM_ENABLEDEALBUTTON,0,0);
	end
	else if BetsEqualized then
		EndBettingRound
	else if NoBodyOpenedYet then
		PostMessage(frame_handle,WM_NEXTOPEN,NextPlaying(po),0)
	else
		PostMessage(frame_handle,WM_NEXTBET,NextPlaying(po),0);
	EvaluateFoldBy(po);
end;

procedure PlayerCheck(po:playerIndex);

{ Execute a check for a player "po" (human or silicon). }

begin
	updatePlayerStatus(po,checked);
	delay(DELAY_ACTION);
end;

procedure DisplayTarget(adc:hDC;p:playerIndex);

{ Display a target marker at player position "p". }

begin
	the_status_notes[p]^.Hide;
end;

function NetAmt(P:playerIndex):integer;

begin
	NetAmt:=the_players[P].NetWinnings;
end;

procedure clearActiveStatus;

{ Visually erase all the active (non-FOLDED) player's status messages. }

var
	player:playerIndex;

begin
	for player:=1 to MAX_PLAYERS do with the_players[player] do
		if participating and (status<>FOLDED) then the_status_notes[player]^.Hide;
end;

function RemainingPlayer:integer;

	{ Return the player index (1..n) if this is the only player left in
		the round otherwise return 0. }

	var
		player:playerIndex;
	begin
		remainingPlayer:=0;
		for player:=1 to MAX_PLAYERS do if the_players[player].participating and everyoneExcept(player,FOLDED) then begin
			remainingPlayer:=player;
			exit;
		end;
	end;

function UserChoose:scoringType;

	{ Return the human players scoring choice. }

	begin
		{$ifndef VER1}
		with Application^ do case ExecDialog(New(PDialog,Init(MainWindow,'High_or_Low'))) of
			1:userChoose:=playHigh;
			2:userChoose:=playLow;
		end;
		{$else}
		userChoose:= playHigh;
		{$endif}
	end;

function playerChoose(po:playerIndex):scoringType;

	{ Return player "po"'s scoring choice. }

	begin
		{$ifndef VER1}
		with the_hand_valuations[po].eh do
			if rslt<ONE_PAIR then
				playerChoose:=playLow
			else
				playerChoose:=playHigh;
		{$else}
		playerChoose:= playHigh;
		{$endif}
	end;

type
	PWinnerDlg=^TWinnerDlg;
	TWinnerDlg=object(ODialog)
		constructor Init;
		function OnInitDialog:boolean; virtual;
		procedure SetupWindow; virtual;
	end;
	PWinnersDlg=^TWinnersDlg;
	TWinnersDlg=object(ODialog)
		constructor Init;
		function OnInitDialog:boolean; virtual;
		procedure SetupWindow; virtual;
	end;

var
	hWinList,lWinList:array[playerIndex] of boolean; { list of winners }
	WinnerAmt:array[playerIndex] of integer; { amount each winner gets from the pot }

constructor TWinnerDlg.Init;

begin
	inherited Construct(the_app^.Frame^.MyFrameWindow^.handle,904);
end;

constructor TWinnersDlg.Init;

begin
	inherited Construct(the_app^.Frame^.MyFrameWindow^.handle,905);
end;

type
	PDrawItemStruct=^TDrawItemStruct;

function TWinnerDlg.OnInitDialog:boolean;

begin
	SetupWindow;
	OnInitDialog:=inherited OnInitDialog;
end;

function TWinnersDlg.OnInitDialog:boolean;

begin
	SetupWindow;
	OnInitDialog:=inherited OnInitDialog;
end;

procedure TWinnerDlg.SetupWindow;
const
	WinnerTextID=102;
var
	pn:playerIndex;
	WinnerText:array[0..80] of Char;
begin
	CenterWindow(self.handle,frame_handle);
	{ setup the text for the winner(s) }
	for pn:=1 to MAX_PLAYERS do if (hWinList[pn] or lWinList[pn]) then begin
		StrPCopy(WinnerText,the_players[pn].GetNickName);
		StrCat(WinnerText,' ');
		StrCat(WinnerText,i2s(WinnerAmt[pn],@WinnerText[70]));
	end;
	windows.SetWindowText(GetDlgItem(WinnerTextId),WinnerText);
end;

procedure TWinnersDlg.SetupWindow;
const
	WinnerTextID=102;
var
	n:integer;
	pn:playerIndex;
	zs:array[0..80] of Char;
begin
	{ setup the text for the winners }
	n:=0;
	for pn:=1 to MAX_PLAYERS do if (hWinList[pn] or lWinList[pn]) then begin
		StrPCopy(zs,the_players[pn].GetNickName);
		StrCat(zs,' ');
		StrCat(zs,i2s(WinnerAmt[pn],@zs[70]));
		windows.SetWindowText(GetDlgItem(100*(n+1)+2),zs);
		{ high or low }
		StrCopy(zs,'(');
		StrPCopy(StrEnd(zs),scoringDesc[gngfr[pn].scrMd]);
		StrCat(zs,')');
		windows.SetWindowText(GetDlgItem(100*(n+1)+4),zs);
		Inc(n);
	end;
end;

procedure PresentWinnerDialog;

var
	aDialog:TWinnerDlg;

begin
	aDialog.Init;
	aDialog.Modal;
end;

procedure PresentWinnersDialog;

var
	aDialog:TWinnersDlg;

begin
	aDialog.Init;
	aDialog.Modal;
end;

procedure setGlobals;

{ Initialize the global scoring parameters for the current game setup. }

var
	s:TSuit;

begin
	case the_poker_options.game_id of
		DRAW_POKER:with theDrawPokerOptions do begin
			setWildCards(wType,wildCards);
			SetScoreMode(m_aScoringMode);
			case m_aScoringMode of
				playHigh
				{$ifndef VER1}
				,playHighLow
				{$endif}:
					SetEvaluationMode(highEval);
				{$ifndef VER1}
				playLow:
					setEvaluationMode(lowEval);
				{$endif}
			end;
			handSize:=5;
		end;
		STUD_POKER:with theStudPokerOptions do begin
			{$ifndef VER1}
			if GType=Baseball then begin
				wType:=WildUser;
				{ 3's and 9's are wild }
				ClearWildCards(WildCards);
				with wildCards do begin
					for s:=clubSuit to spadeSuit do wList[threePip,s]:=true;
					for s:=clubSuit to spadeSuit do wList[ninePip,s]:=true;
				end;
			end
			else
			{$endif}
				setWildCards(wType,wildCards);
			SetScoreMode(m_aScoringMode);
			case m_aScoringMode of
				playHigh
				{$ifndef VER1}
				,playHighLow
				{$endif}:
					setEvaluationMode(highEval);
				{$ifndef VER1}
				playLow:setEvaluationMode(lowEval);
				{$endif}
			end;
			case GType of
				stud5card:handSize:=5;
				stud6card:handSize:=6;
				stud7card
				{$ifndef VER1}
				,baseBall
				{$endif}:
					handSize:=7;
			end;
		end;
	end;
end;

function YouAreBrokeMessage(pn:playerIndex):string;
var
	s:string;
begin
	with the_players[pn] do begin
		if pn = USER_ID 
			then s:= 'The House will lend '
			else s:= 'Should the House lend ';
		if pn=USER_ID 
			then s:=s+'you'
			else if my_gender=MALE 
				then s:=s+'him'
				else s:=s+'her';
		s:=s+' another '+int2str(the_table_stakes)+' chips';
		s:=s+Q(pn=USER_ID,'.','?');
	end;
	YouAreBrokeMessage:=s;
end;

{$ifdef TEST}

procedure Test_YouAreBrokeMessage;
begin
	the_table_stakes:=25;
	AssertAreEqual('The House will lend you another 25 chips.',YouAreBrokeMessage(USER_ID));
	the_players[2].my_gender:=male;
	AssertAreEqual('Should the House lend him another 25 chips?',YouAreBrokeMessage(2));
end;

{$endif}

procedure CleanupPlayers;

{ see if any one is out of money and take the appropriate action }

var
	pn:playerIndex;
	t:string;
	at:array[0..50] of char;
	astext:array[0..50] of char;

begin
	for pn:=1 to MAX_PLAYERS do with the_players[pn] do
		if participating and (pileValue(myChips) = 0) then begin
			x_raspberry;
			if pn=USER_ID then
				t:='You are'
			else
				t:=getNickName + ' is';
			t:=t+' broke!';
			strpcopy(at,t);
			strpcopy(astext,YouAreBrokeMessage(pn));
			if pn = USER_ID then begin
				MessageBox(frame_handle, astext, at, MB_ICONINFORMATION or MB_OK);
				borrowMoney(pn,the_table_stakes)
			end
			else begin
				if MessageBox(frame_handle, astext, at, MB_ICONQUESTION or MB_YESNO) = IDYES 
					then borrowMoney(pn, the_table_stakes)
					else leaveGame(pn);
			end;
		end;
end;

procedure AbortCurrentRound;
var
	pi:playerIndex;
begin
	ClearHands;
	the_pot.Discard;
	for pi:=Low(playerIndex) to High(playerIndex) do
		if (the_players[pi].Participating) then the_players[pi].myBet.Discard;
	CleanupPlayers;
end;

procedure StartRound;
begin
	delay(DELAY_ACTION);
	RndHistory:='';
	{ check out if the theDealerIndex is still in the game, he may have been dropped out }
	{if not the_players[theDealerIndex].participating then theDealerIndex:=NextClockwiseFrom(theDealerIndex);}
	inc(roundCount);
	SetGlobals;
	if USER_ID=0 then begin
		Delay(200);
		windows.PostMessage(frame_handle,WM_DEALBUTTON,0,0);
	end
	else begin
		barbDeal^.Enable;
		SendMessage(frame_handle,WM_ENABLEDEALBUTTON,0,0); 
	end;
end;

procedure showDown(doHigh,doLow:boolean);

	{ Pick the winner or winners of the players still in the game with
		the highest 5-card poker hand if "doHigh" is true and/or with
		the lowest 5-card poker hand if "doLow" is true.

		When both "DoHigh" and "DoLow" are true each player must first
		declare whether they are going for high or low before the showdown.

		Note: all hands must be evaluated prior to calling this function. }

	var
		i:integer;
		PNH,PNL:integer; { player # with the highest hand so far }
		HandEH,HandEL:handType;
		BestEH,BestEL:PokerHandEvaluation;
		first,pn:playerIndex;
		s1,s2:string[40];
		adc:hDC;

	function nWinners:integer;

		{ Return the number of winners. }

		var
			pn:playerIndex;
			n:integer;

		begin
			n:=0;
			for pn:=1 to MAX_PLAYERS do if hWinList[pn] then inc(n);
			for pn:=1 to MAX_PLAYERS do if lWinList[pn] then inc(n);
			nWinners:=n;
		end;

	function LeftOver:integer;
		{ Return the number of chips left over after the winnings have been
			distributed amongst the winners. }
		var
			pn:playerIndex;
			n:integer;
		begin
			n:=0;
			for pn:=1 to MAX_PLAYERS do if hWinList[pn] then inc(n,WinnerAmt[pn]);
			for pn:=1 to MAX_PLAYERS do if lWinList[pn] then inc(n,WinnerAmt[pn]);
			LeftOver:=PotAmt-n;
		end;

	begin
		for pn:=1 to MAX_PLAYERS do hWinList[pn]:=false;
		for pn:=1 to MAX_PLAYERS do lWinList[pn]:=false;
		if (NPlayers>1) then begin
			PNH:=0;
			PNL:=0;
			if doHigh and doLow then begin
				first:=playerIndex(nextPlaying(theDealerIndex));
				pn:=first;
				repeat
					if pn=USER_ID then
						gngFr[pn].scrMd:=userChoose
					else if not GngFr[pn].Dcdd then
						gngFr[pn].scrMd:=playerChoose(pn);
					pn:=playerIndex(nextPlaying(pn));
				until pn=first;
			end;
			{ figure out who the winners are }
			first:=playerIndex(nextPlaying(theDealerIndex));
			pn:=first;
			repeat
			{$ifndef ANAL}
			{ expose the player's sorted hand }
			delay(DELAY_ACTION);
			the_players[pn].ExposeSortedHand;
			{ create and display the description of their hand }
			{$V-}
			if DoHigh and DoLow then
				if GngFr[pn].ScrMd=PlayHigh then begin
					makeHandDesc(s1,the_hand_valuations[pn].EH);
					dspPlrMsg(pn,s1+'(H)');
				end
				else begin
					makeHandDesc(s1,the_hand_valuations[pn].EL);
					dspPlrMsg(pn,s1+'(L)');
				end
			else begin
				makeHandDesc(s1,the_hand_valuations[pn].eh);
				dspPlrMsg(pn,s1);
			end;
			{$V+}
			{$endif}
			if gngfr[pn].scrMd=playHigh then begin
				if PNH=0 then begin
					BestEH:=the_hand_valuations[pn].EH;
					PNH:=pn;
					hWinList[PNH]:=true;
				end
				else begin
					SetEvaluationMode(HighEval);
					i:=cmpHands(the_hand_valuations[pn].eh,BestEH);
					if i=0 then begin
						PNH:=pn;
						hWinList[PNH]:=true;
					end
					else if (i>0) then begin
						BestEH:=the_hand_valuations[pn].EH;
						hWinList[PNH]:=False; { cancel the old winner }
						PNH:=pn;
						hWinList[PNH]:=true;
					end;
				end;
			end
			else begin
				if PNL=0 then begin
					if DoHigh and DoLow then
						BestEL:=the_hand_valuations[pn].EL
					else
						BestEL:=the_hand_valuations[pn].EH;
					PNL:=pn;
					lWinList[PNL]:=true;
				end
				{$ifndef VER1}
				else begin
					SetEvaluationMode(LowEval);
					if DoHigh and DoLow then
						i:=cmpHands(the_hand_valuations[pn].EL,BestEL)
					else
						i:=cmpHands(the_hand_valuations[pn].EH,BestEL);
					if i=0 then begin
						PNL:=pn;
						lWinList[PNL]:=true;
					end
					else if (i>0) then begin
						if DoHigh and DoLow then
							BestEL:=the_hand_valuations[pn].EL
						else
							BestEL:=the_hand_valuations[pn].EH;
						lWinList[PNL]:=False; { cancel the old winner }
						PNL:=pn;
						lWinList[PNL]:=True;
					end;
				end
				{$endif};
			end;
			pn:=playerIndex(nextPlaying(pn));
			until pn=first;
		end
		else begin
			PNH:= NextPlaying(theDealerIndex);
			HWinList[NextPlaying(theDealerIndex)]:=True;
		end;
		System.Assert(nWinners = 1, '# of winners must be 1');
		{ Divide up the pot amongst the winners. Any odd left over chips go to the player that last OPENED,bet or RAISED. }
		for pn:=1 to MAX_PLAYERS do if (hWinList[pn] or lWinList[pn]) then begin
			winnerAmt[pn]:=potAmt div nWinners;
			if pn=whoBetLast then
				winnerAmt[whoBetLast]:=winnerAmt[whoBetLast]+(potAmt mod nWinners);
		end;
		{ if there are still any left over chips then assign them to each winner around the table }
		i:=LeftOver;
		pn:=playerIndex(NextPlaying(theDealerIndex));
		while i > 0 do begin
			while (not hWinList[pn]) and (not lWinList[pn]) do PN:=playerIndex(NextPlaying(PN));
			Inc(winnerAmt[PN]);
			Dec(i);
		end;

		if nWinners>1 then PresentWinnersDialog else PresentWinnerDialog;

		{ Add the winnings to each winners pile of chips. }

		if nPlayers > 1 then begin
			pn:=playerIndex(theDealerIndex);
			repeat
				with the_players[pn] do if isPlaying then the_status_notes[pn]^.Hide;
				pn:=prevPlayerNo(pn);
			until pn=theDealerIndex;
		end;
		ClearHands;

		for pn:= 1 to MAX_PLAYERS do if the_players[pn].Participating then while the_players[pn].myBet.ChipCount>0 do begin
			the_players[pnh].myChips.AddChip(the_players[pn].myBet.Pop);
			delay(DELAY_ACTION div 3);
		end;
		while the_pot.ChipCount>0 do begin
			the_players[pnh].myChips.AddChip(the_pot.Pop);
			delay(DELAY_ACTION div 3);
		end;
		delay(DELAY_ACTION);
		AbortCurrentRound;
		the_app^.Frame^.MyFrameWindow^.SendMessage(WM_NEXTDEALER,theDealerIndex,0);
		if the_show_stats_flag then the_app^.Frame^.MyFrameWindow^.SendMessage(WM_COMMAND,CM_STATS,0);
		StartRound;
	end;

procedure initRaiseLmts;

var
	pn:playerIndex;

begin
	totRaiseCnt:=0;
	for pn:=1 to MAX_PLAYERS do begin
		plrRaiseCnt[pn]:=0;
	end;
end;

function JackpotsVar:boolean;
begin
	JackpotsVar:=(theDrawPokerOptions.GType=JackPots);
end;

procedure DrawEvalOpenBeforeBy(pn:playerIndex);

	{ In Draw poker evaluate what an open before the draw by player "pn"
		means to the other players. }

	var
		q:playerIndex;
		r:real;

	procedure evalOpenHigh(var e:pokerHandEvaluation);

		begin
			{ player "pn" could have a reasonably good pair or could be
				going to improve a short hand. }
			if JackpotsVar then
				predictHand(q,pn,e,ONE_PAIR,jackPip,TWO_PAIR,eightPip,[])
				{ since he OPENED in a game of JackPots he cannot possibly have
					a short hand }
				{???}
			else
				predictHand(q,pn,e,RUNT,jackPip,ONE_PAIR,acePip,[]);
			{???}
			{
			secProbs[noShortHand].result:=0.0;
			for g:=shortFlush to openEndStraight do
				secProbs[g].result:=secOdds[g]/(totalHands-secOdds[noShortHand]);
			}
		end;

	procedure evalOpenLow(var e:pokerHandEvaluation);

		begin
			predictHand(q,pn,e,ONE_PAIR,threePip,RUNT,acepip,[]);
		end;

	begin
		q:=playerIndex(nextPlaying(pn));
		repeat
			{ for each opponent "q" }
			with predict[q,pn] do case theDrawPokerOptions.m_aScoringMode of
				playHigh:evalOpenHigh(eval.eh);
				{$ifndef VER1}
				playLow:evalOpenLow(eval.eh);
				playHighLow:begin
					if DecidedHigh(isGngFr) then begin
						setEvaluationMode(highEval);
						evalOpenHigh(eval.eh);
					end;
					if DecidedLow(isGngFr) then begin
						setEvaluationMode(lowEval);
						evalOpenLow(eval.el);
					end;
					setEvaluationMode(highEval);
				end;
				{$endif}
			end;
			q:=playerIndex(nextPlaying(q));
		until q=pn;
	end;

procedure DrawEvalOpenAfterBy(po:playerIndex);

	{ In Draw poker evaluate what an open after the draw by player "po"
		means to the other players.

		It usually means he got what he was drawing for or at least stayed
		the same. }

	var
		pn:playerIndex;

	procedure evalBetHigh(var e:pokerHandEvaluation);

		begin
			case nCardsDrwn[po] of
				{ playing HIGH }
				1: { if he had a short hand before and is now betting then
					he could have completed his short hand to make a flush or
					straight }
					{ ??? }
					{ if he did not have a short hand then he could have completed
						a two pair to make a full house }
					predictHand(pn,po,e,TWO_PAIR,fivePip,TWO_PAIR,acePip,[]);
				2:{
					3K -> 3K,FH,4K,5K
					1P -> 1P,2P
					}
					predictHand(pn,po,e,ONE_PAIR,queenPip,ONE_PAIR,acePip,[]);
				3:
					predictHand(pn,po,e,ONE_PAIR,ninePip,TWO_PAIR,acePip,[]);
				4,5:
					predictHand(pn,po,e,ONE_PAIR,tenPip,ONE_PAIR,acePip,[]);
			end;
		end;

	procedure evalBetLow(var e:pokerHandEvaluation);

		begin
			case nCardsDrwn[po] of
				1..5:
					predictHand(pn,po,e,RUNT,acePip,RUNT,acePip,[]);
			end;
		end;

	begin
		for pn:=1 to MAX_PLAYERS do if (pn<>po) and the_players[pn].isPlaying then
			with predict[pn,po] do case theDrawPokerOptions.m_aScoringMode of
				playHigh:evalBetHigh(eval.eh);
				{$ifndef VER1}
				playLow:evalBetLow(eval.eh);
				playHighLow:begin
					if DecidedHigh(isGngFr) then begin
						setEvaluationMode(highEval);
						evalBetHigh(eval.eh);
					end;
					if DecidedLow(isGngFr) then begin
						setEvaluationMode(lowEval);
						evalBetLow(eval.el);
					end;
					setEvaluationMode(highEval);
				end;
				{$endif}
			end;
	end;

procedure CannotHave(N:integer;var IgnoreSet:PokerHandTypeSet);
	{ Initialize "IgnoreSet" to the set hands that are impossible for
		an "N" card hand. }
	begin
		case N of
			0:IgnoreSet:=[RUNT..FIVE_KIND];
			1:IgnoreSet:=[ONE_PAIR..FIVE_KIND];
			2:IgnoreSet:=[TWO_PAIR..FIVE_KIND];
			3:IgnoreSet:=[TWO_PAIR,Straight..FIVE_KIND];
			4:IgnoreSet:=[Straight,Flush,FULLHOUSE,STRAIGHT_FLUSH,FIVE_KIND];
			else
				IgnoreSet:=[];
		end;
	end;

var
	UpCardsEval:array[playerIndex] of pkrResultType;

procedure BestOOB(po:playerIndex;var E:pokerHandEvaluation);

	{ Return the best hand "E" showing on the board not including player "po"'s hand. }

	var
		PN:playerIndex;
		EE:PokerHandEvaluation;

	procedure GetEE;
		begin
			{$ifndef VER1}
			if (GetScoreMode = PlayHighLow) then
				if GetEvaluationMode=HighEval then
					EE:=UpCardsEval[PN].EH
				else
					EE:=UpCardsEval[PN].EL
			else
			{$endif}
				EE:=UpCardsEval[PN].EH;
		end;

	begin
		PN:=playerIndex(nextPlaying(PO));
		GetEE;
		E:=EE;
		repeat
			if cmpHands(EE,E)>0 then E:=EE;
			PN:=playerIndex(nextPlaying(Integer(PN)));
			GetEE;
		until PN=PO;
	end;

procedure StudEvalCall(po:playerIndex);

	{ Evaluate what a CALL by player "po" means to his opponents that
		are still in the game.

		When a player CALLS before the final round it means he either has
		or hopes to get a winning hand. When he calls on the final round
		then he believes he has the BEST hand. }

	var
		e2:pkrResultType;
		e,e1:pokerHandEvaluation;
		IgnoreSet:PokerHandTypeSet; { hands to ignore when predicting
			based on the number of cards in the hand }
		SaveAssign:AssignedArray;

	procedure EvalHigh(L,E:PokerHandEvaluation);
		begin
			with L do predictHand(
				opntNo,callerno,E,
				Rslt,Pip,
				NextPokerHand(Rslt),Pip,
				IgnoreSet);
		end;

	procedure EvalLow(L,E:PokerHandEvaluation);
		begin
			with L do predictHand(
				opntNo,callerno,E,
				Rslt,Pip,
				RUNT,AcePip,
				IgnoreSet);
		end;

	begin
		callerNo:=po; { need to get at it globally }
		opntNo:=playerIndex(nextPlaying(Integer(callerNo)));
		{ don't do an the_hand_valuations if this is the bettor and the last round
			since there is no point in evaluating anymore before the showdown }
		if not ((opntNo=whoLastBet) and (studRndNo=lastRnd)) then repeat
			if StudRndNo<LastRnd then with predict[opntNo,callerNo] do begin
				CannotHave(H.N,IgnoreSet);
				case getScoreMode of
					playHigh:EvalHigh(UpCardsEval[CallerNo].EH,Eval.EH);
					{$ifndef VER1}
					PlayLow:EvalLow(UpCardsEval[CallerNo].EH,Eval.EH);
					playHighLow:begin
						SaveAssign:=Assigned;
						SetEvaluationMode(HighEval);
						EvalHigh(UpCardsEval[CallerNo].EH,Eval.EH);
						Assigned:=SaveAssign;
						SetEvaluationMode(LowEval);
						EvalLow(UpCardsEval[CallerNo].EL,Eval.EL);
					end;
					{$endif}
				end;
			end
			{ On the last round anyone that calls must at least beat their
				opponents up cards. }
			else with predict[opntNo,callerNo] do begin
				{ a player that calls on the final round is never bluffing }
				predict[opntNo,callerNo].isBlffng:=false;
				BestOOB(opntNo,e);
				case getScoreMode of
					playHigh
					{$ifndef VER1}
					,playLow
					{$endif}:
						predictHand(opntNo,callerno,eval.eh,e.rslt,e.Pip,nextPokerHand(e.Rslt),acePip,[]);
					{$ifndef VER1}
					playHighLow:predictHand(opntNo,callerno,eval.eh,e.rslt,e.pip,RUNT,acePip,[]); { ??? }
					{$endif}
				end;
			end;
			opntNo:=playerIndex(nextPlaying(Integer(opntNo)));
		until opntNo=callerNo;
	end;

procedure StudEvalOpenBy(po:playerIndex);

	{ Evaluate what a BET by player "po" means to his opponents.

		When a player opens the betting on a round it lets his opponents
		make a prediction about his hole cards.

		The exception to this is on the first betting round when a player
		with the highest showing hand must open. }

	begin
		StudEvalCall(po);
	end;

procedure EvaluateOpenBy(pn:playerIndex);
{ Evaluate what an open by player "pn" means to the other players. }
begin
	if PlayingDrawPoker then
		if BeforeTheDraw then
			DrawEvalOpenBeforeBy(pn)
		else
			DrawEvalOpenAfterBy(pn)
	else
		StudEvalOpenBy(pn);
end;

procedure DoPlayerOpen(pn:playerIndex);
begin
	ClearStatus:=true;
	UpdatePlayerStatus(pn,OPENED);
	if pn=USER_ID
		then ShowPlayerStatus(pn)
		else UpdatePot(pn,the_current_bet);
	theBets[pn]:=the_current_bet;
	WhoOpened:=pn;
	whoLastBet:=whoOpened;
	InitRaiseLmts;
	EvaluateOpenBy(pn);
end;

procedure PlayerOpened(pn:playerIndex);
begin
	DoPlayerOpen(pn);
	PostMessage(frame_handle,WM_NEXTBET,NextPlaying(WhoLastbet),0);
	delay(DELAY_ACTION);
end;

procedure PlayerOpen(pn:playerIndex;amount:integer);
begin //writeln('PlayerOpen(',pn,',',amount,')');
	the_current_bet:=amount;
	DoPlayerOpen(pn);
end;

procedure DoPlayerCall(pn:playerIndex);
begin
	UpdatePlayerStatus(pn,called);
	UpdatePot(pn,the_current_bet-Round(the_players[pn].myBet.Value));
	theBets[pn]:=the_current_bet;
end;

procedure PlayerCalled(pn:playerIndex);
{ Process a Call for player # "pn". }
begin
	DoPlayerCall(pn);
	delay(DELAY_ACTION);
	if BetsEqualized
		then EndBettingRound
		else PostMessage(frame_handle,WM_NEXTBET,NextPlaying(pn),0);
end;

procedure PlayerCall(pn:playerIndex);
begin
	DoPlayerCall(pn);
end;

procedure PlayerPassed(p:playerIndex);
	var
		ss:PlayerStatus;
	begin
		if PassIsCheck then
			ss:=Checked
		else
			ss:=PASSED;
		UpdatePlayerStatus(p,ss);
		if Everyone(ss) then begin
			AllPassed;
		end
		else begin
			{$ifndef ANAL} delay(DELAY_ACTION); {$endif}
			PostMessage(frame_handle,WM_NEXTOPEN,NextPlaying(p),0);
		end;
	end;

function PlayerHistory(po:playerIndex;s:playerStatus):boolean;
	{ Return true if player "po"'s history contains status "s". }
	begin
		with the_players[po] do
			playerHistory:=(pos(int2str(po)+':'+statusCode[s],rndHistory)>0);
	end;

function unAssigned(p1,p2:playerIndex):integer;

	{ Return the number of unassigned predicted cards there are in
		player "p1"'s prediction for player "p2". }

	var
		i,n:integer;

	begin
		n:=0;
		for i:=1 to theHands[p2].n do if not predict[p1,p2].assigned[i] then inc(n);
		unAssigned:=n;
	end;

procedure makePredHand(po,px:playerIndex;var hnd:handType);

	{ Makeup a hand of all known and assigned cards that player "po"
		predicts for player "px". }

	var

		i:integer;

	begin

		with predict[po,px] do begin
			hnd.n:=0;
			for i:=1 to h.n do
				if assigned[i] then begin
					inc(hnd.n);
					hnd.card[hnd.n]:=h.card[i];
				end;
		end;

	end;

procedure predictHand(
	po,px:playerIndex;
	var e:pokerHandEvaluation; { put the result in here }
	lowResult:handrank;lowPip:TPip;
	highResult:handrank;highPip:TPip;
	ignore:pokerHandTypeSet { ignore these hand type }
	);

	{ Create player "po"'s prediction of player "px"'s hand so that it
		is somewhere between "lowResult" of "lowPip" and "highResult" of
		"highPip", ignoring any in the "ignore" set, and put the result
		in "e".

		Use the current setting of the the_hand_valuations mode to determine when
		we have a good enough hand.

		If "po" cannot find cards from its card pool that can give "px"
		a good enough hand then leave it unassigned for now. ??? }

	var

		ui,i,j,sCnt:integer;
		p:TPip;
		s:TSuit;
		nCrdsLft:integer; { how many cards left in "po"'s card pool }
		tempDeck:deckArray;
		maxN:integer;
		hnd:handType;

	function goodEnough:boolean;

		{ Return true if the hand result "e" is within the parameters. }

		begin

			goodEnough:=
				(not (e.rslt in ignore))
				and
				(
					((e.rslt>lowResult) and (e.rslt<highResult))
					or
					(
						(lowResult=highResult)
						and
						(e.rslt=lowResult)
						and
						(rank(e.pip)>=rank(lowPip))
						and
						(rank(e.pip)<=rank(highPip))
					)
					or
					(
						(lowResult<>highResult)
						and
						(
							((e.rslt=lowResult) and (rank(e.pip)>=rank(lowPip)))
							or
							((e.rslt=highResult) and (rank(e.pip)<=rank(highPip)))
						)
					)
				)

		end;

	begin
		ui:=unAssigned(po,px);
		if ui=0 then begin
			{ if "po"'s current prediction of "px"'s hand is already good enough then leave it alone }

			if goodEnough then begin
				exit;
			end;
			{ unassign the ones I assigned before and put them back in my card pool then continue }
			with predict[po,px] do
				for i:=1 to h.n do if not sure[px,i] then begin
					assigned[i]:=false;
					inc(the_card_pool[po][CardPip(h.card[i]), CardSuit(h.card[i])]);
					{ ??? got a range check error here in 7stud with 2;'s wild }
					{ 05-21-06 got a range check error here in Draw Anything opens }
				end;
		end;
		{ make up the temporary deck "tempDeck" from "po"'s current card pool }
		nCrdsLft:=0;
		for p:=acePip to kingPip do
			for s:=clubSuit to spadeSuit do
				if the_card_pool[po][p,s]>0 then begin
					inc(nCrdsLft);
					tempDeck[nCrdsLft]:= MakeCard(p,s);
				end;

		if the_card_pool[po][jokerPip,spadeSuit]>0 then begin
			inc(nCrdsLft);
			tempDeck[nCrdsLft]:= MakeCard(jokerPip,spadeSuit);
		end;
		{ calculate the maximum number of cards we will need to generate
			the highest prediction ("highResult") }
		if the_poker_options.game_id=STUD_POKER then
			maxN:=predict[po,px].h.n
		else
			maxN:=5;
		{ now pick from the combinations until we can make a hand for
			"px" that is good enough }
		sCnt:=1; { sample counter }
		repeat
			shuffleDeck(tempDeck, ncrdslft, 2);
			{ make up the test hand }
			j:=0;
		hnd.n:=0;
			for i:=1 to maxN do begin
			inc(hnd.n);
				if sure[px,i] then
					hnd.card[hnd.n]:= theHands[px].card[i]
		  else begin
			inc(j);
					hnd.card[i]:=tempDeck[j];
				end;
			end;
			if hnd.n<6 then
				evaluate(hnd,e)
			else
				evaluateBest(hnd,5,e);
			inc(sCnt);
		until goodEnough or (sCnt=maxSampleN);
		if goodEnough then with predict[po,px] do begin
			{ adjust "po"'s the_card_pool for the ones we used }
			for i:=1 to Hnd.N do if (not sure[px,i]) then begin
				assigned[i]:=true;
				dec(the_card_pool[po][CardPip(hnd.card[i]), CardSuit(hnd.card[i])]); { ???
				got another range check error here in baseball }
				assignedTo[po][CardPip(hnd.card[i]), CardSuit(hnd.card[i])]:=px;
				h.card[i]:=hnd.card[i];
			end;
		end
		else begin
			{ Couldn't make up anything within the given range. ??? }
		end;
	end;

procedure improveHand(po,px:playerIndex;var e:pokerHandEvaluation);

	{ The purpose of this function is to improve "po"'s prediction of
		"px"'s hand.

		If "nr"=0 then keep it in the same hand type but improve the the_hand_valuations "e" up a 'few' notches so as to improve
		it using cards from the "pool". }

	var

		r:handrank;

	function incHand:handrank;

		begin

			if getEvaluationMode=highEval then begin
				if (rank(e.pip)=rank(acePip)) and (e.rslt<maxHandType) then
					incHand:=succ(e.rslt)
				else
					incHand:=e.rslt;
			end
			else begin
				if (rank(e.pip)=rank(acePip)) and (e.rslt>RUNT) then
					incHand:=pred(e.rslt)
				else
					incHand:=e.rslt;
			end;

		end;

	function incPip:TPip;

		begin

			if getEvaluationMode=highEval then begin
				if (E.Pip=AcePip) then
					if (E.Rslt<maxHandType) then
						IncPip:=TwoPip
					else
						incPip:=E.Pip
				else if E.Pip=KingPip then
					IncPip:=AcePip
				else
					IncPip:=Succ(E.Pip);
			end
			else begin
				if E.Pip=AcePip then
					if (E.Rslt>RUNT) then
						incPip:=KingPip
					else
						IncPip:=E.Pip
				else
					incPip:=Pred(E.Pip);
			end;

		end;

	begin

		r:=incHand;
		predictHand(po,px,e,r,incPip,nextPokerHand(r),acePip,[]);

	end;

procedure doPokerRaiseLimits(x,y:integer;var rlms:pokerRaiseLimits);

	{ Execute the common raise limits panel. }

	begin
	end;

{$ifdef LOG}

procedure logPlayingCard(pc:playingCard);

	begin

		case pc.getPip of
			acePip:write('A');
			twoPip:write('2');
			threePip:write('3');
			fourPip:write('4');
			fivePip:write('5');
			sixPip:write('6');
			sevenPip:write('7');
			eightPip:write('8');
			ninePip:write('9');
			tenPip:write('T');
			jackPip:write('J');
			queenPip:write('Q');
			kingPip:write('K');
		jokerPip:write('$');
		end;
		case pc.getsuit of
			clubSuit:write('c');
			heartSuit:write('h');
			diamondSuit:write('d');
			spadeSuit:write('s');
		end;

	end;

procedure logEvaluation(e:pokerHandEvaluation);

	var

	i:integer;

	begin

		write(pokerHandDesc[e.rslt],'(');
		write(playingCardPipChr[e.pip],') ');
	 with e do begin
		with set1 do for i:=1 to set1.n do begin
			logPlayingCard(wild.card[idxs[i]]);
		  write(' ');
			end;
			if set1.n<5 then for i:=1 to min(5-set1.n,set2.n) do with set2 do begin
				logPlayingCard(wild.card[idxs[i]]);
				write(' ');
			end;
		end;

	end;

procedure logHand(hand:handType);

	var

		i:integer;

  begin

		for i:=1 to hand.n do begin

		write(' ');
		{if not sure[player,i] then write('(');}
			logPlayingCard(hand.card[i]);
		{if not sure[player,i] then write(')');}
	 end;

	end;

procedure logPlayer(player:playerIndex);

	begin

		with the_players[player] do begin
			write('#',pno,' ',getNickName:9);
			if player=theDealerIndex then write(' D') else write('  ');
			if goforblf then write('!') else write('');
		write(' ');
		logHand(hand[player]);
		write(' ');
	 end;

  end;

procedure logInfo(player:playerIndex);

	var

		i:integer;
		s1,s2:string;

	begin

		with the_players[player] do begin
			if getScoreMode=playHighLow then begin
				if gngFr[player].dcdd then case gngFr[player].scrMd of
					playHigh:begin
						logPlayer(player);
						logEvaluation(the_hand_valuations[player].eh);
						write('(H) ');
					end;
					playLow:begin
						logPlayer(player);
						logEvaluation(the_hand_valuations[player].el);
						write('(L) ');
					end;
					playHighLow:begin
						logPlayer(player);
						logEvaluation(the_hand_valuations[player].eh);
						writeln('(H) ');
						logPlayer(player);
						logEvaluation(the_hand_valuations[player].el);
						write('(L) ');
					end;
				end
				else begin
					logPlayer(player);
					logEvaluation(the_hand_valuations[player].eh);
					writeln('(H) ');
					logPlayer(player);
					logEvaluation(the_hand_valuations[player].el);
					write('(L) ');
				end;
			end
			else begin
				logPlayer(player);
				logEvaluation(the_hand_valuations[player].eh);
			end;
			write(' {',PotAmt,'} ');
		end;

	end;

{$endif}

procedure BestPredict(PO:playerIndex;var E:PokerHandEvaluation);

	{ Return "PO"'s best prediction so far of his opponents.

		There must be more than one player left to call this procedure. }

	var

		PN:playerIndex;
		F:boolean;

	begin

		F:=True; { haven't got won yet }
		for PN:=1 to MAX_PLAYERS do if (PN<>PO) and (the_players[PN].IsPlaying) then with Predict[PO,PN] do
			if F then begin
				E:=Eval.EH;
				F:=False;
			end
			else if CmpHands(Eval.EH,E)>0 then
				E:=Eval.EH;

	end;

function AnythingOpensVar:boolean;

begin
	AnythingOpensVar:=(theDrawPokerOptions.GType=ANYTHINGOPENS);
end;

function oddsToWin(po:playerIndex):real;

	{ Return player "po"'s odds of winning with what he has and what
		he predicts his opponents have assuming there are no more cards
		to come (i.e the final betting round). }

  var

		pn:playerIndex;
		CH,CL:integer;
		BestPrediction:PokerHandEvaluation;

	begin
		CH:=0; { counter for opponents going for high }
		CL:=0; { counter for opponents going for low }
		{$ifndef VER1}
		if getScoreMode=playHighLow then begin
			for pn:=1 to MAX_PLAYERS do if (pn<>po) and (the_players[pn].isPlaying) then with predict[po,pn] do begin
				if DecidedHigh(GngFr[po]) and DecidedHigh(IsGngFr) then begin
					Inc(CH);
					SetEvaluationMode(HighEval);
					if (cmpHands(Eval.EH,the_hand_valuations[po].eh)>0) then begin
						OddsToWin:=0.0;
						Exit;
					end;
				end;
				if DecidedLow(GngFr[po]) and DecidedLow(IsGngFr) then begin
					Inc(CL);
					SetEvaluationMode(LowEval);
					if (cmpHands(Eval.EH,the_hand_valuations[po].el)>0) then begin
						OddsToWin:=0.0;
						Exit;
					end;
				end;
			end;
			if not GngFr[po].Dcdd then with GngFr[po] do
				if (CH=0) then begin
					Dcdd:=True;
					ScrMd:=PlayHigh;
				end
				else if (CL=0) then begin
					Dcdd:=True;
					ScrMd:=PlayLow;
				end;
			OddsToWin:=0.5+RandomReal/2;
		end
		else begin
			{$endif}
			BestPredict(po,BestPrediction);
			if cmpHands(BestPrediction,the_hand_valuations[PO].EH)>0 then begin

				{ At this point "po" predicts that the best opponent's minimum
					hand is better than his. If we are not playing JackPots then
					use the player's "GoForIt" factor to decide if he should stay
					in or not. }

				if
					(
						JackpotsVar
						and
						(BestPrediction.Rslt=the_hand_valuations[po].EH.Rslt)
						and
						(the_hand_valuations[PO].EH.Rslt>ONE_PAIR)
					)
					or
					(
						(not JackpotsVar)
						and
						(BestPrediction.Rslt=the_hand_valuations[po].EH.Rslt)
					)
				then begin
					if RandomReal<the_players[po].goForIt then
						oddsToWin:=1.0
					else
						oddsToWin:=0.0;
				end
				else
					oddsToWin:=0.0;
				exit;
			end
			else OddsToWin:=0.5+RandomReal/2;
		{$ifndef VER1}
		end;
		{$endif}
	end;

procedure DrawEvalCheckBy(po:playerIndex);
	{ Evaluate what a CHECK after the draw by player "po" means to the
		other players. It usually means the player didn't get what he was
		drawing for. }
begin
end;

procedure DrawEvalPassBy(po:playerIndex);

	{ This procedure is called after player "po" (human or silicon)
		elects to PASS (note that a PASS is not allowed when playing
		High/Low).

		It gives "po"'s opponents a chance to evaluate what they think
		he holds as a result of his pass. }

	var
		i:integer;
		pn:playerIndex;
		h:handrank;
	begin
		{ When player "po" passes he tells his opponents something about his
			hand. What the opponents infer from this depends on their knowledge
			of the game and what cards they know are remaining. }
		pn:=playerIndex(nextPlaying(po));
		repeat
			{ for each opponent that is still in the game adjust their
				prediction of player "po"'s hand }
			with predict[pn,po] do case theDrawPokerOptions.m_aScoringMode of
				playHigh:predictHand(pn,po,Eval.EH,RUNT,twoPip,ONE_PAIR,tenPip,[]);
				{$ifndef VER1}
				playLow:predictHand(pn,po,Eval.EH,FIVE_KIND,kingPip,ONE_PAIR,jackPip,[]);
				{$endif}
			end;
			{ next opponent }
			pn:=playerIndex(nextPlaying(pn));
		until pn=po;
	end;

procedure StudEvalPassBy(po:playerIndex);

	{ Evaluate what a CHECK by player "po" means to his opponents. }

	begin
	end;

procedure EvaluatePassBy(pn:playerIndex);

	{ This procedure is called after player "pn" (human or silicon)
		elects to PASS (note that a PASS is not allowed when playing
		High/Low).

		It gives "po"'s opponents a chance to evaluate what they think
		he holds as a result of his pass. }

	begin
		if PlayingDrawPoker then
			if BeforeTheDraw then
				DrawEvalPassBy(pn)
			else
				DrawEvalCheckBy(pn)
		else
			StudEvalPassBy(pn);
	end;

function isShort(po:playerIndex):boolean;

{ Return TRUE if "po"'s hand is a Short Hand. }

begin
	isShort:=
		UseShortHands
		and
		(the_hand_valuations[po].eh.rslt<=ONE_PAIR)
		and
		(the_short_hand_valuations[po].rslt in [flush,straight,STRAIGHT_FLUSH])
		and
		(the_short_hand_valuations[po].set1.n=4);
end;

procedure HoldHigh(var e:pokerHandEvaluation);
	{ Given the HIGH poker hand the_hand_valuations "e" adjust "set1" and "set2"
		of "e" to reflect what a player will most likely want to keep and
		discard for the draw. }
	begin
		with e do case rslt of
			ONE_PAIR,THREE_KIND,FOUR_KIND:if (pip2=acePip) and (random(100)>10) then with set1 do begin
				inc(n);
				idxs[n]:=set2.idxs[1];
				OtherIdx(set1,5,set2);
			end;
		end
	end;

procedure HoldLow(var e:pokerHandEvaluation);

	{ Given the LOW poker hand the_hand_valuations "e" adjust "set1" and "set2"
		of "e" to reflect what a player will most likely want to keep
		and discard for the draw. }

	begin
		with e do begin
			case rslt of
				RUNT:begin
					if rank(pip)<rank(acePip) then dec(set2.n);
					with set2 do begin
						inc(n);
						idxs[n]:=set1.idxs[1];
					end;
					set1:=set2;
				end;
				ONE_PAIR:begin
					with set2 do begin
						inc(n);
						idxs[n]:=set1.idxs[1];
					end;
					set1:=set2;
				end;
				TWO_PAIR:begin
					with set2 do begin
						inc(n);
						idxs[n]:=set1.idxs[1];
						inc(n);
						idxs[n]:=set1.idxs[3];
					end;
					set1:=set2;
				end;
				THREE_KIND:begin
					with set2 do begin
						inc(n);
						idxs[n]:=set1.idxs[1];
					end;
					set1:=set2;
				end;
				straight,flush,STRAIGHT_FLUSH:begin
					with set1 do dec(n);
				end;
				FULLHOUSE:begin
					with set2 do begin
						inc(n);
						idxs[n]:=set1.idxs[1];
						inc(n);
						idxs[n]:=set1.idxs[4];
					end;
					set1:=set2;
				end;
				FOUR_KIND:begin
					with set2 do begin
						inc(n);
						idxs[n]:=set1.idxs[1];
					end;
					set1:=set2;
				end;
			end; { case }
			otherIdx(set1,5,set2);
		end;
	end;

function oddsOfWinDraw(po:playerIndex;eee:pokerHandEvaluation):real;

	{ Return player "po"'s odds of winning before the draw with hand
		the_hand_valuations "eee" and his current predictions of his opponents hands. }

	var

		o,subo:real;
		tempDeck:deckArray;
		shortHands,testHands:array[playerIndex] of handType;
		pn:playerIndex;
		sc,sd,i,j,k,plrCnt:integer;
		p:TPip;
		s:TSuit;
		tempHand:handType;
		myResult,myShortResult,tempResult,e:pokerHandEvaluation;
		nCrdsLft:integer;
		stop:boolean;

	begin

		o:=0.0;

		{ make the temporary deck "tempDeck" from "po"'s card pool. }

		nCrdsLft:=0;
		for p:=acePip to kingPip do
			for s:=clubSuit to spadeSuit do
				if the_card_pool[po][p,s]>0 then begin
					inc(nCrdsLft);
					tempDeck[nCrdsLft]:= makecard(p,s);
				end;

		if the_card_pool[po][jokerPip,spadeSuit]>0 then begin
			inc(nCrdsLft);
			tempDeck[nCrdsLft]:= makecard(jokerPip,spadeSuit);
		end;

	 { Iterate thru this player's sampling depth. Adjust the player's
			absolute sampling depth randomly to somewhere between 50%..100%. }

		sd:=(the_players[po].smplDpth div 2)+random(the_players[po].smplDpth div 2)+1;
		for sc:=1 to sd do begin
			shuffleDeck(tempDeck, nCrdsLft, 2);

			{ from the shuffled card pool assign cards to each opponent according
				to what they are most likely to draw based on "po"'s prediction
				of their hand }

			j:=1;
			for pn:=1 to MAX_PLAYERS do begin
				testHands[pn].n:=0;
				shortHands[pn].n:=0;

				if (the_players[pn].isPlaying) then begin

					if pn=po then with the_players[po] do begin
				e:=eee;
						if not IsShort(pn) then case getEvaluationMode of
							highEval:holdHigh(e);
							{$ifndef VER1}
							lowEval:holdLow(e);
							{$endif}
						end;
					end
					else with predict[po,pn] do begin
						{$ifndef VER1}
						if GetScoreMode=PlayHighLow then
							if DecidedHigh(IsGngFr) then
								e:=Eval.EH
							else if DecidedLow(IsGngFr) then
								e:=Eval.EL
							else
								E:=Eval.EH
						else
						{$endif}
							e:=eval.eh;
						if not isShort(pn) then case getEvaluationMode of
							highEval:holdHigh(e);
							{$ifndef VER1}
							lowEval:holdLow(e);
							{$endif}
						end;
					end;

					{ copy the cards we are going to keep into the test hand }

					with testHands[pn] do begin
						for k:=1 to e.set1.n do begin
							inc(n);
							card[n]:= theHands[po].card[e.set1.idxs[k]];
						end;

						{ now fill up the hand with cards from the card pool to simulate
							a draw }

						for k:=1 to 5-n do begin
							inc(n);
							card[n]:=tempDeck[j];
					if j<nCrdsLft then
								inc(j)
							else
						j:=1;
						end;

			  with shortHands[pn] do if n>0 then
							for k:=1 to 5-n do begin
								inc(n);
								card[n]:=tempDeck[j];
						if j<nCrdsLft then
									inc(j)
								else
						j:=1;
							end;
					end;
				end;
		end;

			{ now test "po"s potential hand against all his opponents
				potential hands }

			subo:=1.0; { assume I win it }
			evaluate(testHands[po],myResult);
			if shortHands[po].n>0 then evaluate(shortHands[po],myShortResult);
			i:=1;
			stop:=false;
			while (not stop) and (i<=MAX_PLAYERS) do begin
				if (i<>po) and (testHands[i].n>0) then begin
					evaluate(testHands[i],tempResult);
				if shortHands[po].n>0 then begin
						if (cmpHands(tempResult,myResult)>0) and (cmpHands(tempResult,myShortResult)>0) then begin
							subo:=0.0;
							stop:=true;
						end;
			  end
					else if cmpHands(tempResult,myResult)>0 then begin
						subo:=0.0;
						stop:=true;
					end;
				end;
				inc(i);
			end;
			o:=o+subo;
		end;
		oddsOfWinDraw:=o/sd;
	end;

function RaiseIsOk(aPlayerId:playerIndex;rlms:pokerRaiseLimits):boolean;

begin
	raiseIsOK:=(PileValue(the_players[aPlayerId].myChips)>0);
{		and
		(totRaiseCnt<rlms.lmtPerRnd)
		and
		(plrRaiseCnt[aPlayerId]<rlms.lmtPerPsn);
}
end;

function ShouldStay1(po:playerIndex):boolean;

{ Return true if player "po" should stay in the first betting round of Draw Poker.
	Also set the "shouldRaise" variable if he should raise. }

const
	Delta=0.05;

var
	o,ol:real;
	aResult:boolean;
	h:pokerHandEvaluation;

begin
	shouldRaise:=false;
	{$ifndef VER1}
	if theDrawPokerOptions.m_aScoringMode = playHighLow then begin
		setEvaluationMode(HighEval);
		o:=oddsOfWinDraw(po,the_hand_valuations[po].eh);
		setEvaluationMode(LowEval);
		ol:=oddsOfWinDraw(po,the_hand_valuations[po].el);
		if O>OL+Delta then with gngFr[po] do begin
			dcdd:=true;
			scrMd:=playHigh;
		end
		else if (OL>O+Delta) then with gngFr[po] do begin
			dcdd:=true;
			scrMd:=playLow;
			o:=ol;
		end
		else
			o:=1.0;
	end
	else
	{$endif}
		o:= oddsOfWinDraw(po,the_hand_valuations[po].eh);

	if o=0.0 then o:=1/infinity;
	with the_players[po] do begin
		if (skill>=basicBetLevel) then
			aResult:=((potOdds(the_current_bet-theBets[po])+potOdds(the_current_bet-theBets[po])*potOddsFactor)>=1/o)
		else
			aResult:=(o>=winLimit);
		if
			aResult
			and
			raiseIsOk(po,theDrawPokerOptions.raiseLmts)
			and (potOdds(the_current_bet-theBets[po]+theDrawPokerOptions.afterDrawOpen)>=1/o)
			then shouldRaise:=False{RandomReal>0.9};
		if GoForBlf then aResult:=True;
	end;
	shouldStay1:=aResult;
end;

function beatOnBoard(po:playerIndex):boolean;

{ Return true if player "po" is beaten on the board. }

var
	E,EH,EL:pokerHandEvaluation;

begin
	{$ifndef VER1}
	if GetScoreMode = PlayHighLow then begin
		SetEvaluationMode(HighEval);
		BestOOB(po,EH);
		SetEvaluationMode(LowEval);
		BestOOB(po,EL);
		BeatOnBoard:=(cmpHands(EH,the_hand_valuations[po].eh)>0) and (cmpHands(EL,the_hand_valuations[po].EL)>0);
	end
	else begin
	{$endif}
		BestOOB(po,e);
		BeatOnBoard:=(cmpHands(e,the_hand_valuations[po].eh)>0);
	{$ifndef VER1}
	end;
	{$endif}
end;

procedure studPokerResult(h:handType;var e:pkrResultType);

begin
	if h.n<6 then
		PokerResult(h,e)
	else
		ExtPokerResult(h,5,e);
end;

function StudOddsOfWin(po:playerIndex):real;

	{ Return player "po"'s odds of winning (0.0 to 1.0) with his current hand and predictions. }

	var
		o:real;
		pn:playerIndex;
		tempHand:handType;
		MResult,TResult:pkrResultType;
		i,j,k,plrCnt:integer;
		p:TPip;
		s:TSuit;
		smplCnt:integer;
		tempDeck:deckArray;
		nCrdsLft:integer;
		THands:array[playerIndex] of handType; { potential test hands for
			each player, made up from their known cards, predicted cards and
			cards still to come. }
		Beat:boolean;

	begin
		if studRndNo < lastRnd then begin

			{ calculate "po"s odds based on another card to come }

			o:= 0.0;

			{ make up the temporary deck "tempDeck" from the cards remaining
				in "po"'s pool }

			nCrdsLft:= 0;
			for p:=acePip to kingPip do
				for s:=clubSuit to spadeSuit do
					if the_card_pool[po][p,s]>0 then begin
						inc(nCrdsLft);
						tempDeck[nCrdsLft]:= makecard(p,s);
					end;
			if the_card_pool[po][jokerPip,spadeSuit]>0 then begin
				inc(nCrdsLft);
				tempDeck[nCrdsLft]:= makecard(jokerPip,spadeSuit);
			end;

			for i:=1 to the_players[po].smplDpth do begin
				shuffleDeck(tempDeck, nCrdsLft, 2);

				{ create the array of test hands for each player still in the game }
				for pn:=1 to MAX_PLAYERS do if the_players[pn].isPlaying then begin
					THands[pn].n:=0;
					if pn=po then
						THands[pn]:= theHands[po]
					else
						makePredHand(po,pn,THands[pn]);
				end;

				{ assign enough cards from the "TempDeck" to each test hand to fill it up to the maximum # of cards for the current variation of the game }
				j:=1;
				for pn:=1 to MAX_PLAYERS do with THands[pn] do if n>0 then
					for k:=n+1 to StudRndNo+1 do begin
						inc(n);
						card[n]:=tempDeck[j];
						if j<ncrdslft then inc(j) else j:=1;
					end;

				{ now test "po"s potential hand against all the others }
				StudPokerResult(THands[po],MResult);
				Beat:=false;
				pn:=playerIndex(nextPlaying(Integer(po)));
				while (not Beat) and (pn<>po) do begin
					{ If my potential hand looses to all my opponents test hands then I will loose the round. }

					studPokerResult(THands[pn],TResult);

					{$ifndef VER1}
					if (GetScoreMode = PlayHighLow) then SetEvaluationMode(HighEval);
					{$endif}
					Beat:=(cmpHands(TResult.EH,MResult.EH)>0);

					{$ifndef VER1}
					if (GetScoreMode=PlayHighLow) then begin
						if Beat then begin
							SetEvaluationMode(LowEval);
							Beat:=(cmpHands(TResult.EL,MResult.EL)>0);
							with GngFr[po] do if not Beat then begin
								Dcdd:=True;
								ScrMd:=PlayLow;
							end
							else
								Dcdd:=False;
						end
						else with GngFr[po] do begin
							Dcdd:=True;
							ScrMd:=PlayHigh;
						end;
					end;
					{$endif}

					{ next opponent }

					pn:=playerIndex(nextPlaying(Integer(pn)));
				end;
				if not Beat then o:=o+1.0;
			end;
			StudOddsOfWin:=o/the_players[po].smplDpth;
		end
		{ final round }
		{$ifndef VER1}
		else if (GetScoreMode=PlayHighLow) then begin
			SetEvaluationMode(HighEval);
			BestOOB(po,TResult.EH);
			SetEvaluationMode(LowEval);
			BestOOB(po,TResult.EL);
			if (CmpHands(TResult.EH,the_hand_valuations[po].EH)>0) and (CmpHands(TResult.EL,the_hand_valuations[po].EL)>0) then
				StudOddsOfWin:=0.0
			else
				StudOddsOfWin:=oddsToWin(po);
		end
		{$endif}
		else begin
			BestOOB(po,TResult.EH);
			if CmpHands(TResult.EH,the_hand_valuations[po].EH)>0 then
				StudOddsOfWin:=0.0
			else
				StudOddsOfWin:=oddsToWin(po);
		end;

	end;

function MinPotOdds(amount:integer):real;

{ Return the minimum pot odds of throwing in "amount" chips. }

var
	R:real;
begin
	with theStudPokerOptions do begin
		if StudRndNo=FrstRnd then begin
			if RandomReal>=0.7 then
				MinPotOdds:=1/1000
			else
				MinPotOdds:=1000;
		end
		else if StudRndNo<LastRnd then begin
			R:=LastRnd*m_aRoundBettingLimits.minOpen[1];
			MinPotOdds:=Max(R+R*random(20)/100,PotOdds(amount));
		end
		else
			MinPotOdds:=PotOdds(amount);
	end;
end;

function StudShouldOpen(po:playerIndex):boolean;

{ Return true if player # "po" should open the betting for this round and set "the_current_bet" to the amount of the bet. }

var
	aresult:boolean;
	testBet:integer;
	minBet:integer;

	function DoStudShouldOpen(o:real):boolean;
	
	var
		b:boolean;
		p:real;

	begin //writeln('DoStudShouldOpen(',o,')');
		if o=0.0 then o:=1/infinity;
		with the_players[po] do begin
			if (skill>=basicBetLevel) then begin
				with theStudPokerOptions do begin
					minBet:=m_aRoundBettingLimits.minOpen[studRndNo-frstRnd+1];
					the_current_bet:= minBet - 1;
					repeat
						inc(the_current_bet);
					until
						(the_current_bet = m_aRoundBettingLimits.maxOpen[studRndNo-frstRnd+1])
						or
						(MinPotOdds(the_current_bet+1)<1/o);
					the_current_bet:=minBet+random(the_current_bet-minBet);
					p:=MinPotOdds(the_current_bet);
					DoStudShouldOpen:=(p+p*PotOddsFactor>=1/o);
				end
			end
			else begin
				b:=(o>=winLimit);
				if b then with theStudPokerOptions do the_current_bet:=min(
						Integer(m_aRoundBettingLimits.minOpen[studRndNo-frstRnd+1]+round((o-winLimit)*10)),
						m_aRoundBettingLimits.maxOpen[studRndNo-frstRnd+1]
						);
				DoStudShouldOpen:=b
			end;
		end;
	end;

begin
	if (StudRndNo=FrstRnd) then begin
		StudShouldOpen:=True;
		Exit;
	end
	else if (studrndNo=lastRnd) and BeatOnBoard(po) then begin
		StudShouldOpen:=false;
		Exit;
	end;
	aresult:=DoStudShouldOpen(StudOddsOfWin(po));
	if the_players[PO].GoForBlf then begin
		with theStudPokerOptions do the_current_bet:=m_aRoundBettingLimits.minOpen[studRndNo-frstRnd+1];
		aResult:=True;
	end;
	StudShouldOpen:=aresult;
end;

function DrawShouldOpenAfter(po:playerIndex):boolean;

	{ Return true if player # "po" should open the 2nd betting round (after the draw). }

	var
		o,p:real;
		aresult:boolean;
		
	begin
		with the_players[po] do begin
			o:=oddsToWin(po);
			if o=0 then o:=1/infinity;
			if skill>=basicBetLevel then begin
				{ for player's that know about the basic betting principle adjust the current bet to the max possible without exceeding the pot odds }
				with theDrawPokerOptions do begin
					the_current_bet:=afterDrawOpen-1;
					repeat
						inc(the_current_bet);
					until (the_current_bet=afterDrawOpenMax) or (potOdds(the_current_bet+1)<1/o);
					the_current_bet:=afterDrawOpen+random(the_current_bet-afterDrawOpen);
					p:=potOdds(the_current_bet);
					aResult:=(p+p*potOddsFactor>=1/o);
				end
			end
			else begin
				aResult:=(o>=winLimit);
				if aResult then with theDrawPokerOptions do
					{ assign the bet amount randomly depending on the odds }
					the_current_bet:=min(Integer(afterDrawOpen+round((o-winLimit)*10)),afterDrawOpenMax);
			end;
		end;
		if the_players[PO].GoForBlf then begin
			the_current_bet:=theDrawPokerOptions.afterDrawOpen;
			aResult:=True;
		end;
		DrawShouldOpenAfter:=aResult;
	end;

procedure evaluateRaise1(po:playerIndex);

{ In Draw poker let player # "po"'s opponent's evaluate his first round RAISE. }

begin
end;

procedure evaluateRaise2(po:playerIndex);

	{ In Draw Poker let player # "po"'s opponent's evaluate his final
		round RAISE. }

	var
		pn:playerIndex;
		h:handrank;

	procedure evalRaiseHigh(var e:pokerHandEvaluation);

		begin
			improveHand(pn,po,e);
		end;

	procedure evalRaiseLow(var e:pokerHandEvaluation);

		begin
			improveHand(pn,po,e);
		end;

	begin
		for pn:=1 to MAX_PLAYERS do if (pn<>po) and the_players[pn].isPlaying then with predict[pn,po] do
			{ for each opponent still in the game }
			case theDrawPokerOptions.m_aScoringMode of
				playHigh:evalRaiseHigh(eval.eh);
				{$ifndef VER1}
				playLow:evalRaiseLow(eval.eh);
				playHighLow:begin
					if DecidedHigh(isGngFr) then begin
						setEvaluationMode(highEval);
						evalRaiseHigh(eval.eh);
					end;
					if DecidedLow(isGngFr) then begin
						setEvaluationMode(lowEval);
						evalRaiseLow(eval.el);
					end;
					setEvaluationMode(highEval);
				end;
				{$endif}
			end;
	end;

procedure StudEvalRaise(po:playerIndex);

	var
		pn:playerIndex;
		h:handrank;
		e:pokerHandEvaluation;
	begin
		{ for each opponent still in the game }
		for pn:=1 to MAX_PLAYERS do if (pn<>po) and the_players[pn].isPlaying then begin
			{ update what player "pn" knows about player "po"'s hand }
			BestOOB(pn,e);
			with predict[pn,po] do case getScoreMode of
				playHigh
				{$ifndef VER1}
				,PlayLow
				{$endif}:
					ImproveHand(pn,po,eval.eh);
				{$ifndef VER1}
				playHighLow:
					predictHand(pn,po,eval.eh,e.rslt,e.pip,RUNT,acePip,[]); { ??? }
				{$endif}
			end;
		end;
	end;

procedure EvaluateRaiseBy(pn:playerIndex);
begin
	if PlayingDrawPoker then
		if BeforeTheDraw then
			EvaluateRaise1(pn)
		else
			EvaluateRaise2(pn)
	else
		StudEvalRaise(pn);
end;

procedure DrawEvalCallBeforeBy(po:playerIndex);

	{ Let player "po"'s opponent's evaluate his call during the first
		round. }

	var
		pn:playerIndex;
		l:longInt;
		h:handrank;
		r:real;

	procedure evalCallHigh(var e:pokerHandEvaluation);
		begin
			if JackpotsVar then
				if playerHistory(po,PASSED) then
					{ player already PASSED on this round so it is possible he has
						a short hand }
					predictHand(pn,po,e,ONE_PAIR,jackPip,ONE_PAIR,acePip,[])
					{ ??? }
				else
					{ player did not pass on the first round, so it is not
						as clear that it must be a short hand }
					predictHand(pn,po,e,RUNT,tenPip,ONE_PAIR,acePip,[])
			else
				predictHand(pn,po,e,RUNT,tenPip,ONE_PAIR,acePip,[]);
		end;

	procedure evalCallLow(var e:pokerHandEvaluation);
		begin
			predictHand(pn,po,e,ONE_PAIR,kingPip,RUNT,acePip,[]);
		end;

	begin
		for pn:=1 to MAX_PLAYERS do with the_players[pn] do if (pn<>po) and isPlaying then with predict[pn,po] do
			{ for each opponent still in the game }
			case theDrawPokerOptions.m_aScoringMode of
				playHigh:evalCallHigh(eval.eh);
				{$ifndef VER1}
				playLow:evalCallLow(eval.eh);
				playHighLow:begin
					if DecidedHigh(isGngFr) then begin
						setEvaluationMode(highEval);
						evalCallHigh(eval.eh);
					end;
					if DecidedLow(isGngFr) then begin
						setEvaluationMode(lowEval);
						evalCallLow(eval.el);
					end;
					setEvaluationMode(highEval);
				end;
				{$endif}
			end;
	end;

procedure DrawEvalCallAfterBy(po:playerIndex);

	{ Let player # "po"'s opponent's evaluate his call.

		When a player calls in the final round he is not bluffing. Average
		skill level player's should know this. }

	var

		pn:playerIndex;

	procedure evalCallHigh(var e:pokerHandEvaluation);

		begin
			case nCardsDrwn[po] of
				0:;
				1:predictHand(pn,po,e,TWO_PAIR,eightPip,TWO_PAIR,acePip,[]);
				2:predictHand(pn,po,e,ONE_PAIR,acePip,TWO_PAIR,acePip,[]);
				3:predictHand(pn,po,e,ONE_PAIR,queenPip,TWO_PAIR,acePip,[]);
				4,5:predictHand(pn,po,e,ONE_PAIR,tenPip,TWO_PAIR,acePip,[]);
			end
		end;

	procedure evalCallLow(var e:pokerHandEvaluation);

		begin
			case nCardsDrwn[po] of
				1..5:predictHand(pn,po,e,RUNT,acePip,RUNT,acePip,[]);
			end;
		end;

	begin
		for pn:=1 to MAX_PLAYERS do if (pn<>po) and (the_players[pn].isPlaying) then begin
			with the_players[pn] do if skill>=0 then predict[pn,po].isBlffng:=false;
			with predict[pn,po] do case theDrawPokerOptions.m_aScoringMode of
				playHigh:evalCallHigh(eval.eh);
				{$ifndef VER1}
				playLow:evalCallLow(eval.eh);
				playHighLow:begin
					if DecidedHigh(isGngFr) then begin
						setEvaluationMode(highEval);
						evalCallHigh(eval.eh);
					end;
					if DecidedLow(isGngFr) then begin
						setEvaluationMode(lowEval);
						evalCallLow(eval.el);
					end;
					setEvaluationMode(highEval);
				end;
				{$endif}
			end;
		end;
	end;

procedure EvaluateCallBy(pn:playerIndex);

begin
	if PlayingDrawPoker then
		if BeforeTheDraw then
			DrawEvalCallBeforeBy(pn)
		else
			DrawEvalCallAfterBy(pn)
	else
		StudEvalCall(pn);
end;

var
	discardPile:array[1..deck_size] of playingCard; { discarded cards }

procedure AddToDiscardPile(c:playingCard);

begin
	inc(discardPileN);
	discardPile[discardPileN]:=c;
end;

procedure EndTheDraw;

begin
	newHistory; { start a new line in the history record }
	NewBettingInterval;
	the_current_bet:=theDrawPokerOptions.AfterDrawOpen;
	UpdateMessageLineProxy;
	ClearStatus:=True;
	PostMessage(frame_handle,WM_NEXTOPEN,WhoLastBet,0);
end;

procedure DrawCardsFor(who:playerIndex;n:integer);

{ Discard the last "n" cards (logically and visually) from player
	"who"'s hand and draw "n" more. }

var
	i:integer;

	procedure makeupDeck;
	{ Make the deck up again from the 1 remaining card plus all the ones
		that have already been thrown out. }
	var
		i:integer;
	begin
		for i:=2 to discardPileN+1 do theDeck[i]:=discardPile[i-1];
		deckIndex:=1+discardPileN;
		discardPileN:=0;
		pokerShuffle;
	end;

begin
	{$ifndef ANAL}
	if n=0 then
		DspPlrMsg(who,'Stand Pat')
	else
		DspPlrMsg(who,'Draw '+int2str(n));
	{$endif}
	nCardsDrwn[who]:=n;
	if n=0 then
		AddStatus(who,'Stand')
	else
		AddStatus(who,'Draw '+int2str(n));
	if n>0 then begin;
		with the_players[who] do begin
			{ move the discards to the "discardPile" }
			for i:=1 to n do begin
				addToDiscardPile(theHands[who].card[theHands[who].n]);
				dec(theHands[who].n);
			end;
			the_view^.hand_props[who].Refresh;
			{$ifndef ANAL}
			delay(DELAY_ACTION);
			{$endif}
			for i:=handSize-n+1 to handSize do begin
				inc(theHands[who].n);
				theHands[who].card[theHands[who].n]:= theDeck[deckIndex];
				the_view^.hand_props[who].CopyFromHandData(theHands[who], who = USER_ID);
				{$ifndef ANAL}
				the_view^.hand_props[who].Refresh;
				SndCardFlick;
				{$endif}
				dec(deckIndex);
				if deckIndex=1 then makeupDeck;
			end;
		end;
		delay(DELAY_ACTION);
		the_view^.hand_props[who].Refresh;
	end;
	if who=theDealerIndex then
		EndTheDraw
	else
		PostMessage(frame_handle,wm_NextDraw,NextClockwiseFrom(who),0)
end;

var
	DiscardSelection:array[1..5] of boolean;

const
	id_Draw1=101;
	id_Draw5=105;

constructor TDrawDlg.Init;

begin
	inherited Construct(the_app^.Frame^.MyFrameWindow^.handle,903);
	FillChar(DiscardSelection,SizeOf(DiscardSelection),#0);
end;

function TDrawDlg.OnInitDialog:boolean;

begin
	OnInitDialog:=inherited OnInitDialog;
	CenterWindow(Handle,GetParent);
end;

function TDrawDlg.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	case aMsg of
		WM_DRAWITEM:OnMsg:=WMDrawItem(wParam,PDrawItemStruct(lParam));
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

procedure TDrawDlg.SetState(i:integer;State:boolean);

var
	zs:array[0..20] of Char;

begin
	if State then
		StrCopy(zs,'Discard')
	else
		StrCopy(zs,'Hold');
	windows.SetWindowText(GetDlgItem(200+i),zs);
end;

function CardRect(var r:TRect;x,y:integer):PRect;

{ Return in "r" the bounds of a card image at (x,y). }

begin
	with r do begin
		left:=x;
		top:=y;
		right:=left+CardImageWd;
		bottom:=top+CardImageHt;
	end;
	CardRect:=@r;
end;

function TDrawDlg.WMDrawItem(controlId:UINT;aDrawItemStruct:PDrawItemStruct):LONG;

var
	r:TRect;

	procedure IndicDiscard;

	begin
		with aDrawItemStruct^ do with rcItem do begin
			CardRect(r,Integer(Centered(CardImageWd,Left,Right)),Integer(Centered(CardImageHt,Top,Bottom)));
			with r do begin
				Left:=Left+(CardImageWd div 6);
				Right:=Right-(CardImageWd div 6);
				Top:=Centered(GetRectWd(r),Top,Bottom);
				Bottom:=Top+GetRectWd(r);
			end;
			DrawDCNoSymbol(hDC,r);
		end;
	end;

begin
	with aDrawItemStruct^ do case controlId of
		id_Draw1..id_Draw5:with rcItem do begin
			if (ItemState and ods_Selected)<>0 then begin
				Toggle(DiscardSelection[controlId-id_Draw1+1]);
				SetState(controlId-id_Draw1+1,DiscardSelection[controlId-id_Draw1+1]);
				FillRect(hDC,rcItem,HBRUSH(COLOR_3DFACE+1));
				DisplayCard(hDC, theHands[USER_ID].card[controlId-id_Draw1+1],
					Centered(CardImageWd,Left,Right),
					Centered(CardImageHt,Top,Bottom));
				if DiscardSelection[controlId-id_Draw1+1] then IndicDiscard;
			end;
			if (ItemState and ods_Focus)<>0 then begin
				with rcItem do CardRect(r,Centered(CardImageWd,Left,Right),Centered(CardImageHt,Top,Bottom));
				with r do begin
					Dec(Left,3);Dec(Top,3);
					Inc(Right,3);Inc(Bottom,3);
				end;
				DrawFocusRect(hDC,r);
				{FrameRect(hDC,r,GetStockObject(Black_Brush));}
			end;
			if ItemAction=oda_DrawEntire then begin
				FillRect(hDC,rcItem,HBRUSH(COLOR_3DFACE+1));
				DisplayCard(hDC, theHands[USER_ID].card[controlId-id_Draw1+1],
					Centered(CardImageWd,Left,Right),
					Centered(CardImageHt,Top,Bottom));
				if DiscardSelection[controlId-id_Draw1+1] then IndicDiscard;
			end;
		end;
	end;
	WMDrawItem:=1;
end;

procedure HumanDraw;
var
	i,j,x:integer;
	t:handType;
begin
	delay(cardPlayDelay);
	{ adjust the hand }
	with the_players[USER_ID] do begin
		t.n:=0;
		for i:=1 to handSize do if not discardSelection[i] then begin
			inc(t.n);
			t.card[t.n]:= theHands[USER_ID].card[i];
		end;
		j:=0;
		for i:=1 to handSize do if discardSelection[i] then begin
			inc(t.n);
			inc(j);
			t.card[t.n]:= theHands[USER_ID].card[i];
		end;
		theHands[USER_ID]:= t;
		the_view^.hand_props[USER_ID].CopyFromHandData(theHands[USER_ID], true);
	end;
	drawCardsFor(USER_ID,j);
end;

procedure DrawFor(po:playerIndex);

	{ Draw cards for computer player "po". }

	var
		i:integer;
		t:handType;

	begin
		with the_players[po] do if GoForBlf then begin
			if random(1000)<500 then
				drawCardsFor(po,1)
			else
				drawCardsFor(po,2);
		end
		{ determine what to keep }
		else if isShort(po) then begin
			{ move the good part of the hand to the front }
			t.n:=0;
			with the_short_hand_valuations[po].set1 do for i:=1 to n do begin
				inc(t.n);
				t.card[t.n]:= theHands[po].card[idxs[i]];
			end;
			with the_short_hand_valuations[po] do OtherIdx(set1,5,set2);
			{ now the rest of the hand }
			with the_short_hand_valuations[po].set2 do for i:=1 to n do begin
				inc(t.n);
				t.card[t.n]:= theHands[po].card[idxs[i]];
			end;
			theHands[po]:=t;
			drawCardsFor(po,1);
		end
		else begin
			if gngFr[po].dcdd then case gngFr[po].scrMd of
				playHigh
				{$ifndef VER1}
				,playLow
				{$endif}:begin
					if gngFr[po].scrMd=playHigh then
						holdHigh(the_hand_valuations[po].eh)
					else
						holdLow(the_hand_valuations[po].eh);
					{ move the good part of the hand to the front }
					t.n:=0;
					with the_hand_valuations[po].eh.set1 do for i:=1 to n do begin
						inc(t.n);
						t.card[t.n]:= theHands[po].card[idxs[i]];
					end;
					{ now the rest of the hand }
					with the_hand_valuations[po].eh.set2 do for i:=1 to n do begin
						inc(t.n);
						t.card[t.n]:= theHands[po].card[idxs[i]];
					end;
					theHands[po]:=t;
					drawCardsFor(po,the_hand_valuations[po].eh.set2.n);
				end;
				{$ifndef VER1}
				playHighLow: { probably want to stay with what you got ??? }
					DrawCardsFor(po,0);
				{$endif}
			end
			else begin
				DrawCardsFor(po,0);
				if po=theDealerIndex then
					EndTheDraw
				else
					PostMessage(frame_handle,wm_NextDraw,NextClockwiseFrom(po),0);
			end;
		end;
	end;

function ShouldStay2(po:playerIndex):boolean;

	{ Return true if player "po" should stay/call. Also set the
		"shouldRaise" variable if he should raise. }

	var
		o:real;
		aResult:boolean;

	begin
		shouldRaise:=false;
		with the_players[po] do begin
			o:=oddsToWin(po);
			if o=0 then o:=1/infinity;
			if (skill>=basicBetLevel) then begin
				aResult:=(potOdds(the_current_bet-theBets[po])+potOdds(the_current_bet-theBets[po])*potOddsFactor>=1/o);
				if
					aResult
					and
					raiseIsOk(po,theDrawPokerOptions.raiseLmts)
					and (potOdds(the_current_bet-theBets[po]+theDrawPokerOptions.afterDrawOpen)>=1/o)
					then
					shouldRaise:=RandomReal>0.8;
			end
			else begin
				aResult:=((o+o*potOddsFactor)>=winLimit);
				if
					aResult
					and
					raiseIsOk(po,theDrawPokerOptions.raiseLmts)
					and
					(o>=raiseLmt)
					then
					shouldRaise:=RandomReal>0.8;
			end;
			if GoForBlf then begin
				if raiseIsOk(po,theDrawPokerOptions.raiseLmts) then begin
					aResult:=True;
					ShouldRaise:=True;
				end
				else
					aResult:=False;
			end;
		end;
		if ShouldRaise then the_last_raise_amount:=Random(the_current_bet)+1;
		shouldStay2:=aResult;
	end;

function StudShouldStay(po:playerIndex;rndNo:integer):boolean;

	{ Return true if player "po" should stay/call. Also set the
		"ShouldRaise" variable if he should raise. }

	var
		o,p:real;
		aResult:boolean;
		e:pokerHandEvaluation;
	begin
		ShouldRaise:=false;
		if ((rndNo=lastRnd) and beatOnBoard(po)) then begin
			StudShouldStay:=false;
			exit;
		end;
		o:=StudOddsOfWin(po); { odds that you can win }
		if o=0.0 then o:=1/infinity;
		with theStudPokerOptions do p:=MinPotOdds(the_current_bet-theBets[po]);
		with the_players[po] do begin
			if (skill>=basicBetLevel) then begin
				aResult:=(p+p*PotOddsFactor>=1/o);
				if
					aResult
					and
					raiseIsOk(po,theStudPokerOptions.raiseLmts)
					and
					(StudRndNo>4)
					and
					(MinPotOdds(the_current_bet-theBets[po]+1)>=1/o)
					then ShouldRaise:=(RandomReal>0.8);
			end
			else begin
				aResult:=(o>=winLimit);
				if
					(studRndNo=lastRnd)
			 and
					aResult
					and
					(StudRndNo>3)
					and
					raiseIsOk(po,theStudPokerOptions.raiseLmts)
					and
					(o>raiseLmt)
					then ShouldRaise:=(RandomReal>0.6);
			end;
		end;
		if the_players[PO].GoForBlf then begin
			if raiseIsOk(po,theStudPokerOptions.raiseLmts) then begin
				aResult:=True;
				ShouldRaise:=True;
			end
			else
				aResult:=False;
		end;
		if ShouldRaise then the_last_raise_amount:=Random(the_current_bet)+1;
		StudShouldStay:=aResult;
	end;

function ShouldStay(pn:playerIndex):boolean;

	begin
		if PlayingDrawPoker then
			if BeforeTheDraw then
				ShouldStay:=ShouldStay1(pn)
			else
				ShouldStay:=ShouldStay2(pn)
			{ShouldStay:=true}
		else
			ShouldStay:=StudShouldStay(pn,StudRndNo);
	end;

function PassAllowed:boolean;

{ return true if passing/checking is allowed during this open/bet round }

	function IsPassBeforeDrawOk:boolean;

	begin
		IsPassBeforeDrawOk:=theDrawPokerOptions.PassOK[drawplib.Variation, 0];
	end;

	function IsPassAfterDrawOk:boolean;

	begin
		IsPassAfterDrawOk:=theDrawPokerOptions.PassOK[drawplib.Variation,1];
	end;

begin
	PassAllowed:=
		{$ifndef VER1}
		(GetScoreMode <> PlayHighLow)
		and
		{$endif}
		(
			(
				PlayingDrawPoker
				and
				(
					(BeforeTheDraw and IsPassBeforeDrawOk)
					or
					(AfterTheDraw and IsPassAfterDrawOk)
				)
			)
			or
			(PlayingStudPoker)
		);
end;

function lastUpNo:integer;

{ Return the round number of the last upcard. }

begin
	case theStudPokerOptions.gType of
		stud5card:lastUpNo:=5;
		stud6card:lastUpNo:=5;
		stud7card
		{$ifndef VER1}
		,baseball
		{$endif}:
			lastUpNo:=6;
	end;
end;

function IsUpCardRound(r:integer):boolean;

{ return true if round "r" is an upcard round. }

begin
	case theStudPokerOptions.gType of
		stud5card:IsUpCardRound:=(r>1);
		stud6card:IsUpCardRound:=(r>1) and (r<6);
		stud7card
		{$ifndef VER1}
		,baseball
		{$endif}:
			IsUpCardRound:= (r>2) and (r<7);
	end;
end;

procedure makeShortHand(po:playerIndex;var h:handType);

	{ Makeup a hand from only the known cards for player "po". }

	var
		i:integer;
	begin
		h.n:=0;
		for i:=1 to theHands[po].n do if sure[po,i] then begin
			inc(h.n);
			h.card[h.n]:= theHands[po].card[i];
		end;
	end;

function HighestShowing(f,l:integer):playerIndex;

	{ Return the player who dislays the best up cards.
		In the case of a tie return the one closest to the theDealerIndex.
		In a High/Low game use the High ranked hand. }

	var
		i,best:integer;
		first,pn:playerIndex;
	begin
		best:=0;
		{ start with the theDealerIndex or the first player to his left if the
			theDealerIndex has already FOLDED }
		SetEvaluationMode(HighEval);
		if the_players[theDealerIndex].isPlaying then
			first:=theDealerIndex
		else
			first:=nextPlaying(theDealerIndex);
		pn:=first; { start with this guy }
		repeat
			if (best=0) then
				best:=pn
			else if cmpHands(UpCardsEval[PN].EH,UpCardsEval[Best].EH)>0 then
				Best:=PN;
			pn:=nextPlaying(pn);
		until pn=first;
		highestShowing:=best;
	end;

procedure bestOOT(po:playerIndex;var hand2Beat:pkrResultType);

	{ Return the best hand on the table predicted by player "po". }

	var

		best:integer;
		pn:playerIndex;
		h:handType;
		testResult:pkrResultType;

	begin

		best:=0;
		for pn:=1 to MAX_PLAYERS do if (pn<>po) and the_players[pn].isPlaying then
			if unAssigned(po,pn)=0 then begin
				{ we already have a prediction for this player }
				if (best=0) or (cmpHands(predict[po,pn].eval.eh,hand2beat.eh)>0) then begin
					hand2beat:=predict[po,pn].eval;
					best:=pn;
				end
			end
			else begin
				{ We don't have a prediction yet for this player's hand so just
					evaluate the known cards and use that }
				makePredHand(po,pn,h);
				studPokerResult(h,testResult);

				if (best=0) or (cmpHands(testResult.eh,hand2beat.eh)>0) then begin
					hand2beat:=testResult;
					best:=pn;
				end;
			end;

	end;

procedure WorstOnTable(po:playerIndex;var hand2Beat:pkrResultType);

	{ Return the worst hand on the table predicted by player "po". }

	var

		worst:integer;
		pn:playerIndex;
		h:handType;
		testResult:pkrResultType;

	begin

		worst:=0;
		for pn:=1 to MAX_PLAYERS do if (pn<>po) and the_players[pn].isPlaying then
			if unAssigned(po,pn)=0 then begin

				{ we already have a prediction for this player }

				if (worst=0) or (cmpHands(predict[po,pn].eval.eh,hand2beat.eh)<0) then begin
					hand2beat:=predict[po,pn].eval;
					worst:=pn;
				end
			end
			else begin

				{ We don't have a prediction yet for this player's hand so just
					evaluate the known cards and use that }

				makePredHand(po,pn,h);
				studPokerResult(h,testResult);
				if (worst=0) or (cmpHands(testResult.eh,hand2beat.eh)<0) then begin
					hand2beat:=testResult;
					worst:=pn;
				end;
			end;
	end;

procedure PlayerMatched(pn:playerIndex);
{ In baseball execute a match for player # pn. }
begin
	{updatePlayerStatus(pn,called);}
	updatePot(pn,potAmt);
	delay(DELAY_ACTION);
end;

procedure EvaluateMatch(po:playerIndex);
{ Evaluate what a MATCH by player "po" means to his opponents. }
begin
	StudEvalOpenBy(po);
end;

{$ifdef BASEBALL}

function MatchThePot(po:playerIndex):boolean;

	{ Return true if player "po" decides to match the pot in baseball. }

	begin
		if po=USER_ID then
			with the_app^ do MatchThePot:=(ExecDialog(New(PDialog,Init(MainWindow,'Match')))=1)
		else begin
			the_current_bet:=PotAmt;
			matchThePot:=StudShouldStay(po,StudRndNo);
			the_current_bet:=0;
		end;
	end;

{$endif}

procedure UpdateEvaluation;
	{ Evaluate all the showing hands and store them in "UpCardsEval"
		so the other procedures can access them during the round. }
	var
		PN:playerIndex;
		TempHand:HandType;
	begin
		for PN:=1 to MAX_PLAYERS do with the_players[pn] do if isPlaying then begin
			StudPokerResult(theHands[pn],the_hand_valuations[pn]); { evaluate the player's actual hands }
			{ Decide if player PN should go for the bluff or not }
			if
				(GetScoreMode=PlayHigh)
				and
				(StudRndNo>4)
				and
				(the_hand_valuations[PN].EH.Rslt<ONE_PAIR)
				and
				(Random(1000)<5)
				then
				the_players[PN].GoForBlf:=True;
			MakeShortHand(PN,TempHand);
			StudPokerResult(TempHand,UpCardsEval[PN]);
		end;
	end;

function Stop:boolean;

	{ return true if the round should be stopped. }

	begin
		Stop:=(nPlayers=1);
	end;

function PointAlongRay(w,h:integer;aDegreesFromEast:integer;aPercentDistance:real):point32;

var
	pt:point32;
	x,y,rise,run:real;

	function m:real; begin m:=rise/run; end;

begin
	rise:=Sin(DegToRad(aDegreesFromEast));
	run:=Cos(DegToRad(aDegreesFromEast));
	case aDegreesFromEast of
		0:x:=w;
		1..89:x:=Min(Round(h/m),w);
		90:x:=0;
		91..179:x:=Max(Round(h/m),-w);
		180:x:=-w;
		181..269:x:=Max(Round(-h/m),-w);
		270:x:=0;
		271..359:x:=Min(Round(-h/m),w);
	end;
	case aDegreesFromEast of
		0:y:=0;
		1..89:y:=Min(Round(m*x),h);
		90:y:=h;
		91..179:y:=Min(Round(m*x),h);
		180:y:=0;
		181..269:y:=Max(Round(m*x),-h);
		270:y:=-h;
		271..359:y:=Max(Round(m*x),-h);
	end;
	x:=x*aPercentDistance;
	y:=y*aPercentDistance;
	pt.x:=Round(x);
	pt.y:=Round(y);
	PointAlongRay:=pt;
end;

constructor TStatusNote.Init(aParent:PWindow;pn:playerIndex);
begin
	inherited Init(aParent,'',0,0);
	PlrNo:=pn;
end;

procedure TStatusNote.Show;
var
	aSpan:TRect;
	w,h:LONG;
begin
	the_view^.hand_props[PlrNo].GetSpanRect(aSpan);
	w:=GetWndWd(handle);
	h:=GetWndHt(handle);
	MoveWindow(handle,Centered(w,aSpan.left,aSpan.right-1),Centered(h,aSpan.top,aSpan.bottom-1),w,h,TRUE);
	inherited Show;
end;

constructor TNote.Init(a_pParentWindow:PWindow;a_win_title_p:PChar;x,y:integer);
begin
	FixedWd:=False;
	handle:=CreateWindow('STATIC','',WS_CHILD or WS_BORDER or SS_CENTER,x,y,100,3+SysFontHt+2,a_pParentWindow^.handle,100,hInstance,nil);
	SetText(a_win_title_p);
end;

procedure TNote.Show;
begin
	ShowWindow(handle,SW_SHOWNA);
	UpdateWindow(handle);
end;

procedure TNote.Hide;
begin
	ShowWindow(handle,SW_HIDE);
	UpdateWindow(GetParent(handle));
end;

procedure TNote.SetText(aTextString:PChar);
var
	aDC:hDC;
	ps:stringBuffer;
	w:longint;
begin
	SetWindowText(handle,aTextString);
	if not FixedWd then begin
		StrCopy(ps,aTextString);
		StrCat(ps,'      ');
		{ adjust the width of the note for the current text }
		aDC:=GetDC(GetParent(handle));
		w:=GetHdcTextWidth(aDC,ps);
		ReleaseDC(GetParent(handle),aDC);
		MoveWindow(handle,GetWndLeft(handle),GetWndTop(handle),w,GetWndHt(handle),FALSE);
	end;
end;

function TNote.IsVisible:boolean;
begin
	IsVisible:=IsWindowVisible(handle);
end;

function PokerPlayer.Bet:PokerBetPropP;
begin
	Bet:=@MyBet;
end;

function PokerPlayer.Chips:PlayerChipsPropP;
begin
	Chips:=@MyChips;
end;

procedure PokerPlayer.Raise(amount:number);
begin
	NewHistory;
	Bet^.AddAmount(Chips^.PopUnits(the_current_bet+Integer(amount)-Round(Bet^.Value)));
	the_current_bet:=Round(Bet^.Value);
	theBets[pno]:=the_current_bet;
	Inc(plrRaiseCnt[pno]);
	Inc(totRaiseCnt);
end;

{$ifdef TEST}
constructor testable_OPlayerChipsProp.Construct;
begin
	bundle.Init(0.0);
	TheBundle:=@bundle;
end;

procedure testable_OPlayerChipsProp.GetSpanRect(var rRect:TRect);
begin
end;

procedure testable_OPlayerChipsProp.RefreshRect(const rPrevSpan:TRect);
begin
end;

procedure testable_OPlayerChipsProp.Refresh; 
begin
end;

constructor testable_OBetProp.Construct;
begin
	pile.Init(10);
	TheChips:=@pile;
end;

procedure testable_OBetProp.Refresh; 
begin
end;

procedure testable_OBetProp.GetSpanRect(var rRect:TRect);
begin
end;

function testable_OPokerPlayer.Bet:PokerBetPropP;
begin
	Bet:=@fake_bet_prop;
end;

function testable_OPokerPlayer.Chips:PlayerChipsPropP;
begin
	Chips:=@fake_chips;
end;

procedure Test_Player_Raise;
var
	player:testable_OPokerPlayer;
begin
	player.Construct;
	totRaiseCnt:=23;
	plrRaiseCnt[1]:=13;
	the_current_bet:=5;
	player.Bet^.AddAmount(2);
	player.Chips^.SetAmount(200);
	player.Raise(2);
	AssertAreEqual(7,player.CurrentBet);
	AssertAreEqual(7,the_current_bet);
	AssertAreEqual(7,theBets[player.pno]);
	AssertAreEqual(24,totRaiseCnt);
	AssertAreEqual(14,plrRaiseCnt[1]);
	AssertAreEqual(195,player.Chips^.Value,0.0);
end;
{$endif}

procedure View.PlayerRaise(aPlayerId:playerIndex;raise_amount:number);
begin
	ClearStatus:=true;
	the_players[aPlayerId].Raise(raise_amount);
	UpdatePlayerStatus(aPlayerId,RAISED);
end;

procedure View.ProcessRaiseByPlayer(aPlayerId:playerIndex;amount:word);
begin //writeln('View.ProcessRaiseByPlayer(aPlayerId:playerIndex;amount=',amount,')');
	PlayerRaise(aPlayerId,amount);
	PostCommand(WM_NEXTBET,NextPlaying(aPlayerId),0);
	EvaluateRaiseBy(aPlayerId);
end;

constructor PokerHandProp.Construct(aIndex:playerIndex);
begin //writeln('PokerHandProp.Construct(',aIndex,')');
	inherited Construct(maxHandSize);
	myPlayerId:=aIndex;
//	AddWart(New(PPlayerStatusWart, Construct(@self)), BOTTOM_LEFT);
end;

function SeatHandRayAngle(seat_num:playerIndex):integer;
begin
	case seat_num of
		1:SeatHandRayAngle:=265;
		2:SeatHandRayAngle:=180+16;
		6:SeatHandRayAngle:=360-16;
		3:SeatHandRayAngle:=90+60;
		5:SeatHandRayAngle:=90-60;
		4:SeatHandRayAngle:=95;
	end;
end;

function PokerHandProp.GetAnchorPoint(table_width,table_height:word):xypair;
var
	tableCenter,pt:point32;
begin
	SetCardDX(OptXSpace);
	GetAnchorPoint:=MakeXYPair(Anchor.x, Anchor.y);
	if (table_width>0) and (table_height>0) then begin
		tableCenter.x:=(table_width div 2);
		tableCenter.y:=(table_height div 2);
		pt:=PointAlongRay(tableCenter.x, tableCenter.y, SeatHandRayAngle(myPlayerId), 0.72);
		GetAnchorPoint:=MakeXYPair(
			Nudge(tableCenter.x+pt.x-(CardImageWd*2 div 3),5),
			Nudge(tableCenter.y-pt.y-(CardImageHt div 2),3));
	end;
end;

procedure PokerHandProp.CopyFromHandData(const rHand:handType; aFaceupFlag:boolean);
var
	i:word;
begin
	ThePile^.Empty;
	for i:= 1 to rHand.n do begin
		if aFaceupFlag
			then ThePile^.Add(rHand.card[i] or FACEUP_BIT)
			else ThePile^.Add(rHand.card[i]);
	end;
end;

{$ifdef TEST}

procedure Test_CopyFromHandData;
var
	aPile:PokerHandProp;
	aHandData:handType;
begin
	aPile.Construct(1);
	aHandData.n:= 2;
	aHandData.card[1]:= MakeCard(TJACK, TSPADE);
	aHandData.card[2]:= MakeCard(TTHREE, THEART);
	aPile.CopyFromHandData(aHandData, true);
	AssertAreEqual(2, aPile.Size);
	AssertAreEqual(aHandData.card[2] or FACEUP_BIT, aPile.Get(2));
	aHandData.n:= 0;
	aPile.CopyFromHandData(aHandData, true);
	AssertAreEqual(0, aPile.Size);
end;

{$endif TEST}

constructor PlayerChipsProp.Construct(pView:ViewP; pOwner:PokerPlayerP; seat_num:SeatIndex);
var
	where:relativeposition;
begin
	inherited Construct(seat_num,2);
	owner:=pOwner;
	myTable:=pView;
	case seat_num of
		2,3:where:=BOTTOM_RIGHT;
		4:where:=TOP_LEFT;
		1,5,6:where:=BOTTOM_LEFT;
	end;
	AddWart(New(PPlayerNameTag, Construct(@self, pOwner^.GetNickName)), where);
end;

function SeatChipsRayAngle(seat_num:playerIndex):integer;
begin
	case seat_num of
		1:SeatChipsRayAngle:=SeatHandRayAngle(seat_num)+22;
		2:SeatChipsRayAngle:=SeatHandRayAngle(seat_num)+3;
		6:SeatChipsRayAngle:=SeatHandRayAngle(seat_num)-3;
		3:SeatChipsRayAngle:=SeatHandRayAngle(seat_num)+12;
		5:SeatChipsRayAngle:=SeatHandRayAngle(seat_num)-12;
		4:SeatChipsRayAngle:=SeatHandRayAngle(seat_num)-25;
	end;
end;

procedure PlayerChipsProp.CalcAnchorPt(var aAnchorPt:TPoint;aNewWd,aNewHt:word);
var
	pt:point32;
begin
	pt:=PointAlongRay(aNewWd div 2,aNewHt div 2,SeatChipsRayAngle(SeatNum),0.9);
	aAnchorPt.x:=Nudge((aNewWd div 2)+pt.x-(Width div 2),5);
	aAnchorPt.y:=Nudge((aNewHt div 2)-pt.y+(Height div 2),3);
	case SeatNum of
		1:aAnchorPt.y:=aNewHt-(WART_HEIGHT div 2)-MIN_EDGE_MARGIN;
		//4:aAnchorPt.y:=Height*2 div 3;
	end;
end;

function PlayerChipsProp.GetAnchorPoint(table_width,table_height:word):xypair;
var
	anchorPt:TPoint;
begin
	if table_width>0 then begin
		CalcAnchorPt(anchorPt, table_width, table_height);
		GetAnchorPoint:=MakeXYPair(anchorPt.x, anchorPt.y);
	end;
end;

procedure PlayerChipsProp.Toss(aChipType:TypeOfChip);
begin
	barbCancel^.Enable;
	the_players[SeatNum].myBet.Push(PopChip(aChipType));
	ViewP(MyTabletop)^.OnChipTossed(Trunc(the_players[SeatNum].myBet.Value));
end;

function PlayerChipsProp.CurrentBetLimit:word;

begin
	CurrentBetLimit:=myTable^.PlayerBetLimit(SeatNum);
end;

procedure PlayerChipsProp.OnStackClicked(aChipType:TypeOfChip);

begin
	if (Owner^.CurrentBet+ChipUnitValue(aChipType))<=CurrentBetLimit
		then Toss(aChipType)
		else CueBadClick;
end;

{$ifdef TEST}
type
	TestPlayerChipsProp = object(PlayerChipsProp)
		constructor Construct;
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure Refresh; virtual;
	end;

constructor TestPlayerChipsProp.Construct; begin end;
procedure TestPlayerChipsProp.GetSpanRect(var rRect:TRect); begin end;
procedure TestPlayerChipsProp.Refresh; begin end;
{$endif TEST}

procedure PlayerChipsProp.CueBadClick;

begin
	MessageBeep(MB_ICONEXCLAMATION);
end;

function PlayerChipsProp.PopChip(aChipType:TypeOfChip):TypeOfChip;

begin
	PopChip:=inherited PopChip(aChipType);
	if IsStackEmpty(aChipType) then SetAmount(theBundle^.DollarValue);
end;

{$ifdef TEST}

procedure Test_PopChip;
var
	tester:TestPlayerChipsProp;
begin
	tester.Construct;
	tester.TheBundle:=New(PBundleOfChips, Init(11.0));
	tester.PopChip(CHIP2);
	AssertAreEqual(5, tester.theBundle^.stacks[CHIP2]^.Size);
	punit.assert.AreEqualReal(10.0, tester.Value, 0.1);
	tester.PopChip(CHIP2);
	AssertAreEqual(4, tester.theBundle^.stacks[CHIP2]^.Size);
end;

{$endif}

procedure PlayerChipsProp.OnEnabled; 
begin 
	SaveButtonStates;
end;

procedure PokerPlayer.Borrow(n:number);

begin
	Inc(my_house_loan,n);
end;

{$ifdef TEST}

procedure Test_player_Borrow;

var
	player:testable_OPokerPlayer;
	
begin
	player.Construct;
	player.Borrow(25);
	AssertAreEqual(25, player.HouseLoan);
	player.Borrow(1);
	AssertAreEqual(26, player.HouseLoan);
end;

{$endif}

function PokerPlayer.IsPlaying:boolean;

begin
	isPlaying:= participating and (status<>FOLDED);
end;

function PokerPlayer.ShowStatus:boolean;

begin
	ShowStatus:=not (Status in [NOSTATUS, FOLDED]);
end;

function PokerPlayer.IsDealer:boolean;

begin
	IsDealer:=(pno=theDealerIndex);
end;

procedure PokerPlayer.CommitBetToPot;

begin
	myBet.SnapToPot;
end;

procedure PokerPlayer.SafeInitialize;

begin
	status:=NOSTATUS;
	raiseLmt:=9.9;
	my_house_loan:=0;
	orgMny:=the_table_stakes;
end;

constructor PokerPlayer.init(nickName:string;g:gender;posn:shortInt;p:boolean;sk:skillLevel;wl:real);
{	"nickName"
	"posn" position at the table
	"p" is true if the player is currently participating.
	"bBetting" is true if the player knows the basic betting rule. }
begin //writeln('PokerPlayer.Init(',nickName,',g;posn,p,sk:skillLevel,wl)');
	GamePlayerType.Init(NickName,g,100);
	SafeInitialize;
	participating:=p;
	skill:=sk;
	winLimit:=wl;
	pno:=posn;
end;

procedure PokerPlayer.ExposeSortedHand;

	{ Visually display player number "p"'s sorted hand. }

	var
		i,j,k:integer;
		t:handType;
		E:PokerHandEvaluation;

	begin
		{$ifndef ANAL}
		{$ifndef VER1}
		if GetScoreMode = PlayHighLow then
			if GngFr[PNo].ScrMd=PlayHigh then
				E:=the_hand_valuations[PNo].EH
			else
				E:=the_hand_valuations[PNo].EL
		else
		{$endif}
			E:=the_hand_valuations[PNo].EH;

		{ sort the cards according to the index }
		t.n:=0;
		{ first the hand }
		with E.set1 do for i:=1 to n do begin
			inc(t.n);
			t.card[t.n]:= theHands[pno].card[idxs[i]];
		end;
		{ then the remainder of the hand }
		with E.set2 do for i:=1 to n do begin
			inc(t.n);
			t.card[t.n]:= theHands[pno].card[idxs[i]];
		end;
		{ then any extra cards }
		j:=E.Set1.N+E.Set2.N;
		k:= theHands[PNo].N;
		if k>j then for i:=j+1 to k do begin
			inc(t.n);
			t.card[t.n]:= theHands[pno].card[1]; { ??? any old card }
		end;
		theHands[pno].Card:= t.Card;
		the_view^.hand_props[pno].CopyFromHandData(theHands[pno], true);
		with theHands[pno] do if n > 5 then begin
			for i:= 6 to n do the_view^.hand_props[pno].ThePile^.FlipFacedown(i);
		end;
		the_view^.hand_props[pno].Refresh;
		delay(DELAY_ACTION);
		{$endif}
	end;

function PokerPlayer.Purse:integer;
begin
	Purse:=pileValue(myChips);
end;

function PokerPlayer.NetWinnings:integer;
begin
	NetWinnings:=Purse-HouseLoan-OrgMny;
end;

function PokerPlayer.HouseLoan:integer;
begin
	HouseLoan:=my_house_loan;
end;

function PokerPlayer.NetWinningsAsText:string;
var
	net:integer;
begin
	net:=NetWinnings;
	if net=0 
		then NetWinningsAsText:='even'
		else if net>0
			then NetWinningsAsText:='up '+int2str(net)
			else NetWinningsAsText:='down '+int2str(abs(net));
end;

{$ifdef TEST}

constructor testable_OPokerPlayer.Construct; 
begin 
	inherited Init('Sucker',MALE,1,TRUE,MINSKILLLEVEL,0.0);
	fake_chips.Construct;
	fake_bet_prop.Construct;
end;

type
	TestPlayer=object(testable_OPokerPlayer)
		value_of_chips:integer;
		function Purse:integer; virtual;
	end;

function TestPlayer.Purse:integer; begin Purse:=value_of_chips; end;
	
procedure Test_player_NetWinningsAsText;

var
	player:TestPlayer;
	
begin
	player.Construct;
	player.OrgMny:=250;
	player.my_house_loan:=0;
	player.value_of_chips:=250;
	AssertAreEqual('even', player.NetWinningsAsText);
	player.value_of_chips:=260;
	AssertAreEqual('up 10', player.NetWinningsAsText);
	player.my_house_loan:=15;
	AssertAreEqual('down 5', player.NetWinningsAsText);
end;

{$endif}

function PokerPlayer.CurrentBet:word;
begin
	CurrentBet:=Round(Bet^.Value);
end;

function PokerPlayer.ShouldOpen:boolean;
begin
	if PlayingDrawPoker then begin
		if BeforeTheDraw then
			{$ifdef TEST_ALLPASS}
			ShouldOpen:=FALSE
			{$else}
			ShouldOpen:=
				(
					JackpotsVar
					and
					(
						(the_hand_valuations[pno].eh.rslt>ONE_PAIR)
						or
						((the_hand_valuations[pno].eh.rslt=ONE_PAIR) and (rank(the_hand_valuations[pno].eh.pip)>=rank(JACKPIP)))
					)
				)
				or
				((not JackPotsVar) and ShouldStay1(pno))
			{$endif}
		else
			ShouldOpen:=DrawShouldOpenAfter(pno)
	end
	else ShouldOpen:=StudShouldOpen(pno);
end;

procedure View.Paint(PaintDC:HDC; var PaintInfo:TPaintStruct);
var
	api:playerIndex;
begin
	inherited Paint(PaintDC, PaintInfo);
	for api:=Low(playerIndex) to High(playerIndex) do if (the_players[api].Participating) then DrawDealerPlate(PaintDC,api, 0, 0);
end;

function View.OnSize(resizeType:uint;newWidth,newHeight:integer):LONG;
var
	api:playerIndex;
	aSpanRect:TRect;
begin
	OnSize:=inherited OnSize(resizeType,newWidth,newHeight);
	if Started then begin
		for api:= 1 to MAX_PLAYERS do begin
			with the_status_notes[api]^ do begin
				hand_props[api].GetSpanRect(aSpanRect);
				MoveWindow(handle,
					Centered(GetWndWd(handle),aSpanRect.left,aSpanRect.right),
					Centered(GetWndHt(handle),aSpanRect.top,aSpanRect.bottom),
					GetWndWd(handle),GetWndHt(handle), TRUE);
			end;
		end;
	end;
end;

constructor View.Construct(background_color:TColorRef;bg_image:HBITMAP;use_image:boolean);
var
	iHand:playerIndex;
begin //WriteLn('View.Construct(background_color:TColorRef;bg_image:HBITMAP;use_image:boolean)');
	the_view:=@self;
	inherited Construct(background_color,bg_image,use_image);
	the_pot.Construct;
	for iHand:=1 to MAX_PLAYERS do hand_props[iHand].Construct(iHand);
end;

destructor View.Done;
var
	iHand:word;
begin //Writeln('View.Done');
	for iHand:= 1 to MAX_PLAYERS do begin
		hand_props[iHand].Destruct;
	end;
end;

function View.RaiseAllowed(aPlayerId:playerIndex;var limits:pokerRaiseLimits):boolean;
begin
	RaiseAllowed:=RaiseIsOk(aPlayerId,limits);
end;

procedure GetRaiseLimits(var limits:pokerRaiseLimits);
begin
	if PlayingDrawPoker
		then limits:=theDrawPokerOptions.RaiseLmts
		else limits:=theStudPokerOptions.RaiseLmts;
end;

function View.CurrentPot(aPlayerId:playerIndex):word;
var
	bets:real;
	player:playerIndex;
begin
	bets:=0;
	for player:=Low(playerIndex) to High(playerIndex) do if (player<>aPlayerId) then with the_players[player] do if Participating then bets:=bets+myBet.Value;
	CurrentPot:=Trunc(the_pot.Value+bets);
end;

{$ifdef TEST}
procedure Test_CurrentPot;
var
	test_view:testable_OView;
begin
	test_view.Construct;
	the_pot.Construct;
	the_pot.TheChips^.AddAmount(22);
	InitializeTestPlayers;
	SetupTestPlayersBet(1,11,TRUE);
	SetupTestPlayersBet(2,12,FALSE);
	SetupTestPlayersBet(3,13,TRUE);
	AssertAreEqual({22+13=}35,test_view.CurrentPot(1));
	SetupTestPlayersBet(4,14,TRUE);
	AssertAreEqual({22+13+14=}49,test_view.CurrentPot(1));
end;
{$endif TEST}

function View.PlayerBetLimit(aPlayerId:playerIndex):word;

var
	limits:pokerRaiseLimits;

	function CurrBetLimit:word;

	begin
		if CurrentPot(aPlayerId)=CurrentAnte
			then CurrBetLimit:=the_table_limit
			else CurrBetLimit:=CurrentPot(aPlayerId)*2;
	end;

begin
	GetRaiseLimits(limits);
	if RaiseAllowed(aPlayerId,limits)
		then PlayerBetLimit:=CurrBetLimit
		else PlayerBetLimit:=the_current_bet;
end;

{$ifdef TEST}

type
	BetLimitView=object(testable_OView)
		myCurrentPot:word;
		myRaiseAllowed:boolean;
		function CurrentPot(aPlayerId:playerIndex):word; virtual;
		function RaiseAllowed(aPlayerId:playerIndex;var limits:pokerRaiseLimits):boolean; virtual;
	end;

function BetLimitView.CurrentPot(aPlayerId:playerIndex):word; 

begin 
	CurrentPot:=myCurrentPot; 
end;

function BetLimitView.RaiseAllowed(aPlayerId:playerIndex;var limits:pokerRaiseLimits):boolean;

begin
	RaiseAllowed:=myRaiseAllowed;
end;

procedure Test_PlayerBetLimit;
const
	ANY_PLAYER=3;
var
	aTester:BetLimitView;
	procedure SetCallBet(amount:word);
	begin
		the_current_bet:=amount;
	end;
	procedure SetupDrawGame(aGameType:pokerGameId;aLimit:word);
	begin
		the_poker_options.game_id:=aGameType;
		theDrawPokerOptions.beforeDrawOpenMax:=aLimit;
		theDrawPokerOptions.afterDrawOpenMax:=aLimit;
	end;
	procedure SetAnteAmount(aAnte:word);
	begin
		theDrawPokerOptions.AnteChips[JACKPOTS]:=aAnte;
		theDrawPokerOptions.AnteChips[ANYTHINGOPENS]:=aAnte;
	end;
begin
	aTester.Construct;
	the_table_limit:=50;
	SetupDrawGame(DRAW_POKER,10);
	SetAnteAmount(0);
	InitializeTestPlayers;
	
	aTester.myCurrentPot:=0;
	aTester.myRaiseAllowed:=true;
	AssertAreEqual(50,aTester.PlayerBetLimit(ANY_PLAYER));
	
	aTester.myCurrentPot:=15;
	SetCallBet(11);
	aTester.myRaiseAllowed:=false;
	AssertAreEqual(11,aTester.PlayerBetLimit(ANY_PLAYER));
	aTester.myRaiseAllowed:=true;
	AssertAreEqual({15*2=}30,aTester.PlayerBetLimit(ANY_PLAYER));
	
	SetAnteAmount(3);
	aTester.myCurrentPot:=3;
	AssertAreEqual(50,aTester.PlayerBetLimit(ANY_PLAYER));
end;
{$endif}

{$ifdef TEST}

type
	OnStackClickedTester=object(TestPlayerChipsProp)
		myTossWasCalled:boolean;
		myCueBadClickWasCalled:boolean;
		myCurrentBetLimit:word;
		function CurrentBetLimit:word; virtual;
		procedure Toss(aChipType:TypeOfChip); virtual;
		procedure CueBadClick; virtual;
	end;

function OnStackClickedTester.CurrentBetLimit:word;

begin
	CurrentBetLimit:=myCurrentBetLimit;
end;

procedure OnStackClickedTester.CueBadClick; begin myCueBadClickWasCalled:=true; end;
procedure OnStackClickedTester.Toss(aChipType:TypeOfChip); begin myTossWasCalled:=true; end;

type
	FakePlayer=object(PokerPlayer)
		myCurrentBet:word;
		constructor Construct;
		function CurrentBet:word; virtual;
	end;

constructor FakePlayer.Construct; begin end;

function FakePlayer.CurrentBet:word;

begin
	CurrentBet:=myCurrentBet;
end;

procedure Test_OnStackClicked;

var
	aTester:OnStackClickedTester;
	aFakePlayer:FakePlayer;

begin
	aTester.Construct;
	aFakePlayer.Construct;
	aFakePlayer.myBet.Construct(@aFakePlayer);
	aTester.owner:=@aFakePlayer;

	aFakePlayer.myCurrentBet:=3;
	aTester.myCurrentBetLimit:=10;
	aTester.myTossWasCalled:=false;
	aTester.OnStackClicked(CHIP3);
	punit.Assert.IsTrue(aTester.myTossWasCalled);

	aTester.myCurrentBetLimit:=7;
	aTester.myTossWasCalled:=false;
	aTester.myCueBadClickWasCalled:=false;
	aTester.OnStackClicked(CHIP3);
	punit.Assert.IsFalse(aTester.myTossWasCalled);
	punit.Assert.IsTrue(aTester.myCueBadClickWasCalled);

	aTester.myCurrentBetLimit:=8;
	aTester.myTossWasCalled:= false;
	aTester.OnStackClicked(CHIP3);
	punit.Assert.IsTrue(aTester.myTossWasCalled);
end;

{$endif TEST}

procedure PokerBetProp.CalcNewAnchorPt(aNewWd,aNewHt:word; var rResult:TPoint);
var
	aCenter:TPoint;
	pt:point32;
begin
	aCenter.x:=(aNewWd div 2);
	aCenter.y:=(aNewHt div 2);
	pt:=PointAlongRay(aCenter.x,aCenter.y,SeatHandRayAngle(SeatNum),0.25);
	rResult.x:=Nudge(aCenter.x+pt.x-(Width div 2),8);
	rResult.y:=Nudge(aCenter.y-pt.y,3);
end;

function PokerBetProp.GetAnchorPoint(table_width,table_height:word):xypair;
var
	aPt:TPoint;
begin
	if table_width > 0 then begin
		CalcNewAnchorPt(table_width, table_height, aPt);
		GetAnchorPoint:=MakeXYPair(aPt.x, aPt.y);
	end;
end;

constructor PokerBetProp.Construct(pPlayer:PokerPlayerP);

begin
	inherited Construct(new(PPileOfChips, Init(10)));
	Player:= pPlayer;
end;

function PokerBetProp.SeatNum:SeatIndex;
begin
	SeatNum:=Player^.pno;
end;

procedure PokerBetProp.OnValueChanged;
begin
	inherited OnValueChanged;
	UpdateMessageLineProxy;
end;

constructor PotProp.Construct;
begin
	inherited Init(new(PPileOfChips, Init(10)));
end;

function PotProp.GetAnchorPoint(table_width,table_height:word):xypair;
begin
	GetAnchorPoint:=MakeXYPair(Nudge((table_width div 2)-(Width div 2),3), Nudge((table_height div 2),3));
end;

procedure PokerBetProp.SnapToPot;

begin
	SnapTo(@the_pot);
	delay(DELAY_ACTION div 2);
end;

procedure PotProp.OnValueChanged;
begin
	inherited OnValueChanged;
	UpdateMessageLineProxy;
	if ChipCount>8 then MinimizeChipCount;
end;

{$ifdef TEST}

type
	testable_OPotProp=object(PotProp)
		constructor Construct;
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure Refresh; virtual;
	end;

constructor testable_OPotProp.Construct;
begin
	TheChips:=New(PPileOfChips,Init(10));
end;

procedure testable_OPotProp.GetSpanRect(var rRect:TRect); begin end;
procedure testable_OPotProp.Refresh; begin end;

procedure Test_PotValueChanged;
var
	aTester:testable_OPotProp;
begin
	aTester.Construct;
	while aTester.ChipCount<8 do aTester.TheChips^.AddAmount(1);
	aTester.OnValueChanged;
	AssertAreEqual(8,aTester.ChipCount);
	aTester.TheChips^.AddAmount(1);
	aTester.OnValueChanged;
	AssertAreEqual(5,aTester.ChipCount);
end;

{$endif}

procedure View.DrawDealerPlate(adc:HDC;aPlayerId:playerIndex;adjustX,adjustY:integer);
var
	text:stringBuffer;
begin
	SetBkMode(adc,TRANSPARENT);
	SetTextAlign(adc,Q(aPlayerId in [4],TA_TOP,TA_BOTTOM) or TA_LEFT);
	StrPCopy(text, '');
	if the_players[aPlayerId].IsDealer then StrCat(text,'(DEALER)');
	SetTextColor(adc,RGB_WHITE);
	TextOut(adc,adjustX+NamePlateX(aPlayerId),adjustY+NamePlateY(aPlayerId),text,StrLen(text));
end;

function View.NamePlateX(aPlayerId:playerIndex):integer;
begin
	NamePlateX:=hand_props[aPlayerId].Left;
end;

function View.NamePlateY(aPlayerId:playerIndex):integer;
begin
	NamePlateY:=Q(aPlayerId in [4],hand_props[aPlayerId].Bottom,hand_props[aPlayerId].Top);
end;

procedure View.GetNamePlateRect(var aRect:TRect;aPlayerId:playerIndex);
begin
	aRect.left:=NamePlateX(aPlayerId);
	aRect.top:=NamePlateY(aPlayerId);
	if not (aPlayerId in [4]) then Dec(aRect.top,25);
	aRect.right:=aRect.left+200;
	aRect.bottom:=aRect.top+25;
end;

function pickDealer:integer;

var
	d:integer;

begin
	{$ifdef TEST_DEALER}
	pickDealer:= USER_ID;
	{$else}
	{$ifdef TEST_OPEN_BUTTON}
	pickDealer:= 7;
	{$else}
	d:=random(MAX_PLAYERS)+1;
	d:=NextClockwiseFrom(playerIndex(d));
	pickDealer:=d;
	{$endif}
	{$endif}
end;

procedure View.NextDealer(currentDealer:playerIndex);

var
	r:TRect;

begin
	theDealerIndex:=0;
	GetNamePlateRect(r,currentDealer);
	RefreshRect(r);
	theDealerIndex:=NextClockwiseFrom(currentDealer);
	GetNamePlateRect(r,playerIndex(theDealerIndex));
	RefreshRect(r);
end;

procedure View.PostCommand(aCommand:UINT;wParam:WPARAM;lParam:LPARAM);

begin
	windows.PostMessage(the_app^.Frame^.MyFrameWindow^.Handle,aCommand,wParam,lParam);
end;

procedure DrawPokerRound;
var
	pn:playerIndex;
begin
	with theDrawPokerOptions do begin
		setWildCards(wType,wildCards);
		SetScoreMode(m_aScoringMode);
	end;
	for pn:=1 to MAX_PLAYERS do begin
		with theDrawPokerOptions do case m_aScoringMode of
			playHigh
			{$ifndef VER1}
			,playLow
			{$endif}:with gngFr[pn] do begin
				dcdd:=true;
				scrMd:=m_aScoringMode;
			end;
			{$ifndef VER1}
			playHighLow:gngFr[pn].dcdd:=false;
			{$endif}
		end;
	end;
	DrawPokerDeal;
	AnteUp(CurrentAnte);
	NewBettingInterval;
	the_current_bet:=theDrawPokerOptions.beforeDrawOpen;
	WhoOpened:=NoneYet;
	DrawStarted:=False;
end;

procedure StartStudPokerRound;
var
	i:integer;
	pn,px:playerIndex;
begin
	case theStudPokerOptions.gType of
		stud5card:handSize:=5;
		stud6card:handSize:=6;
		stud7card:handSize:=7;
		{$ifndef VER1}
		BaseBall:handSize:=11;
		{$endif}
	end;
	studRndNo:=1;
	with theStudPokerOptions do begin
		setWildCards(wType,wildCards);
		setScoreMode(m_aScoringMode);
	end;
	for pn:=1 to MAX_PLAYERS do begin
		with theStudPokerOptions do case m_aScoringMode of
			playHigh
			{$ifndef VER1}
			,playLow
			{$endif}:with gngFr[pn] do begin
				dcdd:=true;
				scrMd:=m_aScoringMode;
			end;
			{$ifndef VER1}
			playHighLow:gngFr[pn].dcdd:=false;
			{$endif}
		end;
	end;
	{ init what each player predicts about the other player's hands }
	for pn:=1 to MAX_PLAYERS do begin
		for i:=1 to maxHandSize do sure[pn,i]:=false;
		for px:=1 to MAX_PLAYERS do with predict[pn,px] do begin
			isBlffng:=false;
			h.n:=0;
			for i:=1 to maxHandSize do assigned[i]:=false;
			IsGngFr.Dcdd:=False;
		end;
	end;
	if (potAmt=0) and (GetStudAnte>0) then AnteUp(GetStudAnte);
	PostMessage(frame_handle,WM_STUDDEAL,0,0);
end;

procedure StartDrawPokerRound;
begin
	DrawPokerRound;
	UpdateMessageLine;
	if PassIsCheck then
		barbPass^.setButtonText('Check')
	else
		barbPass^.setButtonText('Pass');
	InvalidateRect(barbPass^.handle, nil, FALSE);
	UpdateWindow(barbPass^.handle);
	PostMessage(frame_handle, WM_NEXTOPEN, NextPlaying(theDealerIndex), 0);
end;

procedure Deal;
begin
	barbDeal^.disable;
	SetupNewRound;
	if the_poker_options.game_id=DRAW_POKER then
		StartDrawPokerRound
	else
		StartStudPokerRound;
end;

procedure View.OnCall;
begin
	HidePlayerPrompt;
	the_players[USER_ID].myChips.Disable;
	barbCall^.disable;
	barbRaise^.disable;
	barbFold^.disable;
	barbCancel^.Disable;
	PlayerCalled(USER_ID);
	EvaluateCallBy(USER_ID);
end;

procedure View.OnRaise;
begin
	HidePlayerPrompt;
	the_players[USER_ID].myChips.Disable;
	the_last_raise_amount:=Round(the_players[USER_ID].myBet.Value-the_current_bet);
	barbCall^.disable;
	barbRaise^.disable;
	barbFold^.disable;
	barbCancel^.Disable;
	ProcessRaiseByPlayer(USER_ID,the_last_raise_amount);
end;

procedure View.OnOpen;
var
	b:integer;
	mnb,mxb:Word;
begin
	HidePlayerPrompt;
	the_players[USER_ID].myChips.Disable;
	if PlayingDrawPoker then
		with theDrawPokerOptions do begin
			if BeforeTheDraw then begin
				mnb:=beforeDrawopen;
				mxb:=beforeDrawOpenMax;
			end
			else begin
				mnb:=AfterDrawOpen;
				mxb:=AfterDrawOpenMax;
			end;
		end
	else
		with theStudPokerOptions do with m_aRoundBettingLimits do begin
			mnb:=minOpen[studRndNo-frstRnd+1];
			mxb:=maxOpen[studRndNo-frstRnd+1];
		end;
	if mnb<mxb then begin
		barbOpen^.disable;
		barbPass^.disable;
		barbFold^.disable;
	end;
	the_current_bet:=Round(the_players[USER_ID].myBet.Value);
	PlayerOpened(USER_ID);
end;

procedure View.OnFold;
begin
	HidePlayerPrompt;
	the_players[USER_ID].myChips.Disable;
	barbCall^.disable;
	barbRaise^.disable;
	barbFold^.disable;
	PlayerFolded(USER_ID);
end;

procedure View.OnPass;
begin
	HidePlayerPrompt;
	barbOpen^.disable;
	barbPass^.disable;
	barbFold^.disable;
	barbCancel^.Disable;
	PlayerPassed(USER_ID);
	EvaluatePassBy(USER_ID);
end;

procedure View.RestoreButtonStates;
begin
	EnableOpen(savedOpenState);
	EnablePass(savedPassState);
	EnableCall(savedCallState);
	EnableRaise(savedRaiseState);
	EnableFold(savedFoldState);
end;

procedure View.OnCancel;
begin
	HidePlayerPrompt;
	with the_players[USER_ID] do myBet.TransferTo(@myChips);
	barbCancel^.Disable;
	RestoreButtonStates;
end;

procedure TDealButton.OnEnabled;
begin
	barbOpen^.disable;
	barbPass^.disable;
	barbCall^.disable;
	the_players[USER_ID].myChips.Disable;
	barbRaise^.disable;
	barbFold^.disable;
	barbCancel^.Disable;
end;

procedure defaultWallet(po:playerIndex);
begin
	the_players[po].orgMny:=the_table_stakes;
	the_players[po].myChips.SetAmount(the_table_stakes);
end;

procedure PreGame;
var
	p:playerIndex;
begin
	Randomize;
	theDealerIndex:=PickDealer;
	RoundCount:=0;
	for p:=1 to MAX_PLAYERS do begin
		DefaultWallet(p);
		the_players[p].my_house_loan:=0;
	end;
	
	// hack: necessary to force a redraw of the player name plates, need a better solution
	InvalidateRect(the_view^.handle,nil,FALSE);
	UpdateWindow(the_view^.handle);
end;

procedure View.StartDraw;
begin //writeln('View.StartDraw');
	barbPass^.setButtonText('Pass');
	the_poker_options.game_id:=DRAW_POKER;
	handSize:=5;
	with theDrawPokerOptions do setWildCards(wType,wildCards);
	SetGlobals;
end;

procedure View.StartStud;
begin //writeln('View.StartStud');
	barbPass^.setButtonText('Check');
	the_poker_options.game_id:=STUD_POKER;
	with theStudPokerOptions do setWildCards(wType,wildCards);
	case theStudPokerOptions.gType of
		stud5card:handSize:=5;
		stud6card:handSize:=6;
		stud7card:handSize:=7;
		{$ifndef VER1}
		baseball:handSize:=11;
		{$endif}
	end;
	SetGlobals;
end;

function View.Create(frame:HWND;w,h:number):HWND;
begin //writeln('View.Create(frame,',w,',',h,');');
	frame_handle:=frame;
	Create:=inherited Create(frame,w,h);
	SelectDesiredCardWidth(85);
	SetupForOdds;
end;

procedure View.Startup;
var
	i,pn:integer;
begin //writeln('View.Startup');
	barbDeal^.Create(the_toolbar,'Deal',0);
	theMessageLine.Create(the_toolbar,0,120,'Welcome',BB_SHIFT);
	barbPass^.Create(the_toolbar,'Pass',BB_SHIFT);
	barbFold^.Create(the_toolbar,'Fold',0);
	barbCancel^.Create(the_toolbar,'X',BB_SHIFT);
	barbOpen^.Create(the_toolbar,'Open',BB_SHIFT );
	barbCall^.Create(the_toolbar,'Call',0);
	barbRaise^.Create(the_toolbar,'Raise',0);
	barbOpen^.disable;
	barbPass^.disable;
	barbCall^.disable;
	barbRaise^.disable;
	barbFold^.disable;
	barbCancel^.Disable;	
	if the_poker_options.game_id=DRAW_POKER then
		StartDraw
	else
		StartStud;
	DefaultPokerGame;
	for pn:= 1 to MAX_PLAYERS do begin
		i:= CircInt(1, MAX_PLAYERS, 5, pn);
		with ViewP(the_view)^ do begin
			if the_players[i].Participating then begin
				hand_props[i].Show;
				the_players[i].myChips.Show;
				the_players[i].myBet.Show;
			end
			else begin
				hand_props[i].Hide;
				the_players[i].myChips.Hide;
				the_players[i].myBet.Hide;
			end;
			with hand_props[i] do SetPosition(GetAnchorPoint(ClientAreaWd, ClientAreaHt));
			AddProp(@hand_props[i]);
			with the_players[i].myChips do SetPosition(GetAnchorPoint(ClientAreaWd, ClientAreaHt));
			AddProp(@the_players[i].myChips);
			with the_players[i].myBet do SetPosition(GetAnchorPoint(ClientAreaWd, ClientAreaHt));
			AddProp(@the_players[i].myBet);
			if i = 5 then begin
				with the_pot do SetPosition(GetAnchorPoint(ClientAreaWd, ClientAreaHt));
				ViewP(the_view)^.AddProp(@the_pot);
				the_pot.Show;
			end;
		end;
	end;
	for pn:=1 to MAX_PLAYERS do the_status_notes[pn]:=New(PStatusNote,Init(@self,pn));
end;

procedure View.OnNextBet(aPlayerId:playerIndex);
var
	pIndex:playerIndex;
begin
	HidePlayerPrompt;
	pIndex:=aPlayerId;
	with the_players[pIndex] do if status<>FOLDED then begin
		if pIndex=USER_ID then begin
			barbCall^.Enable;
			barbFold^.Enable;
			myChips.Enable;
			player_prompt.SetText('Your turn to Bet (click on your chips to Raise)');
			player_prompt.Show;
		end
		else begin
			if ShouldStay(pIndex) then
				if ShouldRaise then begin
					ProcessRaiseByPlayer(pIndex,the_last_raise_amount);
				end
				else begin
					PlayerCalled(pIndex);
					EvaluateCallBy(pIndex);
				end
			else begin
				PlayerFolded(pIndex);
			end;
		end;
	end;
	delay(DELAY_ACTION);
end;

procedure View.OnNextOpen(aPlayerId:playerIndex);
{ Next player to open. Set "WhoOpened" to the index of the opener (1..n) or 0 if no one OPENED. }
var
	player:playerIndex;
	i:integer;
begin
	HidePlayerPrompt;
	player:=aPlayerId; { player to open }
	with the_players[player] do begin
		if PlayingStudPoker then status:=NOSTATUS;
		{ conditions for human user to choose }
		if
			(player=USER_ID)
			and
				(
					(
						PlayingDrawPoker and
							(
								 AnythingOpensVar or AfterTheDraw
								 or
								 (
									 JackpotsVar
									 and
									 (
										 (the_hand_valuations[USER_ID].eh.rslt>ONE_PAIR)
										 or
										 (
											 (the_hand_valuations[USER_ID].eh.rslt=ONE_PAIR)
											 and
											 (Rank(the_hand_valuations[USER_ID].eh.Pip)>=Rank(JackPip))
									 )
								)
							)
						)
					)
					or
					(PlayingStudPoker and (StudRndNo>=FrstRnd))
				)
			then begin
				barbFold^.enabled(not PassAllowed);
				barbPass^.enabled(PassAllowed);
				if PassIsCheck then
					barbPass^.setButtonText('Check')
				else
					barbPass^.setButtonText('Pass');
				the_players[USER_ID].myChips.Enable;
				player_prompt.SetText('Your turn to Open (click on your chips to Bet)');
				player_prompt.Show;
			end
		else if the_players[player].ShouldOpen then begin
			PlayerOpened(player);
		end
		else if PassAllowed then begin
			PlayerPassed(player);
			EvaluatePassBy(player);
		end
		else begin
			PlayerFolded(player);
		end;
	end;
end;

procedure  View.NextDraw(aPlayerId:playerIndex);
var
	pn,px:playerIndex;
	aDrawDlg:TDrawDlg;

	procedure evalDrawHigh(var e:pokerHandEvaluation);
	begin
		case nCardsDrwn[pn] of
			0:{ for high play must be a straight or better }
				predictHand(px,pn,e,straight,fivePip,flush,acePip,[]);
			1:if JackpotsVar and (playerHistory(pn,PASSED)) then
					{ a very good chance he has a short hand }
					{ ??? }
				else
					{ player did not pass on the first round, so he may or
						may not have a short hand }
					predictHand(px,pn,e,TWO_PAIR,fivePip,TWO_PAIR,acePip,[]);
			2:predictHand(px,pn,e,ONE_PAIR,jackPip,THREE_KIND,eightPip,[TWO_PAIR]);
			3:{ in high play drawing 3 cards indicates a pair }
				predictHand(px,pn,e,ONE_PAIR,eightPip,ONE_PAIR,acePip,[]);
			4,5:
				predictHand(px,pn,e,RUNT,queenPip,ONE_PAIR,acePip,[]);
		end;
	end;

	procedure evalDrawLow(var e:pokerHandEvaluation);
	begin
		case nCardsDrwn[pn] of
			0:{ for Low play must be a runt }
				predictHand(px,pn,e,RUNT,acePip,RUNT,acePip,[]);
			1:{ in Low play it means he either has a pair and is
					getting rid of one of them or a high runt that he is
					trying to improve. }
				predictHand(px,pn,e,ONE_PAIR,kingPip,RUNT,acePip,[]);
			2:{ in low play drawing 2 indicates a high one pair or
					two pair or three of a kind }
				predictHand(px,pn,e,THREE_KIND,kingPip,ONE_PAIR,tenPip,[]);
			3:{ in low play drawing 3 means you have 4 of a kind or
					a full house and you are trying to break it }
				predictHand(px,pn,e,FOUR_KIND,kingPip,FULLHOUSE,acePip,[]);
			4,5:
				{ in low play means you have 4 of a kind or a full house
					although I doubt we would ever get here in low play }
				predictHand(px,pn,e,FOUR_KIND,kingPip,FULLHOUSE,acePip,[]);
		end;
	end;

	procedure evalDrawHighLow(var e:pokerHandEvaluation);
	{ Evaluate what a Draw by player "pn" means to his opponent "px". }
	var
		HSet:PokerHandTypeSet;
	begin
		with Predict[px,pn] do begin
			case nCardsDrwn[pn] of
				0,1:begin
					if NCardsDrwn[pn]=0 then
						HSet:=[Straight..FIVE_KIND]
					else
						HSet:=[TWO_PAIR,THREE_KIND,FOUR_KIND];
					if RandomReal <= GetHandOdds(HSet) then begin
						setEvaluationMode(HighEval);
						evalDrawHigh(eval.eh);
						with IsGngFr do begin
							Dcdd:=True;
							ScrMd:=PlayHigh;
						end;
					end
					{$ifndef VER1}
					else begin
						setEvaluationMode(LowEval);
						evalDrawLow(eval.el);
						with IsGngFr do begin
							Dcdd:=True;
							{$ifndef VER1}
							ScrMd:=PlayLow;
							{$endif}
						end;
					end
					{$endif};
					setEvaluationMode(highEval);
				end
				else begin
					setEvaluationMode(HighEval);
					evalDrawHigh(eval.eh);
					with IsGngFr do begin
						Dcdd:=True;
						ScrMd:=PlayHigh;
					end;
				end;
			end;
		end;
	end;

begin
	DrawStarted:=True;
	pn:=aPlayerId;
	if the_players[pn].status<>FOLDED then begin
		if pn=USER_ID then begin
			aDrawDlg.Init;
			aDrawDlg.Modal;
			windows.UpdateWindow(self.handle);
			HumanDraw;
		end
		else begin
			drawFor(pn);
			pokerResult(theHands[pn], the_hand_valuations[pn]);
			{ Decide again if player PN should go for the bluff or not }
			if
				(GetScoreMode=PlayHigh)
				and
				(the_hand_valuations[PN].EH.Rslt=RUNT)
				and
				(Random(1000)<5)
				then
				the_players[PN].GoForBlf:=True;
		end;
		{$ifndef VER1}
		{ if the hand changed modes than adjust what the player is going for }
		if GetScoreMode=PlayHighLow then begin
			if the_hand_valuations[pn].eh.rslt>ONE_PAIR then with gngFr[pn] do begin
				dcdd:=true;
				scrMd:=playHigh;
			end
			else if the_hand_valuations[pn].el.rslt=RUNT then with gngFr[pn] do begin
				dcdd:=true;
				scrMd:=playLow;
			end;
		end;
		{$endif}
		{ update what each opponent thinks player "pn" has based on his draw }
		ThinkMsgOn;
		px:=pn;
		px:=playerIndex(nextPlaying(Integer(px)));
		repeat
			with predict[px,pn] do case theDrawPokerOptions.m_aScoringMode of
				playHigh:evalDrawHigh(eval.eh);
				{$ifndef VER1}
				playLow:evalDrawLow(eval.eh);
				playHighLow:evalDrawHighLow(eval.eh)
				{$endif}
			end;
			px:=playerIndex(nextPlaying(Integer(px)));
		until px=pn;
		ThinkMsgOff;
	end
	else if pn=theDealerIndex then
		EndTheDraw
	else
		windows.PostMessage(GetParent,wm_NextDraw,NextClockwiseFrom(pn),0);
end;

procedure View.OnShowdown;
begin
	ClearAllStatus;
	if USER_ID>0 then if PlayingDrawPoker then
		PokerResult(theHands[USER_ID], the_hand_valuations[USER_ID])
	else
		StudPokerResult(theHands[USER_ID], the_hand_valuations[USER_ID]);
	if PlayingDrawPoker then with theDrawPokerOptions do ShowDown(
		(m_aScoringMode=playHigh)
		{$ifndef VER1}
		or
		(m_aScoringMode=playHighLow)
		{$endif},
		{$ifndef VER1}
		(m_aScoringMode=playLow) or (m_aScoringMode=playHighLow)
		{$else}
		FALSE
		{$endif}
		)
	else with theStudPokerOptions do ShowDown(
		(m_aScoringMode=playHigh)
		{$ifndef VER1}
		or
		(m_aScoringMode=playHighLow)
		{$endif},
		{$ifndef VER1}
		(m_aScoringMode=playLow) or (m_aScoringMode=playHighLow)
		{$else}
		FALSE
		{$endif}
		);
end;

procedure View.OnStudDeal;
var
	First,pn,px:playerIndex;
	c:playingCard;
	hnd:handType;

	procedure adjustForDeal(pn:playerIndex);
	{ Adjust the player's knowledge for the card just dealt to "pn". }
	var
		px:playerIndex;
	begin
		if IsUpCardRound(studRndNo) then sure[pn,theHands[pn].n]:=true;
		{ Adjust the card count array for player "pn" for the card just dealt
			to him. }
		if the_card_pool[pn][CardPip(theHands[pn].card[theHands[pn].n]), CardSuit(theHands[pn].card[theHands[pn].n])]>0 then
			dec(the_card_pool[pn][CardPip(theHands[pn].card[theHands[pn].n]), CardSuit(theHands[pn].card[theHands[pn].n])]);
		assignedTo[pn][CardPip(theHands[pn].card[theHands[pn].n]), CardSuit(theHands[pn].card[theHands[pn].n])]:=pn;
		{ adjust each of "pn"'s opponents knowledge about "pn"'s hand from
			the card just dealt to him }
		for px:=1 to MAX_PLAYERS do if (px<>pn) then with the_players[px] do if isPlaying then begin
			{ increment the card count in player "pn"'s predicted hand }
			inc(predict[px,pn].h.n);
			if IsUpCardRound(studRndNo) then begin
				{ Card just dealt was an upcard so adjust opponents card counts. }
				c:=theHands[pn].card[theHands[pn].n];
				if the_card_pool[px][CardPip(c), CardSuit(c)]>0 then
					dec(the_card_pool[px][CardPip(c), CardSuit(c)]);
				assignedTo[px][CardPip(c), CardSuit(c)]:=pn;
				{ record this card as known for sure by each opponent }
				with predict[px,pn] do begin
					assigned[h.n]:=true;
					h.card[h.n]:=theHands[pn].card[theHands[pn].n];
				end;
			end;
		end;
	end;

begin
	if (StudRndNo<=LastRnd) and (not stop) then begin
		first:=nextPlaying(playerIndex(theDealerIndex));
		pn:=first;
		{Cursor:=SetCursor(LoadCursor(0,idc_Wait));}
		repeat
			the_players[pn].status:=NOSTATUS;
			if studRndNo=1 then begin
				theHands[pn].n:=0;
				the_view^.hand_props[pn].ThePile^.Empty;
			end;
			{ deal the next card }
			with the_players[pn] do begin
				inc(theHands[pn].n);
				theHands[pn].card[theHands[pn].n]:= theDeck[deckIndex];
				the_view^.hand_props[pn].ThePile^.Add(theDeck[deckIndex]);
				if IsUpCardRound(studRndNo) or (pn=USER_ID) then the_view^.hand_props[pn].ThePile^.FlipTop;
			end;
			if deckIndex>1 then dec(deckIndex);
			if IsUpCardRound(studRndNo) then sure[pn,theHands[pn].n]:=true;
			the_view^.hand_props[pn].Refresh;
			SndCardFlick;
			if the_poker_options.speedDeal then delay(cardPlayDelay);
			adjustForDeal(pn);

			{ in a game of Baseball check for a 3 or 4 upcard }

			{$ifdef BASEBALL}
			if (theStudPokerOptions.gType=baseball) and (IsUpCardRound(studRndNo)) then begin
				if CardPip(hand[pn].card[hand[pn].n])=threePip then begin
					if studRndNo=(frstRnd-1) then EraseDealer;
					ThinkMsgOn;
					UpdateEvaluation;
					ThinkMsgOff;
					{$ifdef LOG}
					writeln;
					LogInfo(pn);
					{$endif}
					if MatchThePot(pn) then begin
						PlayerMatched(pn);
						EvaluateMatch(pn);
					end
					else begin
						PlayerFolded(pn);
						if pn=first then first:=PrevPlaying(pn);
					end;
				end
				else if CardPip(hand[pn].card[hand[pn].n])=fourPip then begin
					{ a 4 up means deal him an extra hole card }
					{$ifndef ANAL}
					delay(DELAY_ACTION);
					{$endif}
					with hand[pn] do begin
						inc(n);
						card[n]:=theDeck[deckIndex];
					end;
					dec(deckIndex);
					sure[pn,hand[pn].n]:=false;
					{$ifndef ANAL}
					dspPlayer(pn);
					SndCardFlick;
					{adjustForDeal(pn);}
					if the_poker_options.speedDeal then delay(cardPlayDelay);
					{$endif}
				end;
			end;
			{$endif}
			pn:=nextPlaying(pn);
		until (pn=first) or stop;
		if IsUpCardRound(studRndNo) then delay(DELAY_ACTION); { give human time to view the up cards }
		{SetCursor(Cursor);}
		if (not stop) and (studRndNo>(frstRnd-1)) then begin
			thinkMsgOn;
			UpdateEvaluation;
			for pn:=1 to MAX_PLAYERS do with the_players[pn] do if isPlaying then begin
				if IsUpCardRound(studRndNo) then begin
					{ evaluate what each opponent thinks player "pn" has }
					for px:=1 to MAX_PLAYERS do if (px<>pn) then with the_players[px] do if isPlaying then
						with predict[px,pn] do begin
							makePredHand(px,pn,hnd);
							studPokerResult(hnd,eval);
						end;
				end;
			end;
			thinkMsgOff;
		end;
		{ Is this a betting round? }
		if (not stop) and (studRndNo>(frstRnd-1)) then begin
			{ This is a betting round. }
			{NewHistory;}
			ThinkMsgOn;
			UpdateEvaluation;
			ThinkMsgOff;
			with theStudPokerOptions do with m_aRoundBettingLimits do the_current_bet:= minOpen[studRndNo-frstRnd+1];
			whoLastBet:=highestShowing(frstRnd,min(studRndNo,lastUpNo));
			delay(DELAY_ACTION);
			NewBettingInterval;
			InitRaiseLmts;
			windows.PostMessage(GetParent,WM_NEXTOPEN,WhoLastBet,0);
		end
		else begin
			inc(studRndNo);
			windows.PostMessage(GetParent,WM_STUDDEAL,0,0);
		end;
	end
	else begin
		delay(DELAY_ACTION);
		windows.PostMessage(GetParent,WM_SHOWDOWN,0,0);
	end;
end;

function View.CallEnabled:boolean;
begin
	CallEnabled:=barbCall^.IsEnabled;
end;

function View.RaiseEnabled:boolean;
begin
	RaiseEnabled:=barbRaise^.IsEnabled;
end;

function View.OpenEnabled:boolean;
begin
	openEnabled:=barbOpen^.IsEnabled;
end;

procedure View.EnableOpen(value:boolean);
begin
	if value
		then barbOpen^.Enable
		else barbOpen^.Disable;
end;

procedure View.EnablePass(value:boolean);
begin
	if value
		then barbPass^.Enable
		else barbPass^.Disable;
end;

procedure View.EnableRaise(value:boolean);
begin
	if value
		then barbRaise^.Enable
		else barbRaise^.Disable;
end;

procedure View.EnableFold(value:boolean);
begin
	if value
		then barbFold^.Enable
		else barbFold^.Disable;
end;

procedure View.EnableCall(value:boolean);
begin
	if value
		then barbCall^.Enable
		else barbCall^.Disable;
end;

procedure View.HidePlayerPrompt;
begin
	player_prompt.Hide;
end;

procedure View.OnChipTossed(aPlayersBet:word);
begin
	HidePlayerPrompt;
	EnableOpen(not CallEnabled and not RaiseEnabled);
	if not OpenEnabled then begin
		EnableRaise(aPlayersBet>the_current_bet);
		EnableCall(not RaiseEnabled and (aPlayersBet<=the_current_bet));
	end;
	EnablePass(false);
	EnableFold(false);
end;

{$ifdef TEST}
type
	TestView=object(View)
		EnableCallState:boolean;
		EnableFoldState:boolean;
		EnableOpenState:boolean;
		EnableRaiseState:boolean;
		EnablePassState:boolean;
		constructor Init;
		function CallEnabled:boolean; virtual;
		function OpenEnabled:boolean; virtual;
		function RaiseEnabled:boolean; virtual;
		procedure EnableFold(value:boolean); virtual;
		procedure EnableCall(value:boolean); virtual;
		procedure EnableOpen(value:boolean); virtual;
		procedure EnablePass(value:boolean); virtual;
		procedure EnableRaise(value:boolean); virtual;
		procedure HidePlayerPrompt; virtual;
	end;

constructor TestView.Init; 
begin 
end;

function TestView.CallEnabled:boolean; begin CallEnabled:=EnableCallState; end;
function TestView.OpenEnabled:boolean; begin OpenEnabled:=EnableOpenState; end;
function TestView.RaiseEnabled:boolean; begin RaiseEnabled:=EnableRaiseState; end;
procedure TestView.EnableFold(value:boolean); begin EnableFoldState:=value; end;
procedure TestView.EnableOpen(value:boolean); begin EnableOpenState:=value; end;
procedure TestView.EnableRaise(value:boolean); begin EnableRaiseState:=value; end;
procedure TestView.EnableCall(value:boolean); begin EnableCallState:=value; end;
procedure TestView.EnablePass(value:boolean); begin EnablePassState:=value; end;
procedure TestView.HidePlayerPrompt; begin end;

procedure Test_OnChipTossed;
var
	aFrame:TestView;

	procedure SetCurrentBet(bet:word);
	begin
		the_current_bet:=bet;
	end;

begin
	aFrame.Init;
	SetCurrentBet(10);

	aFrame.EnableCallState:=true;
	aFrame.EnableRaiseState:=false;
	aFrame.OnChipTossed(11);
	punit.Assert.IsFalse(aFrame.EnableCallState);
	punit.Assert.IsTrue(aFrame.EnableRaiseState);

	aFrame.EnableCallState:=true;
	aFrame.EnableRaiseState:=false;
	aFrame.OnChipTossed(10);
	punit.Assert.IsTrue(aFrame.EnableCallState);
	punit.Assert.IsFalse(aFrame.EnableRaiseState);

	aFrame.EnableCallState:=true;
	aFrame.EnableRaiseState:=false;
	aFrame.OnChipTossed(9);
	punit.Assert.IsTrue(aFrame.EnableCallState);
	punit.Assert.IsFalse(aFrame.EnableRaiseState);

	aFrame.EnableOpenState:=false;
	aFrame.EnableCallState:=false;
	aFrame.EnableRaiseState:=false;
	aFrame.OnChipTossed(1);
	punit.Assert.IsTrue(aFrame.EnableOpenState);
	punit.Assert.IsFalse(aFrame.EnableCallState);
	punit.Assert.IsFalse(aFrame.EnableRaiseState);

	SetCurrentBet(2);
	aFrame.EnableOpenState:=false;
	aFrame.EnableCallState:=true;
	aFrame.EnableRaiseState:=false;
	aFrame.OnChipTossed(2);
	punit.Assert.IsFalse(aFrame.EnableOpenState);
	punit.Assert.IsTrue(aFrame.EnableCallState);
	punit.Assert.IsFalse(aFrame.EnableRaiseState);

	SetCurrentBet(3);
	aFrame.EnableOpenState:=false;
	aFrame.EnableCallState:=false;
	aFrame.EnableRaiseState:=true;
	aFrame.OnChipTossed(1);
	punit.Assert.IsFalse(aFrame.EnableOpenState);
	punit.Assert.IsTrue(aFrame.EnableCallState);
	punit.Assert.IsFalse(aFrame.EnableRaiseState);
end;

procedure Test_disabling_pass_OnChipTossed;
var
	aFrame:TestView;
	procedure SetCurrentBet(bet:word); begin the_current_bet:=bet; end;
begin
	aFrame.Init;
	SetCurrentBet(5);
	aFrame.EnablePassState:=true;
	aFrame.OnChipTossed(5);
	punit.Assert.IsFalse(aFrame.EnablePassState);
end;
{$endif}

function OPlayerStatusWart.GetContent:string;
var
	myText:stringbuffer;
	note:PStatusNote;
begin //writeln('OPlayerStatusWart.GetContent');
	GetContent:=EMPTY_STRING;
	note:=the_status_notes[PPokerHandProp(self.Parent)^.myPlayerId];
	if note<>NIL then with note^ do begin
		GetWindowText(handle, myText, sizeof(stringbuffer));
		GetContent:=StrPas(myText);
	end;
end;

constructor OPlayerNameTag.Construct(parent_prop:PHotspot; nickname:string);
begin
	inherited Construct(parent_prop);
	self.nickname:=nickname;
end;

function OPlayerNameTag.GetContent:string;
begin
	GetContent:=' ' + self.nickname + ' ';
end;

begin
{$ifdef TEST}
	Suite.Add(@Test_PotAmt);
	Suite.Add(@Test_OnStackClicked);
	Suite.Add(@Test_PotValueChanged);
	Suite.Add(@Test_PopChip);
	Suite.Add(@Test_PlayerBetLimit);
	Suite.Add(@Test_SelectRandomOpponents);
	Suite.Add(@Test_YouAreBrokeMessage);
	Suite.Add(@Test_CopyFromHandData);
	Suite.Add(@Test_CurrentPot);
	Suite.Add(@Test_player_NetWinningsAsText);
	Suite.Add(@Test_player_Borrow);
	Suite.Add(@Test_MessageLine);
	Suite.Add(@Test_OnChipTossed);
//	Suite.Add(@Test_disabling_pass_OnChipTossed);
//	Suite.Add(@Test_Player_Raise);
	Suite.Run('winpkrtbl');
{$else}
	barbDeal:=New(PDealButton,Init(CM_DEAL));
	barbOpen:=New(PBarButton,Init(CM_OPEN));
	barbPass:=New(PBarButton,Init(CM_PASS));
	barbCall:=New(PBarButton,Init(CM_CALL));
	barbRaise:=New(PBarButton,Init(CM_RAISE));
	barbFold:=New(PBarButton,Init(CM_FOLD));
	barbCancel:=New(PBarButton,Init(CM_CANCELBET));
	theMessageLine.Init(NIL,'');
{$endif}
end.