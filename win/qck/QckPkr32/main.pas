{ (C) 1999 Wesley Steiner }

{$MODE FPC}

{$ifdef DEBUG}
{define AUTOPLAY_HAND}
{define SHOW_CARDS}
{$endif}

program QuickPoker;

{$I platform}

{$R dialogs.res}
{$R menus.res}
{$R main.res}

uses
	{$ifdef TEST} punit, {$endif}
	objects,
	strings,
	windows,
	windowsx,
	std,
	stringsx,
	sdkex,
	cards,
	poker,
	pokerTable,
	cardFactory,
	oapp,
	owindows,
	odlg,
	quick,
	winqcktbl, {$ifdef TEST} winqcktbl_tests, {$endif}
	winpkrtbl, {$ifdef TEST} winpkrtbl_tests, {$endif}
	winCardFactory,
	stdwin,
	toolbars,
	quickWin,
	winTabletopChips,
	casview,
	casino,
	pokerlib,
	drawplib,
	studplib,
	qpdlgs;

const
	REGKEY_DRAW='Draw';
	REGKEY_STUD='Stud';
	REGKEY_DRAW_VARIATION:array[drawPokerVar] of pchar=(REGKEY_DRAW+'\AnythingOpens',REGKEY_DRAW+'\Jackpots');
	REGKEY_STUD_VARIATION:array[studPokerVar] of pchar=(REGKEY_STUD+'\5-Card',REGKEY_STUD+'\6-Card',REGKEY_STUD+'\7-Card');

	KEY_GAMEID='GameId';
	KEY_VARIATION='Variation';
	KEY_ANTE='Ante';
	KEY_SHOWSTATS='ShowStatsDialog';
	KEY_TABLESTAKES='TableStakes';
	KEY_TABLELIMIT='TableLimit';

	fdStudAnte:array[StudPokerVar] of Word=(0, 0, 0 {$ifndef VER1} ,2 {$endif});

	SHUFFLES=7; { number of shuffles }

type
	MainAppP=^MainApp;
	MainFrameP=^MainFrame;

	FrameWindowP=^FrameWindow;
	FrameWindow=object(quickWin.FrameWindow)
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		function OnCmd(aCmdId:UINT):LONG; virtual;
	end;
	
	MainFrame=object(quickWin.Frame)
		constructor Construct(app:MainAppP);
		destructor Done; virtual;
		function Tabletop:winpkrtbl.ViewP;
		function OnNextOpen(aPlayerId:playerIndex):LONG;
		function OnNextBet(aPlayerId:playerIndex):LONG;
		function WmNextDraw(aPlayerId:playerIndex):LONG;
		function WmStudDeal:LONG;
		function OnOptionsDraw:LONG;
		function CMOptionsStud:LONG;
		function OnGameDraw:LONG;
		function OnCallCmd:LONG;
		function OnCancel:LONG;
		function OnCancelCmd:LONG;
		function OnDeal:LONG;
		function OnFoldCmd:LONG;
		function OnGameStud:LONG;
		function OnOpenCmd:LONG;
		function OnPassCmd:LONG;
		function OnRaiseCmd:LONG;
		function OnShowStats:LONG;
		{$ifndef VER1}
		procedure CMHistory(var Msg:TMessage); virtual CM_FIRST+CM_HISTORY;
		procedure CMOdds(var Msg:TMessage); virtual CM_FIRST+CM_ODDS;
		{$endif}

		function OnPass:LONG;
		function OnOpen:LONG;
		function OnFold:LONG;
		function OnCall:LONG;
		function OnRaise:LONG;
		function WMShowdown:LONG;
		function OnNextDealer:LONG;
		function WMDeal:LONG;
		function WMDealButton:LONG;
		function WMStart:LONG;
		function FileNewOk:boolean; virtual;
		procedure UpdateAppTitle;
	private
		function Title:string;
	end;

	MainApp=object(quickWin.Application)
		constructor Construct;
		function Frame:MainFrameP;
		function HomePageUrl:pchar; virtual;
		procedure AbortSession; virtual;
		procedure InitMainWindow; virtual;
		procedure AbortCurrentRound;
		{$ifndef VER1}
		function IdleAction:boolean; virtual;
		procedure Save; virtual;
		{$endif}
	end;

const
	PokerGameTypeText:array[pokerGameId] of PChar = ('Draw','Stud');
	BPINIKeyPass:array[0..1] of PChar=('PassBefore','PassAfter');
	PokerGameTitle:array[pokerGameId] of pchar=('Draw Poker','Stud Poker');

var
	the_app:MainAppP;
	showStartPanel:boolean; { display the start panel after exiting main menu }
	MApp:MainApp;

{$ifndef FREE_VER}
function SelectPlayer(x,y:integer):integer;
{$endif}

function TheMainFrame:MainFrameP;

begin
	TheMainFrame:=MainAppP(theApplication)^.Frame;
end;

constructor Mainframe.Construct(app:MainAppP);
begin //writeln('Mainframe.Construct(app:MainAppP)');
	inherited Init(app,TRUE);
end;

procedure MainApp.InitMainWindow;
begin //writeln('MainApp.InitMainWindow');
	MainWindow:=New(MainFrameP,Construct(@self));
	MainWindow^.MyFrameWindow:=New(FrameWindowP,Construct);
	MainWindow^.Create;
	Frame^.SetTabletopWindow(PTabletop(New(winpkrtbl.ViewP,Construct(RGB(0,64,0),
		LoadBitmapFromFile(PChar(GetStringData(KEY_TABLETOP,KEY_TABLETOP_IMAGEPATH,''))),
		GetBooleanData(KEY_TABLETOP,KEY_TABLETOP_USEIMAGE,FALSE)))));
end;

(*
procedure ReadINIWilds(INIf,INIs:PChar;var w:WildCardList);

	var
		WildStr:array[0..53] of Char; { '00001011000' }
		tp:TPip;ts:TSuit;
		i:integer;

	begin
		ReadINIString(INIf,INIs,'Wilds','',WildStr);
		for ts:=ClubSuit to SpadeSuit do for tp:=AcePip to KingPip do begin
			i:=(Ord(ts)-Ord(ClubSuit))*13+(Ord(tp)-Ord(AcePip));
			w.wList[tp,ts]:=
				(StrLen(WildStr)>=i+1)
				and
				(WildStr[i]='1');
		end;
		w.jWild:=(StrLen(WildStr)>=53) and (WildStr[52]='1');
	end;
*)

procedure loadPokerOptions;

const
	fdPasses:array[DrawPokerVar,0..1] of boolean=(
		(False,True), { anyhing opens }
		(True,True) { jackpots }
		);

var
	i:integer;
	n:Word;
	g:studPokerVar;
	pstr:stringBuffer;
	dv:DrawPokerVar;
	sv:StudPokerVar;

begin
	n:=Word(the_app^.GetIntegerDataRange(REGKEY_ROOT,KEY_GAMEID,0,Ord(High(pokerGameId)),Ord(DRAW_POKER)));
	the_poker_options.game_id:=pokerGameId(n);
	with the_app^ do begin
		with theDrawPokerOptions do begin
			n:=Word(GetIntegerDataRange(REGKEY_DRAW,KEY_VARIATION,0,Ord(High(DrawPokerVar)),Ord(ANYTHINGOPENS)));
			gType:=DrawPokerVar(n);
			for dv:=Low(DrawPokerVar) to High(DrawPokerVar) do for i:=0 to 1 do
				PassOK[dv,i]:=GetBooleanData(REGKEY_DRAW_VARIATION[dv],BPINIKeyPass[i],fdPasses[dv,i]);
			AnteChips[JACKPOTS]:=Word(GetIntegerDataRange(REGKEY_DRAW_VARIATION[JACKPOTS],KEY_ANTE,ANTE_EDITBOX_MIN,ANTE_EDITBOX_MAX,2));
			AnteChips[ANYTHINGOPENS]:=Word(GetIntegerDataRange(REGKEY_DRAW_VARIATION[ANYTHINGOPENS],KEY_ANTE,ANTE_EDITBOX_MIN,ANTE_EDITBOX_MAX,2));
			BeforeDrawOpen:=1;
			BeforeDrawOpenMax:=the_table_limit;
			AfterDrawOpen:=2;
			AfterDrawOpenMax:=the_table_limit;
			with RaiseLmts do begin
				lmtPerRnd:=Word(GetIntegerDataRange(REGKEY_DRAW,'Raises',0,999,3));
				lmtPerPsn:=Word(GetIntegerDataRange(REGKEY_DRAW,'RaisesEach',0,999,3));
			end;
		end;
		with theStudPokerOptions do begin
			n:=Word(GetIntegerDataRange(REGKEY_STUD,KEY_VARIATION,0,Word(High(StudPokerVar)),Ord(Stud5Card)));
			gType:=StudPokerVar(n);
			for sv:=Low(StudPokerVar) to High(StudPokerVar) do 
				AnteChips[sv]:=Word(GetIntegerDataRange(REGKEY_STUD_VARIATION[sv],KEY_ANTE,0,999,fdStudAnte[sv]));
			with m_aRoundBettingLimits do for i:=1 to StudPLib.MAX_BETTING_ROUNDS do begin
				//StrPCopy(pstr,'Round'+IntToStr(i)+'Min');
				MinOpen[i]:=1; //Word(GetIntegerDataRange(REGKEY_STUD,pstr,1,999,1));
				//StrPCopy(pstr,'Round'+IntToStr(i)+'Max');
				MaxOpen[i]:=the_table_limit;
			end;
			with raiseLmts do begin
				lmtPerRnd:=Word(GetIntegerDataRange(REGKEY_STUD,'Raises',0,999,3));
				lmtPerPsn:=Word(GetIntegerDataRange(REGKEY_STUD,'RaisesEach',0,999,3));
			end;
		end;
	end;
	setGlobals;
end;

function MainFrame.WMSTART:LONG;
begin //Writeln('TMainFrm.WMSTART');
	LoadPokerOptions;
	SetMenuBoolean(GetMenu(AppWnd),CM_GAMEDRAW,the_poker_options.game_id=DRAW_POKER);
	SetMenuBoolean(GetMenu(AppWnd),CM_GAMESTUD,the_poker_options.game_id=STUD_POKER);
	winpkrtbl.the_toolbar:=MyToolBar; // hack until toolbar is part of the poker table unit
	TableTop^.Startup;
	winpkrtbl.PreGame;
	winpkrtbl.StartRound;
	UpdateAppTitle;
	WMSTART:=0;
end;

{$ifndef FREE_VER}

const
	ID_DROP=103;

type
	PPlayerDlg=^TPlayerDlg;
	TPlayerDlg=object(TDialog)
		pno:playerIndex;
		constructor Init(p:playerIndex);
		procedure SetupWindow; virtual;
		procedure WMDrawItem(var Msg:TMessage); virtual WM_FIRST+wm_DrawItem;
		procedure IDDrop(var Msg:TMessage); virtual id_First+ID_DROP;
	end;

var
	PlayerData:record
		Wallet:array[0..sl_Word] of Char;
	end;

constructor TPlayerDlg.Init;

	var
		Edit:PEdit;

	begin
		inherited Init(Application.MainWindow,'Player');
		pno:=p;
		Edit:=New(PEdit,InitResource(@Self,105,sl_word+1));
		with PlayerData,the_players[pno] do i2s(PileValue(myChips),Wallet);
		TransferBuffer:=@PlayerData;
	end;

procedure TPlayerDlg.SetupWindow;

	var
		ps:array[0..80] of Char;

	begin
		inherited SetupWindow;
		with the_players[pno] do begin
			StrPCopy(ps,GetNickName);
			SetWindowText(handle,ps);
			StrCopy(ps,'$');
			w2s(HouseLoan,StrEnd(ps));
			SetWindowText(GetItemHandle(202),ps);
			StrCopy(ps,'$');
			i2s(NetAmt(pno),StrEnd(ps));
			SetWindowText(GetItemHandle(203),ps);
			if pno = USER_ID then
				EnableWindow(GetItemHandle(ID_DROP), False)
			else begin
				EnableWindow(GetItemHandle(ID_DROP),
					(nPlay > 2)
					and
					(not Bool(GetMenuState(the_app^.Frame^.GameMenu, CM_GAMEDRAW, mf_ByCommand) and mf_Grayed))
					);
			end;
		end;
	end;

procedure TPlayerDlg.WMDrawItem;

	type
		PDrawItemStruct=^TDrawItemStruct;

	var
		pn:playerIndex;

	begin
		Msg.Result:=1;
	end;

type
	PIconDlg=^TIconDlg;
	TIconDlg=object(TDialog)
		constructor Init;
		procedure SetupWindow; virtual;
		procedure WMDrawItem(var Msg:TMessage); virtual WM_FIRST+wm_DrawItem;
		procedure IDIcon1(var Msg:TMessage); virtual id_First+101;
		procedure IDIcon2(var Msg:TMessage); virtual id_First+102;
		procedure IDIcon3(var Msg:TMessage); virtual id_First+103;
		procedure IDIcon4(var Msg:TMessage); virtual id_First+104;
		procedure IDIcon5(var Msg:TMessage); virtual id_First+105;
		procedure IDIcon6(var Msg:TMessage); virtual id_First+106;
		procedure IDIcon7(var Msg:TMessage); virtual id_First+107;
		procedure IDIcon8(var Msg:TMessage); virtual id_First+108;
	end;

function SelectPlayer(x,y:integer):integer;

	{ Put up the player select panel and return the computer player # or 0. }

	const
		across=4; { # of player icons across the panel }

	var
		i:integer;
		{pPicks:array[1..OPPONENT_POOL_SIZE] of gui_defaultObject;}

	begin
		i:= Application.ExecDialog(New(PIconDlg,Init));
		if i in [101..108] then
			SelectPlayer:=i-100
		else
			SelectPlayer:=0;
	end;

constructor TIconDlg.Init;

	begin
		inherited Init(Application^.MainWindow,'UserIcon');
	end;

procedure TIConDlg.SetupWindow;

	begin
		inherited SetupWindow;
		if CompIcons then
			SetWindowText(handle,'Choose a Player')
		else
			SetWindowText(handle,'Choose Your Icon');
	end;

procedure TIconDlg.IDIcon1; begin EndDlg(101); end;
procedure TIconDlg.IDIcon2; begin EndDlg(102); end;
procedure TIconDlg.IDIcon3; begin EndDlg(103); end;
procedure TIconDlg.IDIcon4; begin EndDlg(104); end;
procedure TIconDlg.IDIcon5; begin EndDlg(105); end;
procedure TIconDlg.IDIcon6; begin EndDlg(106); end;
procedure TIconDlg.IDIcon7; begin EndDlg(107); end;
procedure TIconDlg.IDIcon8; begin EndDlg(108); end;

procedure TIconDlg.WMDrawItem;

	type
		PDrawItemStruct=^TDrawItemStruct;

	var
		i:integer;
		bms,bmm:hBitmap;

	begin
		with PDrawItemStruct(Msg.lParam)^ do
			case Msg.wParam of
				101..108:begin
				end;
			end;
		Msg.Result:=1;
	end;

procedure TPlayerDlg.IDDrop;

	var
		i:integer;

	begin
		if pno=USER_ID then begin
			EndDlg(idCancel);
			CompIcons:=False; { computer icons }
			i:= Application^.ExecDialog(New(PIconDlg,Init));
			if i in [101..108] then begin
				the_players[USER_ID].CNo:=i-100; { so we can restore it if saved }
				DspPlayer(USER_ID);
			end;
		end
		else
			EndDlg(ID_DROP);
	end;

const
	WALLETMAX = 5000; { $ can be given to any player }

function ControlPlayer(p:playerIndex;x,y:integer):boolean;

	{ Put up player "p"'s information panel and return true if the OK
		button was selected otherwise return false for the DROP button.

		If "p" is the human then replace the drop button with the icon
		button. }

	var
		mny:word;

	begin
		case Application^.ExecDialog(New(PPlayerDlg,Init(p))) of
			IDOK:with PlayerData,the_players[p] do begin
				Mny:= minw(WALLETMAX, s2w(Wallet));
				if (mny > 0) and (Mny <> pileValue(myChips)) then begin { did user change this players money ? }
					{ user changed the amount of money this player has so adjust
						the player's original money allocation and chip stacks and
						then redisply his area. }
					orgMny:=mny;
					dspPlayer(p);
				end;
			end;
			ID_DROP:begin
				LeaveGame(p);
				{ if user dropped the current theDealerIndex then pick the next one }
				if p=theDealerIndex then theDealerIndex:=NextClockwiseFrom(theDealerIndex);
			end;
		end;
		if p=USER_ID then dspPlayer(USER_ID);
		controlPlayer:=false;
	end;

{$endif FREE_VER}

constructor MainApp.Construct;
begin
	inherited Construct('Poker');
	the_app:=@Self;
	the_table_stakes:=GetIntegerDataRange(REGKEY_ROOT,KEY_TABLESTAKES,25,5000,the_table_stakes);
	the_table_limit:=GetIntegerDataRange(REGKEY_ROOT,KEY_TABLELIMIT,1,the_table_stakes,the_table_limit);
	the_show_stats_flag:=GetBooleanData(REGKEY_ROOT,KEY_SHOWSTATS,the_show_stats_flag);
	Started:=True;
	{$ifdef AUTOPLAY}
	USER_ID:= 0;
	{$endif}
	splash;
end;

destructor MainFrame.Done;
begin
	inherited Done;
end;

function MainFrame.WMNextDraw(aPlayerId:playerIndex):LONG;
begin
	TableTop^.NextDraw(aPlayerId);
	WMNextDraw:=0
end;

function MainFrame.OnNextBet(aPlayerId:playerIndex):LONG;
begin
	Tabletop^.OnNextBet(aPlayerId);
	OnNextBet:=0;
end;

function MainFrame.OnNextOpen(aPlayerId:playerIndex):LONG;
begin
	Tabletop^.OnNextOpen(aPlayerId);
	OnNextOpen:=0;
end;

function MainFrame.WMShowdown:LONG;
begin
	TableTop^.OnShowdown;
	WMShowdown:=0;
end;

function MainFrame.OnFold:LONG;
begin
	Tabletop^.OnFold;
	OnFold:=0;
end;

function MainFrame.OnCall:LONG;
begin
	Tabletop^.OnCall;
	OnCall:=0;
end;

function MainFrame.OnRaise:LONG;
begin
	Tabletop^.OnRaise;
	OnRaise:=0;
end;

function MainFrame.OnOpen:LONG;
begin
	Tabletop^.OnOpen;
	OnOpen:=0;
end;

procedure MainFrame.UpdateAppTitle;
var
	s:stringBuffer;
begin
	MyFrameWindow^.SetWindowText(StrPCopy(s,Title));
end;

function MainFrame.Title:string;
var
	aTitle:string;
begin
	aTitle:= StrPas(Owner^.FullName) + ' (';
	case the_poker_options.game_id of
		DRAW_POKER:aTitle:= aTitle + '5-Card';
		STUD_POKER:aTitle:= aTitle + StrPas(StudPokerVarDesc[theStudPokerOptions.gType]);
	end;
	aTitle:= aTitle +  ' ' + StrPas(PokerGameTypeText[the_poker_options.game_id]);
	if the_poker_options.game_id = DRAW_POKER then begin
		aTitle:= aTitle + ', ' + StrPas(drawPokerVarDesc[theDrawPokerOptions.gType]);
	end;
	{$ifndef FREE_VER}
	aTitle:= aTitle + ', ';
	if (getWildCardType <> wildNone) then begin
		aTitle:= aTitle + wildTypeDesc[getWildCardType] + ' Wild';
	end
	else
		aTitle:= aTitle + 'No Wild Cards';
	{$endif}
	Title:= aTitle + ', Pot Limit)';
end;

{$ifdef TEST}
type
	FakeApp = object(MainApp)
		constructor Init;
		function FullName:PChar; virtual;
	end;

	TestTPkrFrm=object(MainFrame)
		constructor Construct;
	end;

constructor FakeApp.Init; begin end;

function FakeApp.FullName:PChar; begin FullName:= StrNew('Fake Full Name'); end;

constructor TestTPkrFrm.Construct; 
begin 
end;

procedure Test_GameTitle;
var
	aFakeApp:FakeApp;
	aTestFrm:TestTPkrFrm;
begin
	aFakeApp.Init;
	aTestFrm.Construct;
	aTestFrm.myapp:=@aFakeApp;
	SetWildcardType(WildNone);
	the_poker_options.game_id:= DRAW_POKER;
	theDrawPokerOptions.gType:= ANYTHINGOPENS;
	punit.Assert.EqualStr('Fake Full Name (5-Card Draw, Anything Opens, Pot Limit)', aTestFrm.Title);
	theDrawPokerOptions.gType:= Jackpots;
	punit.Assert.EqualStr('Fake Full Name (5-Card Draw, Jackpots, Pot Limit)', aTestFrm.Title);
	the_poker_options.game_id:= STUD_POKER;
	theStudPokerOptions.gType:= stud6card;
	punit.Assert.EqualStr('Fake Full Name (6-Card Stud, Pot Limit)', aTestFrm.Title);
end;
{$endif TEST}

procedure WriteINIWilds(INIf,INIs:PChar;w:WildCardList);
	var
		WildStr:array[0..53] of Char; { '00001011000' }
		tp:pcPipType;ts:pcSuitType;

	begin
		WildStr[0]:=#0;
		for ts:=ClubSuit to SpadeSuit do for tp:=AcePip to KingPip do
			if w.wList[tp,ts] then
				StrCat(WildStr,'1')
			else
				StrCat(WildStr,'0');
		if w.jWild then
			StrCat(WildStr,'1')
		else
			StrCat(WildStr,'0');
		WriteINIString(INIf,INIs,'Wilds',WildStr);
	end;

const
	ANTE_MAXIMUM=ANTE_EDITBOX_MAX;

function MainFrame.OnOptionsDraw:LONG;

var
	i:integer;
	v:DrawPokerVar;
	aDialog:DrawOptionsDlg;

begin
	aDialog.Init;
	if aDialog.Modal=IDOK then with theApp^,theDrawPokerOptions,theDrawSettingsTransferBuffer do begin
		gType:=myVariation;
		SetIntegerData(REGKEY_DRAW,KEY_VARIATION,Ord(gType));
		if (s2w(myAnteEditboxBuffer) >= 0) then AnteChips[gType]:=minw(ANTE_MAXIMUM,s2w(myAnteEditboxBuffer));
		SetIntegerData(REGKEY_DRAW_VARIATION[gType],KEY_ANTE,AnteChips[gType]);
		{$ifndef VER1}
		m_aScoringMode:=Low(ScoringType);
		while not boolean(Scoring[m_aScoringMode]) do m_aScoringMode:=Succ(m_aScoringMode);
		{$endif}
		if gType=JackPots then m_aScoringMode:=PlayHigh;
		//SetIntegerData(REGKEY_DRAW,'Scoring',Ord(m_aScoringMode));
		for v:=Low(DrawPokerVar) to High(DrawPokerVar) do
			for i:=0 to 1 do begin
				PassOK[gType,i]:=boolean(myPasses[i]);
				SetBooleanData(REGKEY_DRAW_VARIATION[gType],BPINIKeyPass[i],PassOK[gType,i]);
			end;
		//SetIntegerData(REGKEY_DRAW,'MinBefore',BeforeDrawOpen);
		//SetIntegerData(REGKEY_DRAW,'MaxBefore',BeforeDrawOpenMax);
		//SetIntegerData(REGKEY_DRAW,'MinAfter',AfterDrawOpen);
		//SetIntegerData(REGKEY_DRAW,'MaxAfter',AfterDrawOpenMax);
		//with RaiseLmts do begin
			//SetIntegerData(REGKEY_DRAW,'Raises',lmtPerRnd);
			//SetIntegerData(REGKEY_DRAW,'RaisesEach',lmtPerPsn);
		//end;
		//SetIntegerData(REGKEY_DRAW,'Wildcards',Ord(wType));
		//WriteINIWilds(the_app^.GetINIPath,REGKEY_DRAW,WildCards);
		SetGlobals;
		UpdateAppTitle;
	end;
	OnOptionsDraw:=0;
end;

function MainFrame.OnDeal:LONG;

begin
	windows.PostMessage(MyFrameWindow^.handle,WM_DEALBUTTON,0,0);
	OnDeal:=0;
end;

function MainFrame.OnCancelCmd:LONG;

begin
	windows.PostMessage(MyFrameWindow^.handle,WM_CANCELBET,0,0);
	OnCancelCmd:=0;
end;

function MainFrame.OnOpenCmd:LONG;

begin
	windows.PostMessage(MyFrameWindow^.handle,WM_OPEN,0,0);
	OnOpenCmd:=0;
end;

function MainFrame.OnCallCmd:LONG;

begin
	windows.PostMessage(MyFrameWindow^.handle,WM_CALL,0,0);
	OnCallCmd:=0;
end;

function MainFrame.OnFoldCmd:LONG;

begin
	windows.PostMessage(MyFrameWindow^.handle,WM_FOLD,0,0);
	OnFoldCmd:=0;
end;

function MainFrame.OnRaiseCmd:LONG;

begin
	windows.PostMessage(MyFrameWindow^.handle,WM_RAISE,0,0);
	OnRaiseCmd:=0;
end;

function MainFrame.OnPassCmd:LONG;

begin
	windows.PostMessage(MyFrameWindow^.handle,WM_PASS,0,0);
	OnPassCmd:=0;
end;

const
	IDC_STATS_SHOWME=113;

type
	PlayerRankingDialog=object(ODialog)
		constructor Construct(aParent:HWND;show_me_flag:boolean);
		function OnInitDialog:boolean; virtual;
		function OnEndDialog(aCmdId:UINT):boolean; virtual;
	private
		my_show_me_flag:LONG;
		function ShowMeState:boolean;
	end;

constructor PlayerRankingDialog.Construct(aParent:HWND;show_me_flag:boolean);

begin
	inherited Construct(aParent,906);
	my_show_me_flag:=Q(show_me_flag,BST_CHECKED,BST_UNCHECKED);
end;

function Compare(a:LPARAM;b:LPARAM;applicationData:LPARAM):longint;stdcall;

var
	p1,p2:playerIndex;

begin
	p1:=integer(a);
	p2:=integer(b);
	Compare:=NetAmt(p2)-NetAmt(p1);
end;

function PlayerRankingDialog.OnInitDialog:boolean;

const
	nextIndex:int=0;

var
	aListCtrl:OListCtrl;
	pn:playerIndex;
	aIndex:int;
	checkBox:OCheckBox;

	procedure InsertNextColumn(const aTitle:pchar;alignment:int;width_as_percent:integer);

	begin
		aListCtrl.InsertColumn(nextIndex,aTitle,(Integer(aListCtrl.ClientAreaWidth) * width_as_percent) div 100,alignment);
		Inc(nextIndex);
	end;

begin
	OnInitDialog:=inherited OnInitDialog;
	CenterDialog(@Self,GetParent);
	aListCtrl.handle:=GetDlgItem(1003);
	InsertNextColumn('Player',LVCFMT_LEFT,28);
	InsertNextColumn('Chips',LVCFMT_RIGHT,20);
	InsertNextColumn('Loans',LVCFMT_RIGHT,20);
	InsertNextColumn('Net',LVCFMT_RIGHT,32);
	for pn:=1 to MAX_PLAYERS do with the_players[pn] do if Participating then begin
		aIndex:=aListCtrl.AppendItem(getNickName); system.Assert(aIndex<>-1);
		aListCtrl.SetSubItemText(aIndex,1,int2str(pileValue(myChips)));
		aListCtrl.SetSubItemText(aIndex,2,int2str(HouseLoan));
		aListCtrl.SetSubItemText(aIndex,3,NetWinningsAsText);
		aListCtrl.SetItemData(aIndex,pn);
	end;
	aListCtrl.SortItems(@Compare,0);
	checkBox.Handle:=GetDlgItem(IDC_STATS_SHOWME);
	checkBox.SetCheck(my_show_me_flag);
end;

function PlayerRankingDialog.OnEndDialog(aCmdId:UINT):boolean;

var
	checkBox:OCheckBox;

begin
	checkBox.Handle:=GetDlgItem(IDC_STATS_SHOWME);
	my_show_me_flag:=checkBox.GetCheck;
	OnEndDialog:=true;
end;

function PlayerRankingDialog.ShowMeState:boolean;

begin
	ShowMeState:=(my_show_me_flag=BST_CHECKED);
end;

{$ifdef TEST}

procedure Test_StatsDialog;

var
	dialog:PlayerRankingDialog;

begin
	dialog.Construct(0,the_show_stats_flag);
	punit.Assert.AreEqual(BST_CHECKED,dialog.my_show_me_flag);
	dialog.Construct(0,FALSE);
	punit.Assert.AreEqual(BST_UNCHECKED,dialog.my_show_me_flag);
end;

procedure Test_StatsDialog_ShowMeState;

var
	dialog:PlayerRankingDialog;

begin
	dialog.Construct(0,FALSE);
	dialog.my_show_me_flag:=BST_CHECKED;
	punit.Assert.IsTrue(dialog.ShowMeState);
	dialog.my_show_me_flag:=BST_UNCHECKED;
	punit.Assert.IsFalse(dialog.ShowMeState);
end;

{$endif TEST}

function MainFrame.OnShowStats:LONG;

var
	aDialog:PlayerRankingDialog;

begin
	aDialog.Construct(MyFrameWindow^.handle,the_show_stats_flag);
	aDialog.Modal;
	the_show_stats_flag:=aDialog.ShowMeState;
	MApp.SetBooleanData(REGKEY_ROOT,KEY_SHOWSTATS,the_show_stats_flag);
	OnShowStats:=0;
end;

function MainFrame.CMOptionsStud:LONG;

var
	i:integer;
	t:StudPokerVar;
	pstr:array[0..80] of Char;
	aDialog:StudOptionsDlg;

begin
	aDialog.Init;
	if aDialog.Modal=IDOK then with TheApp^,theStudPokerOptions,theStudSettingsTransferBuffer do begin
		gType:=myVariation;
		SetIntegerData(REGKEY_STUD,KEY_VARIATION,Ord(gType));
		if (s2w(myAnteEditboxBuffer) >= 0) then AnteChips[gType]:= minw(ANTE_MAXIMUM, s2w(myAnteEditboxBuffer));
		SetIntegerData(REGKEY_STUD_VARIATION[gType],KEY_ANTE,AnteChips[gType]);
		{$ifndef FREE_VER}
		SetIntegerData(REGKEY_STUD,'Scoring',Ord(m_aScoringMode));
		for i:=1 to StudPLib.MAX_BETTING_ROUNDS do begin
			wvsprintf(pstr,'Round%dMin',i);
			SetIntegerData(REGKEY_STUD,pstr,m_aRoundBettingLimits.MinOpen[i]);
			wvsprintf(pstr,'Round%dMax',i);
			SetIntegerData(REGKEY_STUD,pstr,m_aRoundBettingLimits.MaxOpen[i]);
		end;
		with RaiseLmts do begin
			SetIntegerData(REGKEY_STUD,'Raises',lmtPerRnd);
			SetIntegerData(REGKEY_STUD,'RaisesEach',lmtPerPsn);
		end;
		SetIntegerData(REGKEY_STUD,'Wildcards',Ord(wType));
		SetIntegerData(REGKEY_STUD,WildCards);
		{$endif FREE_VER}
		SetGlobals;
		UpdateAppTitle;
	end;
	CMOptionsStud:=0;
end;

function MainFrame.OnPass:LONG;
begin
	TableTop^.OnPass;
	OnPass:=0;
end;

function MainFrame.OnGameStud:LONG;
begin
	Tabletop^.StartStud;
	SetMenuBoolean(GetMenu(AppWnd),CM_GAMESTUD,True);
	SetMenuBoolean(GetMenu(AppWnd),CM_GAMEDRAW,False);
	updateAppTitle;
	TheApp^.SetIntegerData(REGKEY_ROOT,KEY_GAMEID,Ord(STUD_POKER));
	OnGameStud:=0;
end;

function MainFrame.OnGameDraw:LONG;
begin
	Tabletop^.StartDraw;
	SetMenuBoolean(GetMenu(AppWnd),CM_GAMESTUD,False);
	SetMenuBoolean(GetMenu(AppWnd),CM_GAMEDRAW,True);
	updateAppTitle;
	TheApp^.SetIntegerData(REGKEY_ROOT,KEY_GAMEID,Ord(DRAW_POKER));
	OnGameDraw:=0;
end;

function MainFrame.WMStudDeal:LONG;
begin
	Tabletop^.OnStudDeal;
	WMStudDeal:=0;
end;

{$ifndef VER1}

function MainApp.IdleAction;
begin
	if TotalHndCnt < OddsTableLimit then begin
		if AnyChange then begin
			ClearOddsTable;
			SetupForOdds;
		end;
		IncOddsTable(2);
	end;
	IdleAction:=(TotalHndCnt < OddsTableLimit);
end;

{$endif}

function MainApp.Frame:MainFrameP;
begin
	Frame:=MainFrameP(inherited Frame);
end;

function MainFrame.Tabletop:winpkrtbl.ViewP;
begin
	Tabletop:=winpkrtbl.ViewP(TabletopWindow);
end;

function MainFrame.OnCancel:LONG;
begin
	TableTop^.OnCancel;
	OnCancel:=0;
end;

function MainFrame.OnNextDealer:LONG;
begin
	Tabletop^.NextDealer(playerIndex(theDealerIndex));
	OnNextDealer:=0;
end;

function TheApp:MainAppP;
begin
	TheApp:=@MApp;
end;

{$ifndef FREE_VER}

function doPlayerSelect(var sa;n,s:integer):boolean;

{ Called whenever the user selects a player area to look at. If there
	is no one playing this spot then let the user select one. }

var
	i:integer;
	pn:playerIndex;

begin
	pn:= s - area_firstPlayer + 1; { player # }
	if not the_players[pn].participating then begin
		CompIcons:= pn<>USER_ID;
		i:= SelectPlayer(iconX(pn)+(widestIcon div 2),iconY(pn)+(chipPileHt div 2));
		if i > 0 then begin
			if pn<>USER_ID then begin
				joinGame(pn, i)
			end
			else begin
				the_players[pn].participating:= TRUE;
				DefaultWallet(pn);
				dspPlayer(pn);
			end
		end
	end
	else begin
		ControlPlayer(pn,iconX(pn)+(widestIcon div 2),iconY(pn)+(chipPileHt div 2));
	end;
	doPlayerSelect:=false;
end;

{$endif FREE_VER}

{$ifdef ANAL}

procedure doAnal;
	{ Display analysis results when key is pressed. }
	var
		pn:playerIndex;
	begin
		clrKeyBuffer;
		for pn:=1 to MAX_PLAYERS do with the_players[pn] do begin
			write(pn,': $',pileValue(chps),' Hit Ratio: ');
			if showdownCnt[pn]=0 then
				write('***%')
			else
				write(showdownWin[pn]/showdownCnt[pn]*100:3:0,'%');
			writeln;
		end;
		halt;
	end;
{$endif}

function MainFrame.WMDeal:LONG;
begin
	with the_app^.Frame^ do begin
		EnableMenuItem(MainMenu,CM_OPTIONSDRAW,mf_ByCommand or mf_Grayed);
		EnableMenuItem(MainMenu,CM_OPTIONSSTUD,mf_ByCommand or mf_Grayed);
		EnableMenuItem(MainMenu,CM_GAMEDRAW,mf_ByCommand or mf_Grayed);
		EnableMenuItem(MainMenu,CM_GAMESTUD,mf_ByCommand or mf_Grayed);
	end;
	winpkrtbl.Deal;
	WMDeal:=0;
end;

function MainFrame.WMDealButton:LONG;
begin
	with MApp do begin
		AbortCurrentRound;
		MyFrameWindow^.PostMessage(wm_Deal,0,0);
	end;
	WMDealButton:=0;
end;

procedure MainApp.AbortSession;
begin
	AbortCurrentRound;
	winpkrtbl.PreGame;
	winpkrtbl.StartRound;
end;

procedure MainApp.AbortCurrentRound;
begin
	winpkrtbl.AbortCurrentRound;
end;

function MainFrame.FileNewOk:boolean;
begin
	FileNewOk:=false;
end;

function FrameWindow.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	case aMsg of
		WM_NEXTBET:OnMsg:=TheMainFrame^.OnNextBet(playerIndex(wParam));
		WM_NEXTOPEN:OnMsg:=TheMainFrame^.OnNextOpen(playerIndex(wParam));
		WM_SHOWDOWN:OnMsg:=TheMainFrame^.WMShowdown;
		WM_NEXTDRAW:OnMsg:=TheMainFrame^.WmNextDraw(playerIndex(wParam));
		WM_FOLD:OnMsg:=TheMainFrame^.OnFold;
		WM_CALL:OnMsg:=TheMainFrame^.OnCall;
		WM_RAISE:OnMsg:=TheMainFrame^.OnRaise;
		WM_OPEN:OnMsg:=TheMainFrame^.OnOpen;
		WM_PASS:OnMsg:=TheMainFrame^.OnPass;
		WM_CANCELBET:OnMsg:=TheMainFrame^.OnCancel;
		WM_STUDDEAL:OnMsg:=TheMainFrame^.WmStudDeal;
		WM_NEXTDEALER:OnMsg:=TheMainFrame^.OnNextDealer;
		WM_DEALBUTTON:OnMsg:=TheMainFrame^.wmDealButton;
		WM_DEAL:OnMsg:=TheMainFrame^.wmDeal;
		WM_START:OnMsg:=TheMainFrame^.WMSTART;
		WM_ENABLEDEALBUTTON:begin
			with the_app^.Frame^ do begin
				EnableMenuItem(MainMenu,CM_OPTIONSDRAW,mf_ByCommand or MF_ENABLED);
				EnableMenuItem(MainMenu,CM_OPTIONSSTUD,mf_ByCommand or MF_ENABLED);
				EnableMenuItem(MainMenu,CM_GAMEDRAW,mf_ByCommand or MF_ENABLED);
				EnableMenuItem(MainMenu,CM_GAMESTUD,mf_ByCommand or MF_ENABLED);
			end;
			OnMsg:=0;
		end;
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

function FrameWindow.OnCmd(aCmdId:UINT):LONG;

begin
	case aCmdId of
		CM_OPTIONSDRAW:OnCmd:=TheMainFrame^.OnOptionsDraw;
		CM_STATS:OnCmd:=TheMainFrame^.OnShowStats;
		CM_OPTIONSSTUD:OnCmd:=TheMainFrame^.CMOptionsStud;
		CM_GAMEDRAW:OnCmd:=TheMainFrame^.OnGameDraw;
		CM_GAMESTUD:OnCmd:=TheMainFrame^.OnGameStud;
		CM_DEAL:OnCmd:=TheMainFrame^.OnDeal;
		CM_OPEN:OnCmd:=TheMainFrame^.OnOpenCmd;
		CM_PASS:OnCmd:=TheMainFrame^.OnPassCmd;
		CM_CANCELBET:OnCmd:=TheMainFrame^.OnCancelCmd;
		CM_CALL:OnCmd:=TheMainFrame^.OnCallCmd;
		CM_FOLD:OnCmd:=TheMainFrame^.OnFoldCmd;
		CM_RAISE:OnCmd:=TheMainFrame^.OnRaiseCmd;
		else OnCmd:=inherited OnCmd(aCmdId);
	end
end;

function MainApp.HomePageUrl:pchar;

begin
	HomePageUrl:='http://www.wesleysteiner.com/quickgames/poker.html';
end;

{$ifdef TEST}

procedure BackwardCompatibility;
var
	aApp:FakeApp;
begin
	aApp.Init;
	AssertAreEqual('http://www.wesleysteiner.com/quickgames/poker.html',aApp.HomePageUrl);
	AssertAreEqual('GameId',KEY_GAMEID);
	AssertAreEqual('Variation',KEY_VARIATION);
	AssertAreEqual('ShowStatsDialog',KEY_SHOWSTATS);
	AssertAreEqual('Draw',REGKEY_DRAW);
	AssertAreEqual('Stud',REGKEY_STUD);
	AssertAreEqual('Ante',KEY_ANTE);
	AssertAreEqual('TableStakes',KEY_TABLESTAKES);
	AssertAreEqual('TableLimit',KEY_TABLELIMIT);
	AssertAreEqual('Draw\AnythingOpens',REGKEY_DRAW_VARIATION[anythingOpens]);
	AssertAreEqual('Draw\Jackpots',REGKEY_DRAW_VARIATION[jackpots]);
	AssertAreEqual('Stud\5-Card',REGKEY_STUD_VARIATION[stud5card]);
	AssertAreEqual('Stud\6-Card',REGKEY_STUD_VARIATION[stud6card]);
	AssertAreEqual('Stud\7-Card',REGKEY_STUD_VARIATION[stud7card]);
	AssertAreEqual(0,Ord(DRAW_POKER));
	AssertAreEqual(1,Ord(STUD_POKER));
	AssertAreEqual(1,USER_ID);
end;

{$endif}

begin
	{$ifdef TEST}
	Suite.Add(@BackwardCompatibility);
	Suite.Add(@Test_GameTitle);
	Suite.Add(@Test_StatsDialog);
	Suite.Add(@Test_StatsDialog_ShowMeState);
	Suite.Run('main');
	{$else}
	MApp.Construct;
	MApp.Run;
	MApp.Destruct;
	{$endif}
end.
