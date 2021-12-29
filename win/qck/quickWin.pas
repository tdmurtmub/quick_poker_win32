{ (C) 2005 Wesley Steiner }

{$MODE FPC}

unit quickWin;

{$I platform}
{$I punit.inc}
{$R quickWin}

interface

uses
	std,
	windows,
	quick,
	owindows,
	odlg,
	stdwin,
	winqcktbl, {$ifdef TEST} winqcktbl_tests, {$endif}
	oapp,
	qcktbl,
	toolbars;

const
	MIN_FRAME_WIDTH=800;
	MIN_FRAME_HEIGHT=600;
	{ Screen sizes less than or equal to these values will default to a maximized window. }
	
	MENU_INDEX_PRODUCT	=0;
	MENU_INDEX_TABLE	=MENU_INDEX_PRODUCT+1;
	MENU_INDEX_GAME		=MENU_INDEX_TABLE+1;

type
	PCmnApp=^TCmnApp;
	PCmnFrm=^TCmnFrm;

	stdwinFrameWindow=object(oapp.FrameWindow)
		function AskFileNew:boolean;
		function FileNewOk:boolean; virtual;
		function OnCmd(aCmdId:UINT):LONG; virtual;
		function OnCreate:LONG; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		procedure OnFileNewTemplate; test_virtual
	end;

	BaseAppP=^BaseApp;

	BaseFrameP=^BaseFrame;
	BaseFrame=object(OMainFrame)
		fdW,fdH:Word; { factory default window width and height }
		constructor Init(aMainApp:BaseAppP;startMaximized:boolean);
		destructor Done; virtual;
		function Owner:BaseAppP;
	test_private
		myApp:BaseAppP;
	end;

	BaseApp=object(OApplication)
		constructor Init(aFriendlyTitle,aStorageName:pchar);
		function FullName:PChar; virtual; abstract;
		function MainFrm:BaseFrameP;
		procedure AbortSession; virtual; { abort the current session }
		procedure OnNew; virtual; { init for a new session }
	private
		procedure Initialize;
	end;

	commonFrameWindow=object(stdwinFrameWindow)
		constructor Construct;
		function OnCmd(aCmdId:UINT):LONG; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		function OnSize(resizeType:uint;newWidth,newHeight:integer):LONG; virtual;
		function MinFrmHt:word; virtual;
		function MinFrmWd:word; virtual;
		procedure InitFrmPos;
		procedure InitFrmSize;
	test_private
		function DoHelpAbout:LONG;
		function DoGetMinMaxInfo(aMinMaxInfo:PMINMAXINFO):integer;
	end;
	
	TCmnFrm=object(BaseFrame)
		GameIcon:hIcon;
		{$ifndef NOPLAYBAR}
		myToolbar:PlaybarP;
		{$endif}
		constructor Init(pCmnApp:PCmnApp;startMaximized:boolean);
		destructor Done; virtual;
		function CanClose:boolean; virtual;
		function GetTabletopHWnd:HWND;
		function Owner:PCmnApp;
		function TabletopWindow:PTabletop;
		procedure SetTabletopWindow(aTabletop:PTabletop);
	private
		myTabletop:PTabletop;
//		procedure InitWndClass(var AWndClass:TWndClass);
	end;

	TCmnApp=object(BaseApp)
		{$ifdef MASTER_PALETTE}
		m_master_palette:hPalette; { handle to the master palette }
		{$endif MASTER_PALETTE} 
		constructor Construct(const aBrandName:string;const a_name:pchar {app name without the Brand prefix});
		destructor Destruct; virtual;
		function Frame:PCmnFrm;
		function FullName:pchar; virtual;
		function HomePageUrl:pchar; virtual; abstract;
		function DonatePageUrl:pchar; virtual; abstract;
		procedure ShowHomePage(aHandle:HWND);
		procedure ShowUrlPage(aHandle:HWND;url:pchar);
		procedure Splash; virtual;
		procedure Initialize;
	test_private
		brand_name:string;
		game_name:pchar;
		game_title:pchar;
		procedure InitializeStorageName(aStorageName:pchar; const aBrandName,aAppName:string);
	end;

	ApplicationP=^Application;
	FrameP=^Frame;

	AboutDlg=object(ODialog)
		constructor Construct(pParent:PWindow;pCmnApp:PCmnApp);
		function OnInitDialog:boolean; virtual;
		function OnCmd(aCmdId:UINT):LONG; virtual;
	test_private
		myApp:PCmnApp;
		function OnHomePage:LONG;
		procedure ShowHomePage; test_virtual
	end;

	GamePlayerType=object // OBSOLETE: use quickWin.Player
		PlyrName:string[8]; { name to identify this person }
		my_gender:gender;
		IQVal:byte; { person's IQ }
		constructor Init(NickName:String; G:gender; IQN:byte);
		procedure SetNickName(s:string);
		function GetNickName:string;
	end;

	PGamePlayer=^TGamePlayer; // OBSOLETE: use quickWin.Player
	TGamePlayer=object(GamePlayerType)
		constructor Init(NickName:String; G:gender; IQN:byte);
	end;

	TCardPlayer=object(TGamePlayer) // OBSOLETE: use quickWin.Player
	end;

	Player=object(TCardPlayer)
	end;
	
	BooleanSetting=object
		constructor Construct(app:quick.ApplicationP;menu:HMENU;cmd_id:UINT;key,subkey:pchar;default:boolean);
		procedure OnChanged(new_state:boolean); virtual;
		procedure Toggle;
	private
		app:quick.ApplicationP;
		cmd_id:UINT;
		state:boolean;
		menu:HMENU;
		key,subkey:pchar;
	end;

	BooleanAppSetting=object(BooleanSetting)
	end;
	
	Application=object(TCmnApp)
		BgImagePath:ansistring;
		constructor Construct(const name:string);
		function Frame:FrameP;
	test_private
		procedure ValidateBgImagePath(aBitmap:HBITMAP);
	end;

	FrameWindow=object(commonFrameWindow)
		function ClassBackgroundBrush:HBRUSH; virtual;
		function ClassName:LPCTSTR; virtual;
		function OnCmd(aCmdId:UINT):LONG; virtual;
		function OnTabletopAnimation:LONG; virtual;
		function OnTabletopBackground:LONG; virtual;
		function ShouldAutoSize(UseBgImage:boolean;BgImage:HBITMAP):boolean;
		procedure Autosize(aBitmap:HBITMAP);
	private
		function DoDonatePage:LONG;
		function DoHomePage:LONG;
	end;

	Frame=object(TCmnFrm)
		constructor Construct(app:ApplicationP;start_maximized:boolean);
		function GameMenu:HMENU;
		function Owner:ApplicationP;
		function Tabletop:winqcktbl.PTabletop;
		procedure UpdateTitle(const addendum:string);
	end;

	AnimationSetting=object(BooleanSetting)
		procedure OnChanged(new_state:boolean); virtual;
	end;

const
	the_app:ApplicationP=NIL;
	
function ActionDelay:integer;
function AppWnd:HWnd;
function GetCheckedMenuItem(Menu:hMenu;Item:integer):boolean;
function ReadDelay:integer;
function TheApp:PCmnApp;

procedure X_Raspberry;
procedure CenterDialog(Dlg:ODialogPtr;Wnd:HWnd);
procedure PositionDialog(Dlg:ODialogPtr;Wnd:HWnd;X,Y:integer);

function GetTabletopView:PTabletop; // OBSOLETE!
function TabletopHWindow:HWND; // OBSOLETE

{$ifdef TEST}
function CreateFrameTitle(const title,addendum:string):string;
{$endif}

implementation

uses
	mmsystem,
	strings,
	punit,
	stringsx,
	windowsx,
	sdkex,
	gdiex,
	cmndlgs,
	screen;

const
	CM_HELPHOMEPAGE=908;
	CM_DONATE=907;

var
	animation_toggle:AnimationSetting;
	
const
	SL_APP_TITLE=40; { max length of application title }

{$ifdef DEBUG}
{define TEST_SCREEN_SIZE} { Tests an explicit screen size. }
{$ifdef TEST_SCREEN_SIZE}
const
	TEST_SCREEN_WIDTH=MIN_FRAME_WIDTH;
	TEST_SCREEN_HEIGHT=MIN_FRAME_HEIGHT;
{$endif}
{$endif DEBUG}

var
	CMFileNewOK:boolean; { true if user starts a New... app }

function AppWnd:HWnd;
{ Return the main application's window handle. }
begin
	AppWnd:=theApplication^.MainWindow^.MyFrameWindow^.handle;
end;

function TheApp:PCmnApp;
begin
	TheApp:=PCmnApp(theApplication);
end;

function TCmnApp.FullName:pchar;
begin
	FullName:=game_title;
end;

function GetTabletopView:PTabletop;
begin
	GetTabletopView:=PCmnFrm(TheApp^.MainFrm)^.TabletopWindow;
end;

procedure TCmnApp.Splash;
begin //writeln('TCmnApp.Splash');
	{$ifndef DEBUG} SendMessage(AppWnd,WM_COMMAND,CM_HELPABOUT,0); {$endif}
	UpdateWindow(MainWindow^.MyFrameWindow^.handle);
	PostMessage(AppWnd,WM_START,0,0);
end;

function TabletopHWindow:HWND;
begin
	if (GetTabletopView <> nil) then
		TabletopHWindow:= GetTabletopView^.handle
	else
		TabletopHWindow:= 0;
end;

function ActionDelay:integer;
begin
	ActionDelay:= BASEDELAY * 40;
end;

function ReadDelay:integer;

begin
	ReadDelay:= BASEDELAY * 110;
end;

constructor TCmnFrm.Init(pCmnApp:PCmnApp;startMaximized:boolean);

begin
	GameIcon:= 0;
	inherited Init(pCmnApp,startMaximized);
	myTabletop:= nil;

	if Screen.Properties.Width<=MIN_FRAME_WIDTH then CmdShow:=SW_SHOWMAXIMIZED;

	{$ifdef TEST_SCREEN_SIZE}
	with Attr do begin
		W:=TEST_SCREEN_WIDTH;
		H:=TEST_SCREEN_HEIGHT;
		X:=Center(W,0,Screen.Properties.Width);
		Y:=Center(H,0,Screen.Properties.Height);
		CmdShow:=SW_SHOWNORMAL;
	end;
	{$endif}

	{$ifndef NOPLAYBAR}
	myToolbar:=New(PlaybarP,Init);
	{$endif}
end;

destructor TCmnFrm.Done;

begin
	DestroyIcon(GameIcon);
	inherited Done;
end;

constructor TCmnApp.Construct(const aBrandName:string;const a_name:pchar);
var
	r:TRect;
	aStorageName:stringbuffer;
begin
	brand_name:=aBrandName;
	game_name:=StrNew(a_name);
	Initialize;
	InitializeStorageName(aStorageName,brand_name,StrPas(game_name));
	Randomize;
	inherited Init(game_title,aStorageName);
	MakeNewTTColor(PaletteRGB(
		GetIntegerDataRange(KEY_TABLETOP,KEY_TABLETOP_COLOR_R,0,255,63),
		GetIntegerDataRange(KEY_TABLETOP,KEY_TABLETOP_COLOR_G,0,255,127),
		GetIntegerDataRange(KEY_TABLETOP,KEY_TABLETOP_COLOR_B,0,255,63)));
	X_SoundStatus:=GetBooleanData(REGKEY_ROOT,KEY_SOUND_EFFECTS,{$ifdef DEBUG} FALSE {$else} TRUE {$endif});
	GetClientRect(Frame^.MyFrameWindow^.handle,r);
	with Frame^.MyFrameWindow^ do Frame^.myTabletop^.Create(handle, ClientAreaWd, ClientAreaHt{$ifndef NOPLAYBAR}-BUTTON_BAR_HT{$endif});
	{$ifndef NOPLAYBAR}
	Frame^.myToolbar^.Create(Frame^.MyFrameWindow^.Handle);
	{$endif}
	SetMenuBoolean(GetMenu(AppWnd),CM_OPTIONSSOUND,X_SoundStatus);

	{$ifdef MASTER_PALETTE}
	{ create the master palette }
	GetMem(x_palette, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * NUM_PALETTE_ENTRIES);
	x_palette^.palVersion:= $0300;
	x_palette^.palNumENtries:= NUM_PALETTE_ENTRIES;
	{$R-}
	for i:= 0 to (NUM_PALETTE_ENTRIES - 1) do begin
		x_palette^.palPalEntry[i].peRed:= i;
		x_palette^.palPalEntry[i].peGreen:= i;
		x_palette^.palPalEntry[i].peBlue:= i;
		x_palette^.palPalEntry[i].peFlags:= 0;
	end;
	{$ifdef DEBUG}
	{$R+}
	{$endif DEBUG}
	m_master_palette:= CreatePalette(x_palette^);
	FreeMem(x_palette, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * NUM_PALETTE_ENTRIES);
	{$endif MASTER_PALETTE}
end;

destructor TCmnApp.Destruct;
begin //writeln('TCmnApp.Destruct');
	{$ifdef MASTER_PALETTE}
	DeleteObject(m_master_palette);
	{$endif MASTER_PALETTE}
	inherited Destruct;
	winqcktbl.Terminate;
	StrDispose(game_name);
	FreeMem(game_title, SL_APP_TITLE + 1);
	if TabletopBrush <> 0 then DeleteObject(TabletopBrush);
end;

function GetCheckedMenuItem(Menu:hMenu;Item:integer):boolean;

begin
	GetCheckedMenuItem:= (GetMenuState(Menu, Item, MF_BYCOMMAND) = MF_CHECKED);
end;

function commonFrameWindow.OnCmd(aCmdId:UINT):LONG;

begin
	case aCmdId of
		CM_OPTIONSSOUND:begin
			Toggle(X_SoundStatus);
			SetMenuBoolean(GetMenu(AppWnd),CM_OPTIONSSOUND,X_SoundStatus);
			TheApp^.SetBooleanData(REGKEY_ROOT,KEY_SOUND_EFFECTS,X_SoundStatus);
			OnCmd:=0;
		end;
		CM_HELPABOUT:begin
			OnCmd:=DoHelpAbout;
			exit;
		end
		else OnCmd:=inherited OnCmd(aCmdId);
	end
end;

constructor AboutDlg.Construct(pParent:PWindow;pCmnApp:PCmnApp);

begin
	inherited Construct(pParent^.handle,500);
	myApp:=pCmnApp;
end;

procedure TCmnApp.ShowUrlPage(aHandle:HWND;url:pchar);

begin
	ShellExecute(aHandle,'open',url,'','',SW_RESTORE);
end;

procedure TCmnApp.ShowHomePage(aHandle:HWND);

begin
	ShowUrlPage(aHandle,HomePageUrl);
end;

procedure AboutDlg.ShowHomePage;

begin
	myApp^.ShowHomePage(handle);
end;

function AboutDlg.OnHomePage:LONG;

begin
	ShowHomePage;
	OnHomePage:=0;
end;

function AboutDlg.OnCmd(aCmdId:UINT):LONG;

begin
	case aCmdId of
		101:OnCmd:=OnHomePage;
		else OnCmd:=inherited OnCmd(aCmdId);
	end
end;

function AboutDlg.OnInitDialog:boolean;

begin //WriteLn('AboutDlg.OnInitDialog');
	OnInitDialog:=inherited OnInitDialog;
	CenterWindow(Handle,GetParent);
	SetDlgItemText(Handle,204,PChar(AnsiString(myApp^.VersionInfo^.ProductName)));
	SetDlgItemText(Handle,202,PChar(AnsiString('Version '+myApp^.VersionInfo^.ProductVersion)));
	SetDlgItemText(Handle,203,PChar(AnsiString(myApp^.VersionInfo^.LegalCopyright)));
end;

function commonFrameWindow.DoHelpAbout:LONG;

var
	aDialog:AboutDlg;

begin
	aDialog.Construct(@self,theApp);
	aDialog.Modal;
	DoHelpAbout:=1;
end;

function TCmnFrm.CanClose:boolean;

begin
	CanClose:=true;
end;

(*
procedure TCmnFrm.InitWndClass(var AWndClass:TWndClass);

begin
	with AWndClass do begin
		Style:=Style or CS_DBLCLKS;
		hIcon:=LoadIcon(HInstance, MakeIntResource(101));
		GameIcon:=hIcon;
	end;
end;
*)

function TCmnFrm.GetTabletopHWnd:HWND;

begin
	GetTabletopHWnd:=TabletopWindow^.handle;
end;

procedure commonFrameWindow.InitFrmSize;

begin
	if (myRestoredW=LongWord(CW_USEDEFAULT)) or (myRestoredH=LongWord(CW_USEDEFAULT)) then begin
		myRestoredW:=MIN_FRAME_WIDTH;
		myRestoredH:=MIN_FRAME_HEIGHT;
	end;
end;

procedure commonFrameWindow.InitFrmPos;

begin
    if (myRestoredX=long(CW_USEDEFAULT)) or (myRestoredY=long(CW_USEDEFAULT)) then begin
		myRestoredX:=Center(myRestoredW,0,Screen.Properties.Width);
		myRestoredY:=Center(myRestoredH,0,Screen.Properties.Height);
	end;
end;

function commonFrameWindow.OnSize(resizeType:uint;newWidth,newHeight:integer):LONG;
begin
	if resizeType in [SIZEFULLSCREEN,SIZENORMAL] then begin
		with PCmnFrm(TheApp^.MainFrm)^ do begin
			{$ifndef NOPLAYBAR}
			if myToolbar<>nil then MoveWindow(myToolbar^.handle,0,newHeight-BUTTON_BAR_HT,newWidth,BUTTON_BAR_HT,TRUE);
			if (TabletopWindow <> nil) then MoveWindow(TabletopWindow^.handle,0,0,newWidth,newHeight-BUTTON_BAR_HT,TRUE);
			{$else}
			if (TabletopWindow <> nil) then MoveWindow(TabletopWindow^.handle,0,0,newWidth,newHeight,TRUE);
			{$endif}
		end;
	end;
	OnSize:=inherited OnSize(resizeType,newWidth,newHeight);
end;

procedure X_Raspberry;
begin
	if X_SoundStatus then begin
		MessageBeep($FFFF);
	end;
end;

function TCmnFrm.TabletopWindow:PTabletop;
begin
	TabletopWindow:=myTabletop;
end;

procedure TCmnFrm.SetTabletopWindow(aTabletop:PTabletop);

begin
	myTabletop:=aTabletop;
end;

function commonFrameWindow.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	case aMsg of
		WM_GETMINMAXINFO:OnMsg:=DoGetMinMaxInfo(PMINMAXINFO(lParam));
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

function commonFrameWindow.DoGetMinMaxInfo(aMinMaxInfo:PMINMAXINFO):integer;

begin
	aMinMaxInfo^.ptMinTrackSize.x:=MinFrmWd;
	aMinMaxInfo^.ptMinTrackSize.y:=MinFrmHt;
	DoGetMinMaxInfo:=0;
end;

function commonFrameWindow.MinFrmHt:word;

begin
	MinFrmHt:=MIN_FRAME_HEIGHT;
end;

function commonFrameWindow.MinFrmWd:word;

begin
	MinFrmWd:=MIN_FRAME_WIDTH;
end;

procedure TCmnApp.Initialize;

var
	p:pchar;

begin
	GetMem(game_title,SL_APP_TITLE+1);
	GetMem(p,Length(brand_name)+1);
	StrPCopy(game_title,brand_name);
	StrCat(game_title,' ');
	StrCat(game_title,game_name);
	FreeMem(p,Length(brand_name)+1);
end;

procedure TCmnApp.InitializeStorageName(aStorageName:pchar; const aBrandName,aAppName:string);

begin
	StrPCopy(aStorageName,aBrandname+aAppName);
end;

function TCmnFrm.Owner:PCmnApp;
begin
	Owner:=PCmnApp(inherited Owner);
end;

function TCmnApp.Frame:PCmnFrm;
begin
	Frame:=PCmnFrm(TheApp^.MainFrm);
end;

constructor commonFrameWindow.Construct;
begin //writeln('commonFrameWindow.Construct');
	inherited Construct;
	InitFrmSize;
	InitFrmPos;
end;

function stdwinFrameWindow.FileNewOk:boolean;

begin
	FileNewOk:=true;
end;

function stdwinFrameWindow.AskFileNew:boolean;

begin
	CMFileNewOK:=MessageBox(handle,
		'Are you sure?'+CR+CR+'Starting a new game will reset all scores and standings to their initial values.',
		'Start a New Game',
		MB_ICONQUESTION or MB_YESNO)=IDYES;
	AskFileNew:=CMFileNewOK;
end;

procedure stdwinFrameWindow.OnFileNewTemplate;
begin
	CMFileNewOK:=FileNewOk;
	if CMFileNewOK or AskFileNew then begin
		TheApp^.AbortSession;
		TheApp^.OnNew;
	end;
end;

function stdwinFrameWindow.OnCreate:LONG;
begin //writeln('stdwinFrameWindow.OnCreate');
	OnCreate:=inherited OnCreate;
end;

function stdwinFrameWindow.OnCmd(aCmdId:UINT):LONG;
begin
	case aCmdId of
		CM_FILENEW:begin
			OnFileNewTemplate;
			OnCmd:=0;
		end;
		CM_EXIT:begin
			PostQuitMessage(0);
			OnCmd:=0;
		end;
		else OnCmd:=inherited OnCmd(aCmdId);
	end
end;

function stdwinFrameWindow.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	case aMsg of
		WM_DESTROY:begin
			PostQuitMessage(0);
			OnMsg:=0;
		end
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

function BaseApp.MainFrm:BaseFrameP;
begin
	MainFrm:= BaseFrameP(theApplication^.MainWindow);
end;

constructor BaseFrame.Init(aMainApp:BaseAppP;startMaximized:boolean);
begin
	inherited Construct(aMainApp,'Frame',startMaximized);
	myApp:=aMainApp;
	SetCursor(LoadCursor(0,idc_Arrow));
end;

procedure BaseApp.OnNew;
begin
end;

procedure BaseApp.Initialize;
begin
end;

constructor BaseApp.Init(aFriendlyTitle,aStorageName:pchar);
begin
	inherited Construct(aFriendlyTitle,aStorageName);
	Initialize;
end;

function BaseFrame.Owner:BaseAppP;
begin
	Owner:=myApp;
end;

destructor BaseFrame.Done;
begin
	inherited Destruct;
end;

procedure BaseApp.AbortSession;

begin
end;

procedure CenterDialog(Dlg:ODialogPtr;Wnd:HWnd);
begin
	CenterWindow(dlg^.handle,Wnd);
end;

procedure PositionDialog(Dlg:ODialogPtr;Wnd:HWnd;X,Y:integer);
{ Position a dialog "Dlg" in window "Wnd" at X,Y client coordinates }
var
	aPoint:TPoint;
begin
	aPoint.X:=X;
	aPoint.Y:=Y;
	ClientToScreen(Wnd,aPoint);
	SetWindowPos(Dlg^.handle,Dlg^.handle,aPoint.X,aPoint.Y,0,0,swp_NoSize or swp_NoZOrder);
end;

constructor Application.Construct(const name:string);
begin //writeln('Application.Construct(',name,')');
	inherited Construct('Quick',PChar(AnsiString(name)));
	the_app:=@self;
	animation_toggle.Construct(@self,GetMenu(MainFrm^.MyFrameWindow^.handle),CM_TABLETOP_ANIMATION,KEY_TABLETOP,KEY_TABLETOP_ANIMATION,TRUE);
	BgImagePath:=GetStringData(KEY_TABLETOP,KEY_TABLETOP_IMAGEPATH,'');
	ValidateBgImagePath(Frame^.Tabletop^.BgImage);
end;

function Application.Frame:FrameP;
begin
	Frame:=FrameP(inherited Frame);
end;

constructor Frame.Construct(app:ApplicationP;start_maximized:boolean);
begin //writeln('Frame.Construct(app:ApplicationP;start_maximized:boolean)');
	inherited Init(app,start_maximized);
end;

function CreateFrameTitle(const title,addendum:string):string;
begin
	if StringEmpty(addendum) 
		then CreateFrameTitle:=title
		else CreateFrameTitle:=title+' ('+addendum+')';	
end;

procedure Frame.UpdateTitle(const addendum:string);
begin
	MyFrameWindow^.SetWindowText(PChar(AnsiString(CreateFrameTitle(StrPas(Owner^.Fullname),addendum))));
end;

function Frame.GameMenu:HMENU;
begin
	GameMenu:=GetSubMenu(MainMenu,MENU_INDEX_GAME);
end;

function Frame.Tabletop:PTabletop;
begin
	Tabletop:=PTabletop(TabletopWindow);
end;

function FrameWindow.ClassBackgroundBrush:HBRUSH;
begin
	ClassBackgroundBrush:=0;
end;

function FrameWindow.ClassName:LPCTSTR;
begin
	ClassName:=OWINDOW_CLASS+'.Quick.Frame';
end;

function FrameWindow.DoHomePage:LONG;
begin
	the_app^.ShowHomePage(handle);
	DoHomePage:=0;	
end;

function FrameWindow.DoDonatePage:LONG;
begin
	with the_app^ do ShowUrlPage(handle,DonatePageUrl);
	DoDonatePage:=0;	
end;

function FrameWindow.OnCmd(aCmdId:UINT):LONG;
begin
	case aCmdId of
		CM_HELPHOMEPAGE:OnCmd:=DoHomePage;
		CM_DONATE:OnCmd:=DoDonatePage;
		CM_TABLETOP:OnCmd:=OnTabletopBackground;
		CM_TABLETOP_ANIMATION:OnCmd:=OnTabletopAnimation;
		else OnCmd:=inherited OnCmd(aCmdId);
	end
end;

function FrameWindow.OnTabletopBackground:LONG;
var
	aDialog:TabletopDlg;
begin
	aDialog.Construct(the_app^.Frame,the_app^.Frame^.Tabletop,the_app^.BgImagePath);
	if aDialog.Modal=IDOK then with the_app^ do begin
		SetIntegerData(KEY_TABLETOP,KEY_TABLETOP_COLOR_R,GetRValue(x_table_top_color));
		SetIntegerData(KEY_TABLETOP,KEY_TABLETOP_COLOR_G,GetGValue(x_table_top_color));
		SetIntegerData(KEY_TABLETOP,KEY_TABLETOP_COLOR_B,GetBValue(x_table_top_color));
		SetBooleanData(KEY_TABLETOP,KEY_TABLETOP_USEIMAGE,the_app^.Frame^.Tabletop^.UseBgImage);
		SetStringData(KEY_TABLETOP,KEY_TABLETOP_IMAGEPATH,BgImagePath);
	end;
	aDialog.Destruct;
	OnTabletopBackground:=0;
end;

function FrameWindow.ShouldAutoSize(UseBgImage:boolean;BgImage:HBITMAP):boolean;
begin
	ShouldAutoSize:=((not Maximized) and UseBgImage and IsValidHandle(BgImage));
end;

procedure Application.ValidateBgImagePath(aBitmap:HBITMAP);
begin
	if aBitmap=NULL_HANDLE then BgImagePath:=EMPTY_STRING;
end;

procedure FrameWindow.Autosize(aBitmap:HBITMAP);
var 
	r:RECT;
begin
	System.Assert(aBitmap<>NULL_HANDLE);
	SetRect(r, 0, 0, GetBitmapWd(aBitmap), GetBitmapHt(aBitmap){$ifndef NOPLAYBAR}+BUTTON_BAR_HT{$endif});
	AdjustWindowRect(r,self.WindowStyle,TRUE);
	ApiCheck(LONG(SetWindowPos(self.handle,0,r.left,r.top,GetRectWd(r),GetRectHt(r),SWP_NOMOVE or SWP_NOZORDER)));
end;

constructor GamePlayerType.init(NickName:String;G:gender;IQN:byte);
begin
	plyrName:= NickName;
	iqVal:= iqN;
	my_gender:= G;
end;

function gamePlayerType.getNickName:string;
begin
	getNickName:= plyrName;
end;

procedure gamePlayerType.setNickName(s:string);
begin
	plyrName:= s;
end;

constructor TGamePlayer.Init(NickName:String; G:gender;IQN:byte);
begin
	inherited Init(NickName,G,IQN);
end;

function Frame.Owner:ApplicationP;
begin
	Owner:=ApplicationP(inherited Owner);
end;

constructor BooleanSetting.Construct(app:quick.ApplicationP;menu:HMENU;cmd_id:UINT;key,subkey:pchar;default:boolean);
begin
	self.app:=app;
	self.cmd_id:=cmd_id;
	self.menu:=menu;
	self.key:=key;
	self.subkey:=subkey;
	self.state:=app^.GetBooleanData(key,subkey,default);
	SetMenuBoolean(self.menu,self.cmd_id,self.state);
	OnChanged(self.state);
end;

procedure BooleanSetting.OnChanged(new_state:boolean); 
begin 
end;

procedure BooleanSetting.Toggle;
begin
	std.Toggle(self.state);
	SetMenuBoolean(self.menu,self.cmd_id,self.state);
	self.app^.SetBooleanData(self.key,self.subkey,self.state);
	OnChanged(self.state);
end;

procedure AnimationSetting.OnChanged(new_state:boolean);
begin
	AnimateSteps:=Q(new_state,FDSTEP,1);
end;
	
function FrameWindow.OnTabletopAnimation:LONG;
begin
	animation_toggle.Toggle;
	OnTabletopAnimation:=0;
end;

end.
