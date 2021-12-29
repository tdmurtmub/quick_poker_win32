{ (C) 2005 Wesley Steiner }

{$MODE FPC}

unit quickWinTests;

{$I platform}
{$I punit.inc}

interface

uses
	windows,
	punit,
	quick,
	quickWin;

type
	BaseAppStub=object(BaseApp)
		GetBooleanData_result:boolean;
		constructor Construct;
		function GetBooleanData(aKey,aSubKey:pchar;default:boolean):boolean; virtual;
		procedure InitMainWindow; virtual;
	end;
		
	ApplicationStub=object(Application)
		constructor Construct;
		function FullName:pchar; virtual;
		function HomePageUrl:pchar; virtual;
		function DonatePageUrl:pchar; virtual;
		procedure InitMainWindow; virtual;
	end;
	
	FrameWindowStub=object(FrameWindow)
		constructor Construct;
	end;
	
	FrameStub=object(Frame)
		my_app:ApplicationP;
		constructor Construct;
		constructor Construct(app:ApplicationP);
	end;

implementation

uses
	stringsx,
	windowsx,
	screen,
	stdwin,
	winqcktbl;
	
type
	FakeTCmnApp=object(TCmnApp)
		constructor Init;
		function HomePageUrl:pchar; virtual;
		function DonatePageUrl:pchar; virtual;
		function FullName:PChar; virtual;
		procedure InitMainWindow; virtual;
	end;

constructor FakeTCmnApp.Init; begin end;
function FakeTCmnApp.HomePageUrl:pchar; begin HomePageUrl:=''; end;
function FakeTCmnApp.DonatePageUrl:pchar; begin DonatePageUrl:=''; end;
function FakeTCmnApp.FullName:PChar; begin FullName:=''; end;
procedure FakeTCmnApp.InitMainWindow; begin end;

type
	TestAboutDlg = object(AboutDlg) 
		constructor Construct;
		procedure ShowHomePage; virtual;
	end;

constructor TestAboutDlg.Construct; begin end;
procedure TestAboutDlg.ShowHomePage; begin end;

procedure Test_OnHomePage_return_code;

var
	aDialog:TestAboutDlg;

begin
	aDialog.Construct;
	punit.Assert.Equal(0,aDialog.OnHomePage);
end;

type
	TestTCmnFrm = object(commonFrameWindow)
		constructor Init;
	end;

constructor TestTCmnFrm.Init; begin end;

procedure TestTCmnFrmInitFrmSize; 

var
	aFrm:TestTCmnFrm;

begin
	aFrm.Init;

	{ adjust w and h if either w or h are the default }
	aFrm.myRestoredW:=LongWord(CW_USEDEFAULT);
	aFrm.myRestoredH:=123;
	aFrm.InitFrmSize;
	punit.Assert.EqualLong(MIN_FRAME_WIDTH,Long(aFrm.myRestoredW));
	punit.Assert.EqualLong(MIN_FRAME_HEIGHT,Long(aFrm.myRestoredH));
	aFrm.myRestoredW:=245;
	aFrm.myRestoredH:=LongWord(CW_USEDEFAULT);
	aFrm.InitFrmSize;
	punit.Assert.AreEqualLong(MIN_FRAME_WIDTH,Long(aFrm.myRestoredW));
	punit.Assert.AreEqualLong(MIN_FRAME_HEIGHT,Long(aFrm.myRestoredH));

	{ do not adjust w or h if both w and h are not the default }
	aFrm.myRestoredW:=792;
	aFrm.myRestoredH:=1034;
	aFrm.InitFrmSize;
	punit.Assert.Equal(792,aFrm.myRestoredW);
	punit.Assert.Equal(1034,aFrm.myRestoredH);
end;

procedure TestTCmnFrmInitFrmPos; 

var
	aFrm:TestTCmnFrm;

begin
	aFrm.Init;

	aFrm.myRestoredW:= 320;
	aFrm.myRestoredH:= 240;

	{ do not adjust x or y if both values are not the default }
	aFrm.myRestoredX:= 1829;
	aFrm.myRestoredY:= 156;
	aFrm.InitFrmPos;
	punit.Assert.Equal(1829, aFrm.myRestoredX);
	punit.Assert.Equal(156, aFrm.myRestoredY);

	{ if x or y are the default then center the frame in the screen }
	aFrm.myRestoredX:=Longint(CW_USEDEFAULT);
	aFrm.myRestoredY:= 123;
	aFrm.InitFrmPos;
	punit.Assert.Equal((Screen.Properties.Width - 320) div 2, aFrm.myRestoredX);
	punit.Assert.Equal((Screen.Properties.Height - 240) div 2, aFrm.myRestoredY);
	aFrm.myRestoredX:= 9325;
	aFrm.myRestoredY:=Longint(CW_USEDEFAULT);
	aFrm.InitFrmPos;
	punit.Assert.Equal((Screen.Properties.Width - 320) div 2, aFrm.myRestoredX);
	punit.Assert.Equal((Screen.Properties.Height - 240) div 2, aFrm.myRestoredY);
end;

type
	TFakeCmnFrm=object(commonFrameWindow)
		constructor Init;
	end;

constructor TFakeCmnFrm.Init; begin end;

procedure TestTCmnFrmMinFrmHt; 

var
	aFakeCmnFrm:TFakeCmnFrm;
	
begin
	aFakeCmnFrm.Init;
	punit.Assert.Equal(MIN_FRAME_HEIGHT, aFakeCmnFrm.MinFrmHt);
end;

procedure TestTCmnFrmMinFrmWd; 

var
	aFakeCmnFrm:TFakeCmnFrm;

begin
	aFakeCmnFrm.Init;
	punit.Assert.Equal(MIN_FRAME_WIDTH,aFakeCmnFrm.MinFrmWd);
end;

type
	TFakeCmnFrm2=object(commonFrameWindow)
		constructor Init;
		function MinFrmHt:word; virtual;
		function MinFrmWd:word; virtual;
	end;

constructor TFakeCmnFrm2.Init; begin end;
function TFakeCmnFrm2.MinFrmHt:word; begin MinFrmHt:= 6225; end;
function TFakeCmnFrm2.MinFrmWd:word; begin MinFrmWd:= 1238; end;

procedure TestTCmnFrmWmGetMinMaxInfo; 

var
	aFakeCmnFrm:TFakeCmnFrm2;
	aInfo:TMINMAXINFO;

begin
	{ must set the message member values to the return values from MinFrmHt and MinFrmWd functions }
	aFakeCmnFrm.Init;
	aFakeCmnFrm.DoGetMinMaxInfo(@aInfo);
	punit.Assert.Equal(aFakeCmnFrm.MinFrmWd, aInfo.ptMinTrackSize.x);
	punit.Assert.Equal(aFakeCmnFrm.MinFrmHt, aInfo.ptMinTrackSize.y);
	{ must return zero to indicate message was processed }
	punit.Assert.EqualLong(0,aFakeCmnFrm.DoGetMinMaxInfo(@aInfo));
end;

procedure TestTCmnAppInitialize; 

var
	aFakeTCmnApp:FakeTCmnApp;

begin
	aFakeTCmnApp.Init;
	aFakeTCmnApp.brand_name:='Brand';
	aFakeTCmnApp.game_name:='Title';
	aFakeTCmnApp.Initialize;
	punit.Assert.EqualText('Brand Title',aFakeTCmnApp.game_title);
end;

type
	TestTCmnApp=object(FakeTCmnApp)
	end;

procedure Test_InitializeStorageName;

var
	aApp:TestTCmnApp;
	aStorageName:stringBuffer;

begin
	aApp.Init;
	aApp.InitializeStorageName(aStorageName,'','');
	punit.Assert.EqualText('',aStorageName);
	aApp.InitializeStorageName(aStorageName,'Brand','Title');
	punit.Assert.EqualText('BrandTitle',aStorageName);
end;

procedure Test_backwards_compatibility;

begin
	punit.Assert.EqualText('Tabletop',KEY_TABLETOP);	
	punit.Assert.EqualText('ColorR',KEY_TABLETOP_COLOR_R);	
	punit.Assert.EqualText('ColorG',KEY_TABLETOP_COLOR_G);	
	punit.Assert.EqualText('ColorB',KEY_TABLETOP_COLOR_B);	
end;

procedure Test_resources;

begin
	punit.Assert.IsTrue(FindResource(hInstance,'CARD_FLICK','WAVE')<>0);
end;

type
	TestBaseFrame=object(stdwinFrameWindow)
		constructor Init;		
		procedure OnFileNewTemplate; virtual;
	end;

constructor TestBaseFrame.Init; begin end;
procedure TestBaseFrame.OnFileNewTemplate; begin end;

procedure Test_BaseFrame_OnCmd;
var
	tester:TestBaseFrame;
begin
	tester.Init;
	AssertAreEqual(1,tester.OnCmd(0));
	AssertAreEqual(0,tester.OnCmd(CM_FILENEW));
end;

constructor BaseAppStub.Construct;
begin
	inherited Construct('test','storage');
end;

procedure BaseAppStub.InitMainWindow; begin end;

function BaseAppStub.GetBooleanData(aKey,aSubKey:pchar;default:boolean):boolean; 
begin
	GetBooleanData:=GetBooleanData_result;
end;

procedure Test_UpdateTitle;

begin
	punit.Assert.EqualStr('Brand Game (addendum)',CreateFrameTitle('Brand Game', 'addendum'));
	punit.Assert.EqualStr('Brand Game',CreateFrameTitle('Brand Game', ''));
end;

constructor ApplicationStub.Construct; begin end;
function ApplicationStub.HomePageUrl:pchar; begin HomePageUrl:=''; end;
function ApplicationStub.DonatePageUrl:pchar; begin DonatePageUrl:=''; end;
function ApplicationStub.FullName:PChar; begin FullName:=''; end;
procedure ApplicationStub.InitMainWindow; begin end;
	
type
	Test_QuickApp=object(ApplicationStub)
	end;

constructor FrameStub.Construct; 
begin 
end;

constructor FrameStub.Construct(app:ApplicationP); 
begin 
	myapp:=app;
end;

constructor FrameWindowStub.Construct;
begin
end;

type
	FakeFrameWindow=object(FrameWindowStub)
		Maximized_result:boolean;
		function Maximized:boolean; virtual;
		procedure SimulateMaximizedState(state:boolean);
	end;

function FakeFrameWindow.Maximized:boolean; 
begin 
	Maximized:=Maximized_result; 
end;

procedure FakeFrameWindow.SimulateMaximizedState(state:boolean); 
begin 
	Maximized_result:=state; 
end;

procedure test_ShouldAutosize;
const
	VALID_BITMAP_HANDLE:HBITMAP=1;
var
	fake_frame:FakeFrameWindow;
begin
	fake_frame.Construct;
	fake_frame.SimulateMaximizedState(TRUE);
	AssertIsFalse(fake_frame.ShouldAutoSize(TRUE,VALID_BITMAP_HANDLE));
	fake_frame.SimulateMaximizedState(FALSE);
	AssertIsTrue(fake_frame.ShouldAutoSize(TRUE,VALID_BITMAP_HANDLE));
	AssertIsFalse(fake_frame.ShouldAutoSize(FALSE,VALID_BITMAP_HANDLE));
	AssertIsFalse(fake_frame.ShouldAutoSize(TRUE,NULL_HANDLE));
end;

procedure Test_ValidateBgImagePath;
const
	VALID_BITMAP_HANDLE:HBITMAP=1;
var
	testApp:Test_QuickApp;
begin
	testApp.Construct;
	testApp.BgImagePath:='existing path name';
	testApp.ValidateBgImagePath(VALID_BITMAP_HANDLE);
	AssertAreEqual('existing path name',testApp.BgImagePath);
	testApp.ValidateBgImagePath(NULL_HANDLE);
	AssertAreEqual('',testApp.BgImagePath);
end;

procedure Test_AnimationSetting_OnChanged;
var
	testee:AnimationSetting;
	fake_app:BaseAppStub;
begin
	fake_app.Construct;
	testee.Construct(@fake_app,0,123,'key','subkey',FALSE);
	testee.OnChanged(FALSE);
	AssertAreEqual(1,AnimateSteps);
	testee.OnChanged(TRUE);
	AssertAreEqual(FDSTEP,AnimateSteps);
end;

begin
	Suite.Add(@Test_resources);
	Suite.Add(@Test_backwards_compatibility);
	Suite.Add(@TestTCmnAppInitialize);
	Suite.Add(@TestTCmnFrmInitFrmPos);
	Suite.Add(@TestTCmnFrmInitFrmSize);
	Suite.Add(@TestTCmnFrmMinFrmHt);
	Suite.Add(@TestTCmnFrmMinFrmWd);
	Suite.Add(@TestTCmnFrmWmGetMinMaxInfo);
	Suite.Add(@Test_OnHomePage_return_code);
	Suite.Add(@Test_InitializeStorageName);
	Suite.Add(@Test_BaseFrame_OnCmd);
	Suite.Add(@test_UpdateTitle);
	Suite.Add(@test_ValidateBgImagePath);
	Suite.Add(@test_ShouldAutosize);
	Suite.Add(@Test_AnimationSetting_OnChanged);
	Suite.Run('quickWinTests');
end.
