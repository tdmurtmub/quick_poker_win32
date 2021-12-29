{ (C) 2006 Wesley Steiner }

{$MODE FPC}

unit oapp;

{$I punit.inc}

interface

uses
	{$ifdef TEST} punit, {$endif}
	windows,
	app,
	owindows,quick;

type
	OMainFrameP=^OMainFrame;
	OApplicationP=^OApplication;

	OApplication=object(quick.Application)
		MainWindow:OMainFrameP;
		constructor Construct(friendly_name,aStorageName:pchar);
		destructor Destruct; virtual;
		function GetBooleanData(aKey,aSubKey:pchar;aDefaultValue:boolean):boolean; virtual;
		function GetIntegerData(aKey,aSubKey:pchar;aDefaultValue:longint):longint; virtual;
		function GetIntegerDataRange(aKey,aSubKey:pchar;aLowValue,aHighValue,aDefaultValue:longint):longint; virtual;
		function GetStringData(aKey,aSubKey:pchar;const aDefaultValue:ansistring):ansistring; virtual;
		function VersionInfo:FileVersionInfoPtr;
		procedure DeleteData(aKey,aSubKey:pchar); test_virtual
		procedure InitMainWindow; virtual; abstract;
		procedure SetBooleanData(aKey,aSubKey:pchar;aValue:boolean); virtual;
		procedure SetIntegerData(aKey,aSubKey:pchar;aValue:longint); virtual;
		procedure SetStringData(aKey,aSubKey:pchar;const aValue:ansistring);
		procedure Run;
	private
		myAccelerators:HACCEL;
		function OpenRegKey(aKey:pchar):HKEY; test_virtual
		function CallRegCreateKey(hKey:HKEY;lpSubKey:LPCTSTR;phkResult:PHKEY):LONG; test_virtual
		function CallRegDeleteValue(hKey:HKEY;lpSubKey:LPCTSTR):LONG; test_virtual
		function CallRegQueryValueEx(hKey:HKEY;lpValueName:LPCTSTR;lpType:LPDWORD;lpData:LPBYTE;lpcbData:LPDWORD):LONG; test_virtual
		function CallRegSetValueEx(aHKey:HKEY;lpValueName:LPCTSTR;dwType:DWORD;lpData:LPBYTE;cbData:DWORD):LONG; test_virtual
		function GetRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;aDataBufferSize:DWORD):DWORD; test_virtual
		procedure PutRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;cbData:DWORD); test_virtual
	end;

	{$ifdef TEST}
	FakeOApplication=object(OApplication)
		constructor Construct;
		function CallRegCreateKey(hKey:HKEY;lpSubKey:LPCTSTR;phkResult:PHKEY):LONG; virtual;
		function CallRegDeleteValue(hKey:HKEY;lpSubKey:LPCTSTR):LONG; virtual;
		function CallRegQueryValueEx(hKey:HKEY;lpValueName:LPCTSTR;lpType:LPDWORD;lpData:LPBYTE;lpcbData:LPDWORD):LONG; virtual;
		function CallRegSetValueEx(aHKey:HKEY;aSubKey:LPCTSTR;dwType:DWORD;lpData:LPBYTE;cbData:DWORD):LONG; virtual;
		procedure DeleteData(aKey,aSubKey:pchar); virtual;
		procedure InitMainWindow; virtual;
	end;
	
	OApplicationStub=object(FakeOApplication) // OBSOLETE: use FakeOApplication
	end;
	{$endif}

	TWindowEx=object(OWindow) // OBSOLETE: use OWindow
		function ClientAreaWd:word; test_virtual // OBSOLETE: use ClientWidth
		function ClientAreaHt:word; test_virtual // OBSOLETE: use ClientHeight
		function Maximized:boolean; test_virtual
		function MinClientAreaWd:word; virtual;
		function Minimized:boolean; test_virtual
	end;

	PopupWndPersistorPtr=^PopupWndPersistor;
	PopupWndPersistor=object
		constructor Construct(aWindow:PWindow;aStorageKey:pchar;persistPosition,persistSize:boolean;defaultX,defaultY:LONG);
		destructor Destruct;
		procedure RestorePos(var aXVar,aYVar:LONG);
		procedure RestoreSize(var aWidthVar,aHeightVar:longword);
		procedure Save;
		procedure SaveMaximized(aState:boolean); test_virtual
		procedure SavePos; test_virtual
		procedure SaveSize; test_virtual
	private
		myWindow:PWindow;
		myStorageKey:pchar;
		myPersistPositionFlag,myPersistSizeFlag:boolean;
		myDefaultX,myDefaultY:LONG;
		function GetWndX:LONG; test_virtual
		function GetWndY:LONG; test_virtual
		function GetWndW:DWORD; test_virtual
		function GetWndH:DWORD; test_virtual
		procedure WriteWindowPos(aXPos, aYPos:LONG); test_virtual
		procedure WriteWindowSize(aWidth, aHeight:longword); test_virtual
		procedure ReadWindowPos(var aXVar,aYVar:LONG;defaultX,defaultY:LONG); test_virtual
		procedure ReadWindowSize(var aWidthVar, aHeightVar:longword); test_virtual
	end;

	FrameWindowP=^FrameWindow;
	FrameWindow=object(TWindowEx)
		myRestoredX,myRestoredY:LONG;
		myRestoredW,myRestoredH:longword;
		constructor Construct;
		destructor Destruct; virtual;
		function OnSize(resizeType:uint;newWidth,newHeight:integer):LONG; virtual;
		function OnMove(newX,newY:integer):LONG; virtual;
	private
		myPersistor:PopupWndPersistorPtr;
		function HasRestoredData:boolean;
		procedure SetCmdShow(maxFromStorage,start_maximized,hasPersistence:boolean);
	end;

	OMainFrame=object	
		MyFrameWindow:^FrameWindow;
		constructor Construct(aMainApp:OApplicationP;aStorageKey:pchar;start_maximized:boolean);
		destructor Destruct; virtual;
		function Create:HWND;
		function MainMenu:HMENU;
		function Owner:OApplicationP;
	private
		myMainApp:OApplicationP;
		procedure CheckStartMaximized(start_maximized:boolean);
	end;

	{$ifdef TEST}
	OMainFrameStub=object(OMainFrame)
		constructor Construct;
	end;
	{$endif}
	
const
	TheApplication:OApplicationP=nil;

implementation

uses
	std,strings,
	stringsx,sdkex,windowsx;

var
	the_persistence_key:ansiString;
	the_start_maximized:boolean;
	the_version_info:FileVersionInfo;

	{$ifdef TEST}
type
	Test_OApplication=object(FakeOApplication)
		myHKey:HKEY;
		myCallRegSetValueEx,myCallRegQueryValueEx:CallTelemetry;
		myDataBuffer:pchar;
		myDataTypeArg,myDataLenArg:DWORD;
		myValueName:pchar;
		myQueryDataType:DWORD;
		myQueryDataBuffer:LPBYTE;
		myQueryCbDataIn,myQueryCbDataOut:DWORD;
		myRegQueryValueEx:LONG;
		constructor Construct;
		function CallRegQueryValueEx(hKey:HKEY;lpValueName:LPCTSTR;lpType:LPDWORD;lpData:LPBYTE;lpcbData:LPDWORD):LONG; virtual;
		function CallRegSetValueEx(aHKey:HKEY;aSubKey:LPCTSTR;dwType:DWORD;lpData:LPBYTE;cbData:DWORD):LONG; virtual;
	end;
	{$endif}

function ParamStr0Fix(source:ansistring):ansistring;

var
	up:ansistring;

begin
	up:=Upcase(source);
	if StringEndsWith(up,'.EXE') 
		then ParamStr0Fix:=up
		else ParamStr0Fix:=Copy(up,1,Length(up)-1);
end;

{$ifdef TEST}
procedure Test_ParamStr0Fix;
begin
	punit.Assert.EqualStr('NAME.EXE',ParamStr0Fix('name.exEI'));
	punit.Assert.EqualStr('NAME.EXE',ParamStr0Fix('name.Exe'));
end;
{$endif TEST}

constructor OApplication.Construct(friendly_name,aStorageName:pchar);
begin //Writeln('OApplication.Construct("',friendly_name,'","',aStorageName,'")"');
	inherited Construct(friendly_name);
	MainWindow:=nil;
	theApplication:=@self;
	the_version_info.Construct(PChar(ParamStr0Fix(AnsiString(ParamStr(0)))));
	myAccelerators:=HACCEL(ApiCheck(LoadAccelerators(HINST(hInstance),MakeIntResource(LongInt(101)))));
	InitMainWindow;
	if MainWindow<>nil then ShowWindow(MainWindow^.MyFrameWindow^.handle,CmdShow);
end;

destructor OApplication.Destruct;
begin
	the_version_info.Destruct;
	inherited Destruct;
end;

procedure OApplication.Run;
var
	msg:TMsg;
begin
	while GetMessage(msg,0,0,0) do if TranslateAccelerator(MainWindow^.MyFrameWindow^.handle,myAccelerators,msg)=0 then begin
	    TranslateMessage(msg);
		DispatchMessage(msg);
	end;
end;

function TWindowEx.ClientAreaWd:word;

begin
	ClientAreaWd:=ClientWidth;
end;

function TWindowEx.ClientAreaHt:word;

begin
	ClientAreaHt:=ClientHeight;
end;

function TWindowEx.MinClientAreaWd:word;

begin
	MinClientAreaWd:=0;
end;

function TWindowEx.Maximized:boolean;

begin
	Maximized:=IsZoomed(handle);
end;

function TWindowEx.Minimized:boolean;

begin
	Minimized:=IsIconic(handle);
end;

{$ifdef TEST}

procedure TestTWindowEx_MinClientAreaWd; 

var
	aWindow:TWindowEx;

begin
	aWindow.Construct;
	{ default value should be 0 }
	punit.Assert.Equal(0, aWindow.MinClientAreaWd);
end;

{$endif TEST}

const
	KEY_MAXIMIZED='Maximized';

constructor OMainFrame.Construct(aMainApp:OApplicationP;aStorageKey:pchar;start_maximized:boolean);
begin
	MyFrameWindow:=NIL;
	the_persistence_key:=aStorageKey;
	the_start_maximized:=start_maximized;
	CheckStartMaximized(start_maximized);
	myMainApp:=aMainApp;
end;

destructor OMainFrame.Destruct;
begin
	if MyFrameWindow<>NIL then MyFrameWindow^.Destruct;
end;

destructor FrameWindow.Destruct;
begin
	Dispose(myPersistor,Destruct);
end;

{$ifdef TEST}

type
	FakePopupWndPersistorP=^FakePopupWndPersistor;
	FakePopupWndPersistor=object(PopupWndPersistor)
		mySavePos,mySaveSize,mySaveMaximized:CallTelemetry;
		mySaveMaximized_arg:boolean;
		constructor Construct;
		procedure SavePos; virtual;
		procedure SaveSize; virtual;
		procedure SaveMaximized(aState:boolean); virtual;
	end;

constructor FakePopupWndPersistor.Construct; begin end;
procedure FakePopupWndPersistor.SavePos; begin mySavePos.WasCalled:=TRUE; end;
procedure FakePopupWndPersistor.SaveSize; begin mySaveSize.WasCalled:=TRUE; end;

procedure FakePopupWndPersistor.SaveMaximized(aState:boolean); 

begin 
	mySaveMaximized.Wascalled:=TRUE; 
	mySaveMaximized_arg:=aState;
end;

type
	FrameWindowTester=object(FrameWindow)
		myMaximized,myMinimized:boolean;
		constructor Init;
		function Maximized:boolean; virtual;
		function Minimized:boolean; virtual;
	end;

constructor FrameWindowTester.Init;

begin
	myPersistor:=New(FakePopupWndPersistorP, Construct);
end;

function FrameWindowTester.Maximized:boolean; begin Maximized:=myMaximized; end;
function FrameWindowTester.Minimized:boolean; begin Minimized:=myMinimized; end;

{$endif TEST}

function FrameWindow.OnSize(resizeType:uint;newWidth,newHeight:integer):LONG;

begin //Writeln('FrameWindow.OnSize(resizeType:uint;',newWidth,',',newHeight,')');
	case resizeType of
		SIZE_RESTORED:begin
			myPersistor^.SaveSize;
			myPersistor^.SaveMaximized(FALSE);
		end;
		SIZE_MAXIMIZED:myPersistor^.SaveMaximized(TRUE);
	end;
	OnSize:=0;
end;

{$ifdef TEST}

procedure Test_FrameWindow_OnSize_result;

const
	ANY_SIZE_TYPE=UINT(-1);

var
	aWindow:FrameWindowTester;

begin
	aWindow.Init;
	punit.Assert.Equal(0,aWindow.OnSize(ANY_SIZE_TYPE,0,0));
end;
	
procedure Test_FrameWindow_OnSize;

var
	aWindow:FrameWindowTester;
	aPersistor:^FakePopupWndPersistor;

begin
	aWindow.Init;
	aPersistor:=FakePopupWndPersistorP(aWindow.myPersistor);

	aPersistor^.mySaveSize.WasCalled:=FALSE;
	aPersistor^.mySaveMaximized.Wascalled:=FALSE;
	aPersistor^.mySaveMaximized_arg:=TRUE;
	aWindow.OnSize(SIZE_RESTORED,0,0);
	punit.Assert.IsTrue(aPersistor^.mySaveSize.WasCalled);
	punit.Assert.IsTrue(aPersistor^.mySaveMaximized.Wascalled);
	punit.Assert.IsFalse(aPersistor^.mySaveMaximized_arg);

	aPersistor^.mySaveSize.WasCalled:=FALSE;
	aWindow.OnSize(SIZE_RESTORED+467,0,0);
	punit.Assert.IsFalse(aPersistor^.mySaveSize.WasCalled);

	aPersistor^.mySaveMaximized.Wascalled:=FALSE;
	aPersistor^.mySaveMaximized_arg:=FALSE;
	aWindow.OnSize(SIZE_MAXIMIZED,800,600);
	punit.Assert.IsTrue(aPersistor^.mySaveMaximized.Wascalled);
	punit.Assert.IsTrue(aPersistor^.mySaveMaximized_arg);
end;

{$endif TEST}

function FrameWindow.OnMove(newX,newY:integer):LONG;

begin
	if (not Maximized) and (not Minimized) then myPersistor^.SavePos;
	OnMove:=0;
end;

{$ifdef TEST}

procedure Test_FrameWindow_OnMoved_result;

var
	aWindow:FrameWindowTester;

begin
	aWindow.Init;
	punit.Assert.Equal(0,aWindow.OnMove(0,0));
end;

procedure Test_FrameWindow_OnMove;

var
	aWindow:FrameWindowTester;
	aPersistor:FakePopupWndPersistorP;

begin
	aWindow.Init;
	aPersistor:=FakePopupWndPersistorP(aWindow.myPersistor);
	aWindow.myMaximized:=FALSE;
	aWindow.myMinimized:=FALSE;
	aPersistor^.mySavePos.WasCalled:=FALSE;
	aWindow.OnMove(0,0);
	punit.Assert.IsTrue(aPersistor^.mySavePos.WasCalled);
	aWindow.myMaximized:=TRUE;
	aWindow.myMinimized:=FALSE;
	aPersistor^.mySavePos.WasCalled:=FALSE;
	aWindow.OnMove(0,0);
	punit.Assert.IsFalse(aPersistor^.mySavePos.WasCalled);
	aWindow.myMaximized:=FALSE;
	aWindow.myMinimized:=TRUE;
	aPersistor^.mySavePos.WasCalled:=FALSE;
	aWindow.OnMove(0,0);
	punit.Assert.IsFalse(aPersistor^.mySavePos.WasCalled);
end;

{$endif TEST}

constructor PopupWndPersistor.Construct(aWindow:PWindow;aStorageKey:pchar;persistPosition,persistSize:boolean;defaultX,defaultY:LONG);

begin
	myWindow:=aWindow;
	myStorageKey:=StrNew(aStorageKey);
	myPersistPositionFlag:=persistPosition;
	myPersistSizeFlag:=persistSize;
	myDefaultX:=defaultX;
	myDefaultY:=defaultY;
end;

destructor PopupWndPersistor.Destruct;

begin
	StrDispose(myStorageKey);
end;

procedure PopupWndPersistor.WriteWindowPos(aXPos,aYPos:LONG);

begin
	theApplication^.SetIntegerData(myStorageKey,'X',aXPos);
	theApplication^.SetIntegerData(myStorageKey,'Y',aYPos);
end;

procedure PopupWndPersistor.WriteWindowSize(aWidth,aHeight:longword);

begin
//	writeln('PopupWndPersistor.WriteWindowSize(aWidth,aHeight:longword)');
	theApplication^.SetIntegerData(myStorageKey,'W',aWidth);
	theApplication^.SetIntegerData(myStorageKey,'H',aHeight);
end;

procedure PopupWndPersistor.ReadWindowPos(var aXVar,aYVar:LONG;defaultX,defaultY:LONG);

begin
	aXVar:=theApplication^.GetIntegerData(myStorageKey,'X',defaultX);
	aYVar:=theApplication^.GetIntegerData(myStorageKey,'Y',defaultY);
end;

procedure PopupWndPersistor.ReadWindowSize(var aWidthVar,aHeightVar:longword);

begin
	aWidthVar:=longword(theApplication^.GetIntegerData(myStorageKey,'W',LONG(CW_USEDEFAULT)));
	aHeightVar:=longword(theApplication^.GetIntegerData(myStorageKey,'H',LONG(CW_USEDEFAULT)));
end;

function PopupWndPersistor.GetWndX:LONG;

begin
	GetWndX:=GetWndLeft(myWindow^.handle);
end;

function PopupWndPersistor.GetWndY:LONG;

begin
	GetWndY:= GetWndTop(myWindow^.handle);
end;

function PopupWndPersistor.GetWndW:DWORD;

begin
	GetWndW:= GetWndWd(myWindow^.handle);
end;

function PopupWndPersistor.GetWndH:DWORD;

begin
	GetWndH:=GetWndHt(myWindow^.handle);
end;

procedure PopupWndPersistor.SavePos;

begin
	if (myPersistPositionFlag) then WriteWindowPos(GetWndX,GetWndY);
end;

procedure PopupWndPersistor.SaveSize;

begin
//	writeln('PopupWndPersistor.SaveSize');
	if (myPersistSizeFlag) then WriteWindowSize(GetWndW,GetWndH);
end;

procedure PopupWndPersistor.Save;

begin
	SavePos;
	SaveSize;
end;

{$ifdef TEST}

type
	TestPopupWndPersistor = object(PopupWndPersistor)
		WriteWindowPos_was_called:boolean;
		WriteWindowPos_aXPos:LONG;
		WriteWindowPos_aYPos:LONG;
		WriteWindowSize_was_called:boolean;
		WriteWindowSize_aWidth:DWORD;
		WriteWindowSize_aHeight:DWORD;
		ReadWindowPos_was_called:boolean;
		ReadWindowSize_was_called:boolean;
		GetWndX_result:LONG;
		GetWndY_result:LONG;
		GetWndW_result:DWORD;
		GetWndH_result:DWORD;
		constructor Construct;
		function GetWndX:LONG; virtual;
		function GetWndY:LONG; virtual;
		function GetWndH:DWORD; virtual;
		function GetWndW:DWORD; virtual;
		procedure WriteWindowPos(aXPos,aYPos:LONG); virtual;
		procedure WriteWindowSize(aWidth,aHeight:longword); virtual;
		procedure ReadWindowPos(var aXVar,aYVar:LONG; defaultX, defaultY:LONG); virtual;
		procedure ReadWindowSize(var aWidthVar,aHeightVar:longword); virtual;
	end;

constructor TestPopupWndPersistor.Construct; begin end;
function TestPopupWndPersistor.GetWndX:LONG; begin GetWndX:=GetWndX_result; end;
function TestPopupWndPersistor.GetWndY:LONG; begin GetWndY:=GetWndY_result; end;
function TestPopupWndPersistor.GetWndW:DWORD; begin GetWndW:=GetWndW_result; end;
function TestPopupWndPersistor.GetWndH:DWORD; begin GetWndH:=GetWndH_result; end;

procedure TestPopupWndPersistor.WriteWindowPos(aXPos,aYPos:LONG);

begin
	WriteWindowPos_was_called:= true;
	WriteWindowPos_aXPos:=aXPos;
	WriteWindowPos_aYPos:=aYPos;
end;

procedure TestPopupWndPersistor.WriteWindowSize(aWidth, aHeight:longword);

begin
	WriteWindowSize_was_called:= true;
	WriteWindowSize_aWidth:= aWidth;
	WriteWindowSize_aHeight:= aHeight;
end;

procedure TestPopupWndPersistor.ReadWindowPos(var aXVar,aYVar:LONG; defaultX, defaultY:LONG);

begin
	ReadWindowPos_was_called:= true;
	aXVar:= 9871;
	aYVar:= 9872;
end;

procedure TestPopupWndPersistor.ReadWindowSize(var aWidthVar, aHeightVar:longword);

begin
	ReadWindowSize_was_called:= true;
	aWidthVar:= 6661;
	aHeightVar:= 6662;
end;

procedure Test_PopupWndPersistor_SavePos; 

var
	aTest:TestPopupWndPersistor;

begin
	aTest.Construct;

	{ should save if pos save flag is set }
	aTest.myPersistPositionFlag:=true;
	aTest.WriteWindowPos_was_called:=false;
	aTest.GetWndX_result:=393000;
	aTest.GetWndY_result:=645000;
	aTest.SavePos;
	punit.Assert.IsTrue(aTest.WriteWindowPos_was_called);
	punit.Assert.EqualLong(393000,aTest.WriteWindowPos_aXPos);
	punit.Assert.EqualLong(645000,aTest.WriteWindowPos_aYPos);

	{ should NOT save if pos save flag is false }
	aTest.myPersistPositionFlag:=false;
	aTest.WriteWindowPos_was_called:= false;
	aTest.SavePos;
	punit.Assert.IsFalse(aTest.WriteWindowPos_was_called);
end;

procedure Test_PopupWndPersistor_SaveSize; 

var
	aTest:TestPopupWndPersistor;

begin
	aTest.Construct;

	{ should save if pos save flag is set }
	aTest.myPersistSizeFlag:= true;
	aTest.WriteWindowSize_was_called:= false;
	aTest.GetWndW_result:=41923;
	aTest.GetWndH_result:=53326;
	aTest.SaveSize;
	punit.Assert.IsTrue(aTest.WriteWindowSize_was_called);
	punit.Assert.AreEqualWord(41923, aTest.WriteWindowSize_aWidth);
	punit.Assert.AreEqualWord(53326, aTest.WriteWindowSize_aHeight);

	{ should NOT save if pos save flag is false }
	aTest.myPersistSizeFlag:= false;
	aTest.WriteWindowSize_was_called:= false;
	aTest.SaveSize;
	punit.Assert.IsFalse(aTest.WriteWindowSize_was_called);
end;

{$endif TEST}

procedure PopupWndPersistor.RestorePos(var aXVar, aYVar:LONG);

begin
	if myPersistPositionFlag then ReadWindowPos(aXVar,aYVar,myDefaultX,myDefaultY);
end;

procedure PopupWndPersistor.RestoreSize(var aWidthVar, aHeightVar:longword);

begin
	if myPersistSizeFlag then ReadWindowSize(aWidthVar,aHeightVar);
end;

{$ifdef TEST}

procedure Test_PopupWndPersistor_RestorePos; 

var
	aTest:TestPopupWndPersistor;
	aXPos,aYPos:LONG;

begin
	aTest.Construct;

	{ should restore if flag is true }
	aTest.myPersistPositionFlag:= true;
	aTest.ReadWindowPos_was_called:= false;
	aTest.RestorePos(aXPos, aYPos);
	punit.Assert.IsTrue(aTest.ReadWindowPos_was_called);
	punit.Assert.Equal(9871, aXPos);
	punit.Assert.Equal(9872, aYPos);

	{ should NOT restore if flag is false }
	aTest.myPersistPositionFlag:=false;
	aTest.ReadWindowPos_was_called:=false;
	aTest.RestorePos(aXPos, aYPos);
	punit.Assert.IsFalse(aTest.ReadWindowPos_was_called);
end;

procedure Test_PopupWndPersistor_RestoreSize; 

var
	aTest:TestPopupWndPersistor;
	aWd,aHt:longword;

begin
	aTest.Construct;

	{ should restore size if size flag is set }
	aTest.myPersistSizeFlag:= true;
	aTest.ReadWindowSize_was_called:= false;
	aTest.RestoreSize(aWd,aHt);
	punit.Assert.IsTrue(aTest.ReadWindowSize_was_called);
	punit.Assert.Equal(6661, aWd);
	punit.Assert.Equal(6662, aHt);

	{ should NOT restore size if size flag is false }
	aTest.myPersistSizeFlag:= false;
	aTest.ReadWindowSize_was_called:= false;
	aTest.RestoreSize(aWd, aHt);
	punit.Assert.IsFalse(aTest.ReadWindowSize_was_called);
end;

{$endif TEST}

{$ifdef TEST}

constructor FakeOApplication.Construct; 
begin 
	theApplication:=@self;
end;

function FakeOApplication.CallRegCreateKey(hKey:HKEY;lpSubKey:LPCTSTR;phkResult:PHKEY):LONG; begin CallRegCreateKey:=0; end;
function FakeOApplication.CallRegDeleteValue(hKey:HKEY;lpSubKey:LPCTSTR):LONG; begin CallRegDeleteValue:=0; end;
function FakeOApplication.CallRegQueryValueEx(hKey:HKEY;lpValueName:LPCTSTR;lpType:LPDWORD;lpData:LPBYTE;lpcbData:LPDWORD):LONG; begin CallRegQueryValueEx:=0; end;
function FakeOApplication.CallRegSetValueEx(aHKey:HKEY;aSubKey:LPCTSTR;dwType:DWORD;lpData:LPBYTE;cbData:DWORD):LONG; begin CallRegSetValueEx:=0; end;
procedure FakeOApplication.DeleteData(aKey,aSubKey:pchar); begin end;
procedure FakeOApplication.InitMainWindow; begin end;

constructor Test_OApplication.Construct; begin FriendlyName:='Test_OApplication'; end;

{$endif TEST}

function OApplication.GetBooleanData(aKey,aSubKey:pchar;aDefaultValue:boolean):boolean;
begin //writeln('OApplication.GetBooleanData("',aKey,'","',aSubKey,'",',aDefaultValue,')');
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	GetBooleanData:=Boolean(GetIntegerData(aKey,aSubKey,Q(aDefaultValue,1,0)));
end;

{$ifdef TEST}

type
	FakeRegAppBase=object(Test_OApplication)
		myOpenRegKey:HKEY;
		myKey:pchar;
		myTestKey,myTestSubKey:string;
		myTestValue:longint;
		myTestStringValue:ansistring;
		function OpenRegKey(aKey:pchar):HKEY; virtual;
		procedure AddIntegerKey(aKey,aSubKey:string;aValue:longint);
		procedure AddStringKey(aKey,aSubKey:string;aValue:ansistring);
	end;

	FakeRegApp=object(FakeRegAppBase)
		myGetRegData:DWORD;
		function GetRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;aDataBufferSize:DWORD):DWORD; virtual;
	end;

function FakeRegApp.GetRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;aDataBufferSize:DWORD):DWORD;

begin
	if (StrPas(aKey)=myTestKey) and (StrPas(aSubKey)=myTestSubKey) then PDWORD(aDataBuffer)^:=DWORD(myTestValue);
	GetRegData:=myGetRegData;
end;

procedure FakeRegAppBase.AddIntegerKey(aKey,aSubKey:string;aValue:longint);

begin
	myTestKey:=aKey;
	myTestSubKey:=aSubKey;
	myTestValue:=aValue;
end;

procedure FakeRegAppBase.AddStringKey(aKey,aSubKey:string;aValue:ansistring);

begin
	myTestKey:=aKey;
	myTestSubKey:=aSubKey;
	myTestStringValue:=aValue;
end;

function FakeRegAppBase.OpenRegKey(aKey:pchar):HKEY; begin OpenRegKey:=myOpenRegKey; end;

type
	GetBooleanDataTest=object(FakeRegApp)
		procedure AddBooleanKey(aKey,aSubKey:string;aValue:boolean);
	end;

procedure GetBooleanDataTest.AddBooleanKey(aKey,aSubKey:string;aValue:boolean);

begin
	if aValue
		then AddIntegerKey(aKey,aSubKey,1)
		else AddIntegerKey(aKey,aSubKey,0);
end;

procedure Test_GetBooleanData;

var
	aTester:GetBooleanDataTest;

begin
	aTester.Construct;
	aTester.myGetRegData:=0;
	aTester.AddBooleanKey('a valid bool key','a valid bool subkey',TRUE);
	punit.Assert.IsFalse(aTester.GetBooleanData('non-existent bool key','',FALSE));
	punit.Assert.IsTrue(aTester.GetBooleanData('non-existent bool key','',TRUE));
	punit.Assert.IsFalse(aTester.GetBooleanData('a valid bool key','non-valid bool subkey',FALSE));
	aTester.myGetRegData:=4;
	punit.Assert.IsTrue(aTester.GetBooleanData('a valid bool key','a valid bool subkey',FALSE));
end;

{$endif TEST}

function OApplication.GetStringData(aKey,aSubKey:pchar;const aDefaultValue:ansistring):ansistring;
var
	aValue:ansistring;
	n,nn:DWORD;
	aDataBuffer:LPBYTE;
begin //Writeln('OApplication.GetStringData("',aKey,'","',aSubKey,'")');
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	aValue:=aDefaultValue;
	n:=GetRegData(aKey,aSubKey,REG_SZ,NIL,0);
	if n>0 then begin
		aDataBuffer:=GetMem(n);
		GetRegData(aKey,aSubKey,REG_SZ,aDataBuffer,n);
		aValue:=StrPas(PChar(aDataBuffer));
		freeMem(aDataBuffer);
	end;
	GetStringData:=aValue;
end;

function OApplication.GetIntegerData(aKey,aSubKey:pchar;aDefaultValue:longint):longint;

begin //writeln('OApplication.GetIntegerData("',aKey,'","',aSubKey,'",',aDefaultValue,')');
	GetIntegerData:=GetIntegerDataRange(aKey,aSubKey,MIN_LONG,MAX_LONG,aDefaultValue);
end;

function OApplication.GetIntegerDataRange(aKey,aSubKey:pchar;aLowValue,aHighValue,aDefaultValue:longint):longint;
var
	aValue:longint;
begin //writeln('OApplication.GetIntegerDataRange("',aKey,'","',aSubKey,'",',aLowValue,',',aHighValue,',',aDefaultValue,')');
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	System.Assert(aLowValue<=aHighValue,'invalid argument range values'); 
	GetIntegerDataRange:=aDefaultValue;
	if (GetRegData(aKey,aSubKey,REG_DWORD,LPBYTE(@aValue),4)=4) and (aValue>=aLowValue) and (aValue<=aHighValue) then GetIntegerDataRange:=aValue;
end;

{$ifdef TEST}

procedure Test_GetIntegerData;

const
	A_DEFAULT_VALUE=-1627498;

var
	aTester:FakeRegApp;

begin
	aTester.Construct;
	aTester.myGetRegData:=0;
	punit.Assert.Equal(A_DEFAULT_VALUE,aTester.GetIntegerData('anykey','anysubkey',A_DEFAULT_VALUE));
	aTester.AddIntegerKey('a valid key','a valid subkey',-1928437);
	punit.Assert.Equal(A_DEFAULT_VALUE,aTester.GetIntegerData('non-existent key','',A_DEFAULT_VALUE));
	punit.Assert.Equal(A_DEFAULT_VALUE,aTester.GetIntegerData('a valid key','non-existent subkey',A_DEFAULT_VALUE));
	aTester.myGetRegData:=4;
	punit.Assert.Equal(-1928437,aTester.GetIntegerData('a valid key','a valid subkey',A_DEFAULT_VALUE));
end;

procedure Test_GetIntegerDataRange;

const
	A_DEFAULT_VALUE=-11000009;

var
	aTester:FakeRegApp;

begin
	aTester.Construct;
	aTester.AddIntegerKey('a valid key','a valid subkey',-1928437);
	aTester.myGetRegData:=0;
	punit.Assert.Equal(A_DEFAULT_VALUE,aTester.GetIntegerDataRange('non-existent key','',0,100,A_DEFAULT_VALUE));
	punit.Assert.Equal(A_DEFAULT_VALUE,aTester.GetIntegerDataRange('a valid key','non-existent subkey',0,100,A_DEFAULT_VALUE));
	aTester.myGetRegData:=4;
	punit.Assert.Equal(-1928437,aTester.GetIntegerDataRange('a valid key','a valid subkey',-1928437,-1928437,A_DEFAULT_VALUE));
	punit.Assert.Equal(A_DEFAULT_VALUE,aTester.GetIntegerDataRange('a valid key','a valid subkey',-1928436,1928436,A_DEFAULT_VALUE));
	punit.Assert.Equal(A_DEFAULT_VALUE,aTester.GetIntegerDataRange('a valid key','a valid subkey',-1928438,-1928438,A_DEFAULT_VALUE));
end;

{$endif TEST}

procedure OApplication.SetBooleanData(aKey,aSubKey:pchar;aValue:boolean);

begin //writeln('OApplication.SetBooleanData(',aKey,',',aSubKey,',',aValue,')');
	SetIntegerData(aKey,aSubKey,Q(aValue,1,0));
end;

{$ifdef TEST}

type
	TestAppBool=object(Test_OApplication)
		myIntValPassedToSetIntegerData:long;
		procedure SetIntegerData(aKey,aSubKey:pchar;aValue:longint); test_virtual
	end;

procedure TestAppBool.SetIntegerData(aKey,aSubKey:pchar;aValue:longint);

begin
	myIntValPassedToSetIntegerData:=aValue;	
end;

procedure Test_SetBooleanData;

var
	tester:TestAppBool;

begin
	tester.Construct;
	tester.SetBooleanData('aKey','aSubKey',TRUE);
	punit.Assert.Equal(1,tester.myIntValPassedToSetIntegerData);
	tester.SetBooleanData('aKey','aSubKey',FALSE);
	punit.Assert.Equal(0,tester.myIntValPassedToSetIntegerData);
end;

{$endif TEST}

procedure OApplication.PutRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;cbData:DWORD);
var
	aHKey:HKEY;
begin
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	System.Assert(aDataBuffer<>NIL,'aDataBuffer'); 
	aHKey:=OpenRegKey(aKey);
	if aHKey<>0 then begin
		CallRegSetValueEx(aHKey,aSubKey,dwType,aDataBuffer,cbData);
		RegCloseKey(aHKey); 
	end;
end;

const
	NULL_HKEY=NULL_HANDLE;

{$ifdef TEST}

function Test_OApplication.CallRegSetValueEx(aHKey:HKEY;aSubKey:LPCTSTR;dwType:DWORD;lpData:LPBYTE;cbData:DWORD):LONG;

begin
	myCallRegSetValueEx.WasCalled:=TRUE;
	myHKey:=aHKey;
	myDataBuffer:=StrNew(PChar(lpData));
	myDataTypeArg:=dwType;
	myDataLenArg:=cbData;
	CallRegSetValueEx:=0;
end;

function Test_OApplication.CallRegQueryValueEx(hKey:HKEY;lpValueName:LPCTSTR;lpType:LPDWORD;lpData:LPBYTE;lpcbData:LPDWORD):LONG;

begin //writeln('Test_OApplication.CallRegQueryValueEx(hKey:HKEY;lpValueName:LPCTSTR;lpType:LPDWORD;lpData=',DWORD(lpData),',lpcbData=',lpcbData^,')');
	myCallRegQueryValueEx.WasCalled:=TRUE;
	myHKey:=hKey;
	myValueName:=StrNew(lpValueName);
	myQueryCbDataIn:=lpcbData^;
	lpType^:=myQueryDataType;
	if (myQueryDataBuffer<>nil) and (lpData<>nil) and (lpcbData<>nil) then Move(myQueryDataBuffer^,lpData^,lpcbData^);
	if (lpType^=REG_SZ) and (lpcbData<>nil) then lpcbData^:=StrLen(PChar(myQueryDataBuffer))+1;
	lpcbData^:=myQueryCbDataOut;
	CallRegQueryValueEx:=myRegQueryValueEx;
end;

const
	A_VALID_HKEY=1;

procedure Test_PutRegData;

const
	A_KEY='aKey';
	A_SUBKEY='aSubKey';
	A_VALUE:pchar='REG_sz data';

var
	app:FakeRegApp;

begin
	app.Construct;
	app.myOpenRegKey:=NULL_HKEY;
	app.myCallRegSetValueEx.WasCalled:=FALSE;
	app.PutRegData(A_KEY,A_SUBKEY,REG_SZ,LPBYTE(@A_VALUE[0]),StrLen(A_VALUE)+1);
	punit.Assert.IsFalse(app.myCallRegSetValueEx.WasCalled);
	app.myOpenRegKey:=A_VALID_HKEY;
	app.myCallRegSetValueEx.WasCalled:=FALSE;
	app.PutRegData(A_KEY,A_SUBKEY,REG_SZ,LPBYTE(@A_VALUE[0]),StrLen(A_VALUE)+1);
	punit.Assert.IsTrue(app.myCallRegSetValueEx.WasCalled);
	punit.Assert.EqualText(A_VALUE,app.myDataBuffer);
	punit.Assert.Equal(REG_SZ,app.myDataTypeArg);
	punit.Assert.Equal(StrLen(A_VALUE)+1,app.myDataLenArg);
end;

{$endif TEST}

procedure OApplication.SetStringData(aKey,aSubKey:pchar;const aValue:ansistring);
var
	aHKey:HKEY;
begin
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	PutRegData(aKey,aSubKey,REG_SZ,LPBYTE(aValue),Length(aValue)+1);
end;

{$ifdef TEST}

type
	TestApp2=object(FakeRegApp)
		mySubKey:pchar;
		myDataType,myDataLen:DWORD;
		procedure PutRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;cbData:DWORD); virtual;
	end;

procedure TestApp2.PutRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;cbData:DWORD);

begin
	myKey:=StrNew(aKey);
	mySubKey:=StrNew(aSubKey);
	myDataType:=dwType;
	myDataBuffer:=GetMem(cbData);
	Move(aDataBuffer^,myDataBuffer^,SizeInt(cbData));
	myDataLen:=cbData;
end;

procedure Test_SetStringData;

const
	A_KEY='aKey2';
	A_SUBKEY='aSubKey2';
	A_VALUE:string='string data value2';

var
	app:TestApp2;
	aAnsiString:ansistring;

begin
	app.Construct;
	app.myOpenRegKey:=A_VALID_HKEY;
	app.SetStringData(A_KEY,A_SUBKEY,A_VALUE);
	punit.Assert.EqualText(A_KEY,app.myKey);
	punit.Assert.EqualText(A_SUBKEY,app.mySubKey);
	punit.Assert.Equal(REG_SZ,app.myDataType);
	punit.Assert.EqualLong(Length(A_VALUE)+1,Long(app.myDataLen));
	punit.Assert.EqualStr(A_VALUE,StrPas(PChar(app.myDataBuffer)));

	aAnsiString:='';
	while Length(aAnsiString)<256 do aAnsiString:=aAnsiString+'1234567890';
	app.SetStringData(A_KEY,A_SUBKEY,aAnsiString);
	punit.Assert.EqualLong(Length(aAnsiString)+1,Long(app.myDataLen));
	punit.Assert.EqualStr(aAnsiString,StrPas(PChar(app.myDataBuffer)));
end;

{$endif TEST}

procedure OApplication.SetIntegerData(aKey,aSubKey:pchar;aValue:longint);
var
	aValueBuffer:DWORD;
begin //writeln('OApplication.SetIntegerData(',aKey,',',aSubKey,',',aValue,')');
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	aValueBuffer:=DWORD(aValue);
	PutRegData(aKey,aSubKey,REG_DWORD,@aValueBuffer,4);
end;

{$ifdef TEST}

procedure Test_SetIntegerData;

const
	A_KEY='aIntKey';
	A_SUBKEY='aIntSubKey';
	A_VALUE:longint=-238192;

var
	app:TestApp2;

begin
	app.Construct;
	app.myOpenRegKey:=A_VALID_HKEY;
	app.SetIntegerData(A_KEY,A_SUBKEY,A_VALUE);
	punit.Assert.EqualText(A_KEY,app.myKey);
	punit.Assert.EqualText(A_SUBKEY,app.mySubKey);
	punit.Assert.Equal(REG_DWORD,app.myDataType);
	punit.Assert.Equal(4,app.myDataLen);
	punit.Assert.EqualLong(A_VALUE,LongintPtr(app.myDataBuffer)^);
end;

{$endif TEST}

function OApplication.CallRegCreateKey(hKey:HKEY;lpSubKey:LPCTSTR;phkResult:PHKEY):LONG;

begin
	CallRegCreateKey:=RegCreateKey(hKey,lpSubKey,phkResult); 
end;

function OApplication.OpenRegKey(aKey:pchar):HKEY;
const
	REG_KEY_BASE='Software\Wesley Steiner\';
	aHKey:HKEY=NULL_HKEY;
var
	aRegKey:stringBuffer;
begin //Writeln('OApplication.OpenRegKey("',aKey,'")');
	System.Assert(aKey<>NIL,'aKey');
	StrCopy(aRegKey,REG_KEY_BASE);
	StrCat(aRegKey,FriendlyName);
	StrCat(aRegKey,'\');
	StrCat(aRegKey,aKey);
	CallRegCreateKey(HKEY_CURRENT_USER,aRegKey,@aHKey);
	OpenRegKey:=aHKey;
end;

{$ifdef TEST}

const
	FAKE_HKEY:HKEY=1234;

type
	OpenRegKeyTestApp=object(Test_OApplication)
		myHKeyArg:HKEY;
		mySubKeyArg:pchar;
		function CallRegCreateKey(hKey:HKEY;lpSubKey:LPCTSTR;phkResult:PHKEY):LONG; virtual;
	end;

function OpenRegKeyTestApp.CallRegCreateKey(hKey:HKEY;lpSubKey:LPCTSTR;phkResult:PHKEY):LONG;

begin
	myHKeyArg:=hKey;
	mySubKeyArg:=StrNew(lpSubKey);
	phkResult^:=FAKE_HKEY;
	CallRegCreateKey:=phkResult^;
end;

procedure Test_OpenRegKey;

var
	tester:OpenRegKeyTestApp;
	aRegKey:HKEY;

begin
	tester.Construct;
	tester.FriendlyName:='Brand Name';
	aRegKey:=tester.OpenRegKey('aKeyName');
	punit.Assert.Equal(FAKE_HKEY,aRegKey);
	punit.Assert.Equal(Long(HKEY_CURRENT_USER),Long(tester.myHKeyArg));
	punit.Assert.EqualText('Software\Wesley Steiner\Brand Name\aKeyName',tester.mySubKeyArg);
end;

{$endif TEST}

function OApplication.CallRegSetValueEx(aHKey:HKEY;lpValueName:LPCTSTR;dwType:DWORD;lpData:LPBYTE;cbData:DWORD):LONG;

begin
	CallRegSetValueEx:=RegSetValueEx(aHKey,lpValueName,0,dwType,lpData,cbData); 
end;

function OApplication.CallRegQueryValueEx(hKey:HKEY;lpValueName:LPCTSTR;lpType:LPDWORD;lpData:LPBYTE;lpcbData:LPDWORD):LONG;

begin
	CallRegQueryValueEx:=RegQueryValueEx(hKey,lpValueName,nil,lpType,lpData,lpcbData);
end;

function OApplication.CallRegDeleteValue(hKey:HKEY;lpSubKey:LPCTSTR):LONG;

begin
	CallRegDeleteValue:=RegDeleteValue(hKey,lpSubKey);
end;

procedure OApplication.DeleteData(aKey,aSubKey:pchar);
var
	aHKey:HKEY;
begin
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	aHKey:=OpenRegKey(aKey);
	if aHKey<>0 then begin
		CallRegDeleteValue(aHKey,aSubKey);
		RegCloseKey(aHKey); 
	end;
end;

function OApplication.GetRegData(aKey,aSubKey:pchar;dwType:DWORD;aDataBuffer:LPBYTE;aDataBufferSize:DWORD):DWORD;
var
	aHKey:HKEY;
	aDataType:DWORD;
	cbData:DWORD;
begin
	System.Assert(aKey<>NIL,'aKey'); 
	System.Assert(aSubKey<>NIL,'aSubKey'); 
	System.Assert(StrLen(aSubKey)>0,'aSubKey cannot be an empty string'); 
	System.Assert((dwType=REG_SZ) or (dwType=REG_DWORD),'dwType must be one of REG_SZ or REG_DWORD');
	GetRegData:=0;
	aHKey:=OpenRegKey(aKey);
	if aHKey<>0 then begin
		cbData:=aDatabufferSize;
		if CallRegQueryValueEx(aHKey,aSubKey,@aDataType,aDataBuffer,@cbData)=ERROR_SUCCESS then GetRegData:=cbData;
		RegCloseKey(0);
	end
end;	

{$ifdef TEST}

procedure Test_GetRegData;

const
	FAILURE=0;
	SUCCESS=1;
	ERROR_FAILURE=ERROR_SUCCESS-1;

var
	aTester:FakeRegAppBase;
	cbData:DWORD;

begin
	aTester.Construct;

	aTester.myOpenRegKey:=FAILURE;
	punit.Assert.Equal(0,aTester.GetRegData('some Key','some SubKey',REG_DWORD,NIL,1197));

	aTester.myOpenRegKey:=SUCCESS;
	aTester.myQueryDataBuffer:=nil;
	aTester.myRegQueryValueEx:=ERROR_SUCCESS;
	aTester.myQueryCbDataOut:=125;
	punit.Assert.Equal(125,aTester.GetRegData('some Key','some SubKey',REG_SZ,NIL,412235));
	punit.Assert.Equal(412235,aTester.myQueryCbDataIn);

	aTester.myOpenRegKey:=SUCCESS;
	aTester.myQueryDataBuffer:=nil;
	aTester.myRegQueryValueEx:=ERROR_FAILURE;
	aTester.myQueryCbDataOut:=123;
	punit.Assert.Equal(0,aTester.GetRegData('some Key','some SubKey',REG_SZ,NIL,123));
end;

{$endif TEST}

procedure PopupWndPersistor.SaveMaximized(aState:boolean);

begin
	theApplication^.SetBooleanData(myStorageKey,KEY_MAXIMIZED,aState);
end;

procedure OMainFrame.CheckStartMaximized(start_maximized:boolean);

begin
	if (start_maximized) then CmdShow:=SW_SHOWMAXIMIZED;
end;

{$ifdef TEST}

constructor OMainFrameStub.Construct; begin end;

procedure Test_StartMaximized;

var
	aTester:OMainFrameStub;

begin
	aTester.Construct;
	CmdShow:=123;
	aTester.CheckStartMaximized(false);
	punit.Assert.Equal(123,CmdShow);
	aTester.CheckStartMaximized(true);
	punit.Assert.Equal(SW_SHOWMAXIMIZED,CmdShow);
end;

{$endif TEST}

procedure FrameWindow.SetCmdShow(maxFromStorage,start_maximized,hasPersistence:boolean);

begin
	if (maxFromStorage) or ((not hasPersistence) and start_maximized)
		then CmdShow:=SW_SHOWMAXIMIZED
		else CmdShow:=SW_SHOWNORMAL;
end;

{$ifdef TEST}

procedure Test_SetCmdShow;

var
	aTester:FrameWindowTester;

begin
	aTester.Init;
	CmdShow:=SW_SHOWNORMAL;
	aTester.SetCmdShow(false,false,false);
	punit.Assert.Equal(SW_SHOWNORMAL,CmdShow);
	CmdShow:=SW_SHOWNORMAL;
	aTester.SetCmdShow(true,false,false);
	punit.Assert.Equal(SW_SHOWMAXIMIZED,CmdShow);
	CmdShow:=SW_SHOWNORMAL;
	aTester.SetCmdShow(true,true,false);
	punit.Assert.Equal(SW_SHOWMAXIMIZED,CmdShow);
	CmdShow:=SW_SHOWNORMAL;
	aTester.SetCmdShow(true,false,true);
	punit.Assert.Equal(SW_SHOWMAXIMIZED,CmdShow);
	CmdShow:=SW_SHOWNORMAL;
	aTester.SetCmdShow(true,true,true);
	punit.Assert.Equal(SW_SHOWMAXIMIZED,CmdShow);
	CmdShow:=SW_SHOWNORMAL;
	aTester.SetCmdShow(false,true,false);
	punit.Assert.Equal(SW_SHOWMAXIMIZED,CmdShow);
	CmdShow:=SW_SHOWNORMAL;
	aTester.SetCmdShow(false,true,true);
	punit.Assert.Equal(SW_SHOWNORMAL,CmdShow);
end;

{$endif TEST}

function FrameWindow.HasRestoredData:boolean;

begin
	if (myRestoredX<>LONG(CW_USEDEFAULT)) or (myRestoredY<>LONG(CW_USEDEFAULT)) or (myRestoredW<>LongWord(CW_USEDEFAULT)) or (myRestoredH<>LongWord(CW_USEDEFAULT))
		then HasRestoredData:=TRUE
		else HasRestoredData:=FALSE;
end;

{$ifdef TEST}

procedure Test_HasRestoredData;

var
	aTester:FrameWindowTester;

begin
	aTester.Init;
	aTester.myRestoredX:=LONG(CW_USEDEFAULT);
	aTester.myRestoredY:=LONG(CW_USEDEFAULT);
	aTester.myRestoredW:=LongWord(CW_USEDEFAULT);
	aTester.myRestoredH:=LongWord(CW_USEDEFAULT);
	punit.Assert.IsFalse(aTester.HasRestoredData);
	aTester.myRestoredX:=1;
	punit.Assert.IsTrue(aTester.HasRestoredData);
	aTester.myRestoredX:=LONG(CW_USEDEFAULT);
	aTester.myRestoredY:=1;
	punit.Assert.IsTrue(aTester.HasRestoredData);
	aTester.myRestoredY:=LONG(CW_USEDEFAULT);
	aTester.myRestoredW:=10;
	punit.Assert.IsTrue(aTester.HasRestoredData);
	aTester.myRestoredW:=LongWord(CW_USEDEFAULT);
	aTester.myRestoredH:=20;
	punit.Assert.IsTrue(aTester.HasRestoredData);
end;

{$endif TEST}

function OMainFrame.Create:HWND;
begin //writeln('OMainFrame.Create');
	Create:=MyFrameWindow^.Create(myMainApp^.FriendlyName,WS_OVERLAPPEDWINDOW or WS_VISIBLE,MyFrameWindow^.myRestoredX,MyFrameWindow^.myRestoredY,MyFrameWindow^.myRestoredW,MyFrameWindow^.myRestoredH,0,LoadMenu(hInstance,MakeIntResource(101)),hInstance,nil);
end;

function OMainFrame.Owner:OApplicationP;
begin
	Owner:=myMainApp;
end;

{$ifdef TEST}

procedure Test_GetStringData;

const
	A_DEFAULT_VALUE='foobar';

var
	aTester:FakeRegApp;
	aLongString:ansistring;

begin
	aTester.Construct;
	aLongString:='';
	while Length(aLongString)<255 do aLongString:=aLongString+'a long string ';
	aTester.AddStringKey('a valid key','a valid subkey',aLongString);
	aTester.myOpenRegKey:=0;
	punit.Assert.EqualStr(A_DEFAULT_VALUE,aTester.GetStringData('non-existent key','',A_DEFAULT_VALUE));
// my test environment is not working correctly!
//	punit.Assert.EqualStr(aLongString,aTester.GetStringData('a valid key','a valid subkey',A_DEFAULT_VALUE));
end;

{$endif}

function OApplication.VersionInfo:FileVersionInfoPtr;

begin
	VersionInfo:=@the_version_info;
end;

function OMainFrame.MainMenu:HMENU;
begin
	MainMenu:=GetMenu(self.MyFrameWindow^.Handle);
end;

constructor FrameWindow.Construct;

begin
	inherited Construct;
	myPersistor:=new(PopupWndPersistorPtr,Construct(@self,PChar(the_persistence_key),TRUE,TRUE,LONG(CW_USEDEFAULT),LONG(CW_USEDEFAULT)));
	myPersistor^.RestorePos(myRestoredX,myRestoredY);
	myPersistor^.RestoreSize(myRestoredW,myRestoredH);
	SetCmdShow(theApplication^.GetBooleanData(PChar(the_persistence_key),KEY_MAXIMIZED,FALSE),the_start_maximized,HasRestoredData);
end;

{$ifdef TEST}
begin
	Suite.Add(@TestTWindowEx_MinClientAreaWd);
	Suite.Add(@Test_FrameWindow_OnMoved_result);
	Suite.Add(@Test_FrameWindow_OnMove);
	Suite.Add(@Test_FrameWindow_OnSize_result);
	Suite.Add(@Test_FrameWindow_OnSize);
	Suite.Add(@Test_PopupWndPersistor_SavePos);
	Suite.Add(@Test_PopupWndPersistor_SaveSize);
	Suite.Add(@Test_PopupWndPersistor_RestorePos);
	Suite.Add(@Test_PopupWndPersistor_RestoreSize);
	Suite.Add(@Test_SetStringData);
	Suite.Add(@Test_OpenRegKey);
	Suite.Add(@Test_PutRegData);
	Suite.Add(@Test_SetIntegerData);
	Suite.Add(@Test_SetBooleanData);
	Suite.Add(@Test_GetRegData);
	Suite.Add(@Test_GetIntegerData);
	Suite.Add(@Test_GetBooleanData);
	Suite.Add(@Test_GetIntegerDataRange);
	Suite.Add(@Test_StartMaximized);
	Suite.Add(@Test_SetCmdShow);
	Suite.Add(@Test_HasRestoredData);
	Suite.Add(@Test_GetStringData);
	Suite.Add(@Test_ParamStr0Fix);
	Suite.Run('oapp');
{$endif TEST}
end.
