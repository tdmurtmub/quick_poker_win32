{ (C) 2006 Wesley Steiner }

{$MODE FPC}

{$I platform}

unit owindows;

interface

uses
	windows;

const
	OWINDOW_CLASS='com.wesleysteiner.owindow.class';
	WM_MESSAGE_PROCESSED=0;
	WM_MESSAGE_NOT_PROCESSED=1;

type
	int=LONG;

	OHANDLE=object
		Handle:HANDLE;
	end;

	OWND=object(OHANDLE)
		function ClientHeight:UINT;
		function ClientRect:RECT;
		function ClientWidth:UINT;
		function EnableWindow(bEnabled:BOOL):BOOL;
		function GetParent:HWND;
		function GetClientRect(var r:RECT):BOOL;
		function Height:WORD; {$ifdef TEST} virtual; {$endif}
		function IsValid:boolean;
		function PostMessage(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):BOOL;
		function SetWindowText(const aText:pchar):BOOL;
		function SendMessage(aMsg:UINT):LRESULT;
		function SendMessage(aMsg:UINT;wParam:WPARAM):LRESULT;
		function SendMessage(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LRESULT;
		function ShowWindow(nCmdShow:int):BOOL;
		function UpdateWindow:BOOL;
		function Width:WORD; {$ifdef TEST} virtual; {$endif}
		function WindowStyle:LONG;
		function ClientAreaHeight:UINT; // OBSOLETE: use ClientHeight
		function ClientAreaWidth:UINT; // OBSOLETE: use ClientWidth
	end;

	PWindow=^OWindow;
	OWindow=object(OWND)
		constructor Construct;
		function ClassBackgroundBrush:HBRUSH; virtual;
		function Create(lpWindowName:LPCTSTR;dwStyle:DWORD;x,y,nWidth,nHeight:integer;hWndParent:HWND;hMenu:HMENU;hInstance:HINST;lpParam:LPVOID):HWND;
		function OnCmd(aCmdId:UINT):LONG; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		function OnCreate:LONG; virtual;
		function OnEraseBkGnd(aDC:HDC):LONG; virtual;
		function OnLButtonDown(keys:UINT;x,y:integer):LONG; virtual;
		function OnLButtonUp(keys:UINT;x,y:integer):LONG; virtual;
		function OnRButtonDown(keys:UINT;x,y:integer):LONG; virtual;
		function OnMouseMove(keys:UINT;x,y:integer):LONG; virtual;
		function OnMove(newX,newY:integer):LONG; virtual;
		function OnNotify(aCmdId:UINT;info:PNMHDR):LONG; virtual;
		function OnPaint:LONG; virtual;
		function OnSize(resizeType:UINT;newWidth,newHeight:integer):LONG; virtual;
		function ClassName:LPCTSTR; virtual;
		procedure Paint(aPaintDC:HDC;var PaintInfo:TPaintStruct); virtual;
	private
		function RegisterWindowClass:TAtom;
	end;

type
	FileVersionInfoPtr=^FileVersionInfo;
	FileVersionInfo=object
		constructor Construct(const aPath:pchar);
		destructor Destruct;
		function ProductName:string;
		function ProductVersion:string;
		function LegalCopyright:string;
	private
		mySize:DWORD;
		myData:pointer;
		function ExtractStringInfo(info:string):string;
	end;

function XWND(h:HWND):OWND;

implementation

uses
	{$ifdef TEST} punit, {$endif}
	sdkex,windowsx;

constructor OWindow.Construct;

begin 	
	handle:=0;
end;

//var
//	the_obj:pointer;
	
function OWindowProc(aHWnd:HWND;aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LRESULT; stdcall;

var
	rc:LRESULT;
	obj:PWindow;

begin
	//if aMsg=WM_CREATE then SetWindowLong(aHWnd,0,LONG(the_obj));
	obj:=PWindow(GetWindowLong(aHWnd,0));
	if obj<>nil
		then rc:=obj^.OnMsg(aMsg,wParam,lParam)
		else rc:=0;
	if rc<>0
		then OWindowProc:=rc
		else OWindowProc:=DefWindowProc(aHWnd,aMsg,wParam,lParam);
end;

function OWindow.Create(lpWindowName:LPCTSTR;dwStyle:DWORD;x,y,nWidth,nHeight:integer;hWndParent:HWND;hMenu:HMENU;hInstance:HINST;lpParam:LPVOID):HWND;

begin //writeln('OWindow.Create(lpWindowName:LPCTSTR;dwStyle:DWORD;x,y,nWidth,nHeight:integer;hWndParent:HWND;hMenu:HMENU;hInstance:HINST;lpParam:LPVOID)');
//	the_obj:=@self;
	RegisterWindowClass;
	handle:=ApiCheck(CreateWindow(ClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam));
	SetWindowLong(handle,0,LONG(@self));
	Create:=handle;
end;

function OWindow.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin //writeln('OWindow.OnMsg(',aMsg,',',wParam,',',lParam,')');
	case aMsg of
		WM_COMMAND:if (HIWORD(wParam)=0) or (HIWORD(wParam)=1) then OnMsg:=OnCmd(LOWORD(wParam)) else OnMsg:=0;
		WM_PAINT:OnMsg:=OnPaint;
		WM_CREATE:OnMsg:=OnCreate;
		WM_ERASEBKGND:OnMsg:=OnEraseBkGnd(HDC(wParam));
		WM_LBUTTONDOWN:OnMsg:=OnLButtonDown(wParam,LOWORD(lParam),HIWORD(lParam));
		WM_LBUTTONUP:OnMsg:=OnLButtonUp(wParam,Integer(LOWORD(lParam)),Integer(HIWORD(lParam)));
		WM_SIZE:OnMsg:=OnSize(wParam,LOWORD(lParam),HIWORD(lParam));
		WM_MOVE:OnMsg:=OnMove(Integer(LOWORD(lParam)),Integer(HIWORD(lParam)));
		WM_MOUSEMOVE:OnMsg:=OnMouseMove(UINT(wParam),Integer(LOWORD(lParam)),Integer(HIWORD(lParam)));
		WM_NOTIFY:OnMsg:=OnNotify(UINT(wParam),PNMHDR(lParam));
		WM_RBUTTONDOWN:OnMsg:=OnRButtonDown(wParam,LOWORD(lParam),HIWORD(lParam));
	else
		OnMsg:=0;
	end;
end;

{$ifdef TEST}

type
	TestOWindow=object(OWindow)
		myKeys:UINT;
		myX,myY,myWidth,myHeight:integer;
		OnEraseBkGnd_was_called:boolean;
		OnPaint_was_called:boolean;
		OnMouseMove_was_called:boolean;
		OnSize_was_called:boolean;
		OnSize_type:UINT;	
		OnMove_was_called:boolean;
		OnCmd_was_called:boolean;
		OnLbuttonDown_was_called:boolean;
		OnLbuttonUp_was_called:boolean;
		OnRbuttonDown_was_called:boolean;
		OnCreate_was_called:boolean;
		myParam:UINT;
		function OnEraseBkGnd(aDC:HDC):LONG; virtual;
		function OnLButtonDown(keys:UINT;x,y:integer):LONG; virtual;
		function OnLButtonUp(keys:UINT;x,y:integer):LONG; virtual;
		function OnRButtonDown(keys:UINT;x,y:integer):LONG; virtual;
		function OnMouseMove(keys:UINT;x,y:integer):LONG; virtual;
		function OnPaint:LONG; virtual;
		function OnCreate:LONG; virtual;
		function OnSize(resizeType:UINT;newWidth,newHeight:integer):LONG; virtual;
		function OnMove(newX,newY:integer):LONG; virtual;
		function OnCmd(aCmdId:UINT):LONG; virtual;
	end;

function TestOWindow.OnLButtonUp(keys:UINT;x,y:integer):LONG;

begin
	OnLbuttonUp_was_called:=true;
	myKeys:=keys;
	myX:=x;
	myY:=y;
	OnLButtonUp:=0;
end;

function TestOWindow.OnRButtonDown(keys:UINT;x,y:integer):LONG;

begin
	OnRbuttonDown_was_called:=true;
	myKeys:=keys;
	myX:=x;
	myY:=y;
	OnRButtonDown:=0;
end;

function TestOWindow.OnLButtonDown(keys:UINT;x,y:integer):LONG;
begin
	OnLbuttonDown_was_called:=true;
	myKeys:=keys;
	myX:=x;
	myY:=y;
	OnLButtonDown:=0;
end;

function TestOWindow.OnSize(resizeType:UINT;newWidth,newHeight:integer):LONG;

begin
	OnSize_was_called:=true;
	OnSize_type:=resizeType;
	myWidth:=newWidth;
	myHeight:=newHeight;
	OnSize:=0;
end;

function TestOWindow.OnCmd(aCmdId:UINT):LONG;

begin
	OnCmd_was_called:=true;
	myParam:=aCmdId;
	OnCmd:=0;
end;

function TestOWindow.OnMove(newX,newY:integer):LONG;

begin
	OnMove_was_called:=true;
	myX:=newX;
	myY:=newY;
	OnMove:=0;
end;

function TestOWindow.OnMouseMove(keys:UINT;x,y:integer):LONG;

begin
	OnMouseMove_was_called:=true;
	myKeys:=keys;
	myX:=x;
	myY:=y;
	OnMouseMove:=0;
end;

function TestOWindow.OnEraseBkGnd(aDC:HDC):LONG;

begin
	OnEraseBkGnd_was_called:=true;
	OnEraseBkGnd:=0;
end;

procedure Test_OnMsg;

const
	NON_HANDLED_MSG=99999;

var
	tester:TestOWindow;

begin
	tester.Construct;
	punit.Assert.Equal(0,tester.OnMsg(NON_HANDLED_MSG,0,0));
end;

procedure Test_OnMsg_WM_ERASEBKGND;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnEraseBkGnd_was_called:=false;
	tester.OnMsg(WM_ERASEBKGND,2,3);
	punit.Assert.IsTrue(tester.OnEraseBkGnd_was_called);
end;

function TestOWindow.OnCreate:LONG;

begin
	OnCreate_was_called:=TRUE;
	OnCreate:=0;
end;

function TestOWindow.OnPaint:LONG;

begin
	OnPaint_was_called:=true;
	OnPaint:=0;
end;

procedure Test_OnMsg_WM_PAINT;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnPaint_was_called:=false;
	tester.OnMsg(WM_PAINT,2,3);
	punit.Assert.IsTrue(tester.OnPaint_was_called);
end;

procedure Test_OnMsg_WM_MOUSEMOVE;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnMouseMove_was_called:=false;
	tester.myKeys:=0;
	tester.myX:=0;
	tester.myY:=0;
	tester.OnMsg(WM_MOUSEMOVE,22,$000e000f);
	punit.Assert.IsTrue(tester.OnMouseMove_was_called);
	punit.Assert.Equal(22,tester.myKeys);
	punit.Assert.Equal(15,tester.myX);
	punit.Assert.Equal(14,tester.myY);
end;

procedure Test_OnMsg_WM_LBUTTONDOWN;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnLbuttonDown_was_called:=false;
	tester.myKeys:=0;
	tester.myX:=0;
	tester.myY:=0;
	tester.OnMsg(WM_LBUTTONDOWN,456,$000d000c);
	punit.Assert.IsTrue(tester.OnLbuttonDown_was_called);
	punit.Assert.Equal(456,tester.myKeys);
	punit.Assert.Equal(12,tester.myX);
	punit.Assert.Equal(13,tester.myY);
end;

procedure Test_OnMsg_WM_RBUTTONDOWN;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnRbuttonDown_was_called:=false;
	tester.myKeys:=0;
	tester.myX:=0;
	tester.myY:=0;
	tester.OnMsg(WM_RBUTTONDOWN,458,$00080007);
	punit.Assert.IsTrue(tester.OnRbuttonDown_was_called);
	punit.Assert.Equal(458,tester.myKeys);
	punit.Assert.Equal(7,tester.myX);
	punit.Assert.Equal(8,tester.myY);
end;

procedure Test_OnMsg_WM_LBUTTONUP;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnLbuttonUp_was_called:=false;
	tester.myKeys:=0;
	tester.myX:=0;
	tester.myY:=0;
	tester.OnMsg(WM_LBUTTONUP,457,$000a000b);
	punit.Assert.IsTrue(tester.OnLbuttonUp_was_called);
	punit.Assert.Equal(457,tester.myKeys);
	punit.Assert.Equal(11,tester.myX);
	punit.Assert.Equal(10,tester.myY);
end;

procedure Test_OnMsg_WM_SIZE;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnSize_was_called:=false;
	tester.OnSize_type:=0;
	tester.myWidth:=0;
	tester.myHeight:=0;
	tester.OnMsg(WM_SIZE,77,$00ff00fe);
	punit.Assert.IsTrue(tester.OnSize_was_called);
	punit.Assert.Equal(77,tester.OnSize_type);
	punit.Assert.Equal(254,tester.myWidth);
	punit.Assert.Equal(255,tester.myHeight);
end;

procedure Test_OnMsg_WM_CREATE;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnCreate_was_called:=false;
	tester.OnMsg(WM_CREATE,0,0);
	punit.Assert.IsTrue(tester.OnCreate_was_called);
end;

procedure Test_OnMsg_WM_MOVE;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnMove_was_called:=false;
	tester.myX:=0;
	tester.myY:=0;
	tester.OnMsg(WM_MOVE,0,$00C900C8);
	punit.Assert.IsTrue(tester.OnMove_was_called);
	punit.Assert.Equal(200,tester.myX);
	punit.Assert.Equal(201,tester.myY);
end;

procedure Test_OnMsg_WM_COMMAND_from_control;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnCmd_was_called:=false;
	punit.Assert.EqualLong(0,tester.OnMsg(WM_COMMAND,WPARAM($12348001),456));
	punit.Assert.IsFalse(tester.OnCmd_was_called);
end;

procedure Test_OnMsg_WM_COMMAND_from_menu;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnCmd_was_called:=false;
	tester.OnMsg(WM_COMMAND,WPARAM($00008002),456);
	punit.Assert.IsTrue(tester.OnCmd_was_called);
	punit.Assert.Equal(Integer($8002),Integer(tester.myParam));
end;

procedure Test_OnMsg_WM_COMMAND_from_accelerator;

var
	tester:TestOWindow;

begin
	tester.Construct;
	tester.OnCmd_was_called:=false;
	tester.OnMsg(WM_COMMAND,WPARAM($00018032),789);
	punit.Assert.IsTrue(tester.OnCmd_was_called);
	punit.Assert.Equal(Integer($8032),Integer(tester.myParam));
end;

procedure Test_OnSize;

var
	tester:OWindow;

begin
	tester.Construct;
	punit.Assert.IsFalse(tester.OnSize(1,2,3)=0);
end;

procedure Test_OnMove;

var
	tester:OWindow;

begin
	tester.Construct;
	punit.Assert.IsFalse(tester.OnMove(2,3)=0);
end;

procedure Test_OnCmd;

var
	tester:OWindow;

begin
	tester.Construct;
	punit.Assert.IsFalse(tester.OnCmd(3)=0);
end;

procedure Test_OnLButtonDown;

var
	tester:OWindow;

begin
	tester.Construct;
	punit.Assert.IsFalse(tester.OnLButtonDown(1,2,3)=0);
end;

procedure Test_OnRButtonDown;

var
	tester:OWindow;

begin
	tester.Construct;
	punit.Assert.IsFalse(tester.OnRButtonDown(1,2,3)=0);
end;

procedure Test_OnLButtonUp;

var
	tester:OWindow;

begin
	tester.Construct;
	punit.Assert.IsFalse(tester.OnLButtonUp(1,2,3)=0);
end;

{$endif TEST}

function OWindow.OnEraseBkGnd(aDC:HDC):LONG;

begin
	OnEraseBkGnd:=0;
end;

function OWindow.OnMouseMove(keys:UINT;x,y:integer):LONG;

begin
	OnMouseMove:=1;
end;

function OWindow.OnPaint:LONG;

var
	aDC:HDC;
	aPaintInfo:TPaintStruct;

begin
	aDC:=BeginPaint(handle,aPaintInfo);
	Paint(aDC,aPaintInfo);
	EndPaint(handle,aPaintInfo);
	OnPaint:=1;
end;

procedure OWindow.Paint(aPaintDC:HDC;var PaintInfo:TPaintStruct);
begin
end;

function OWindow.OnCreate:LONG;
begin //writeln('OWindow.OnCreate');
	OnCreate:=0;
end;

function OWindow.OnSize(resizeType:UINT;newWidth,newHeight:integer):LONG;
begin
	OnSize:=1;
end;

function OWindow.OnMove(newX,newY:integer):LONG;
begin
	OnMove:=1;
end;

function OWindow.OnCmd(aCmdId:UINT):LONG;
begin
	OnCmd:=1;
end;

function OWindow.OnLButtonDown(keys:UINT;x,y:integer):LONG;
begin
	OnLButtonDown:=1;	
end;

function OWindow.OnRButtonDown(keys:UINT;x,y:integer):LONG;
begin
	OnRButtonDown:=1;	
end;

function OWindow.OnLButtonUp(keys:UINT;x,y:integer):LONG;
begin
	OnLButtonUp:=1;	
end;

function OWindow.ClassBackgroundBrush:HBRUSH;
begin
	ClassBackgroundBrush:=COLOR_WINDOW+1;
end;

function OWindow.ClassName:LPCTSTR;
begin
	ClassName:=OWINDOW_CLASS;
end;

function OWindow.RegisterWindowClass:TAtom;
var
    wc:TWNDCLASS;
begin
    wc.style:=CS_HREDRAW or CS_VREDRAW or CS_DBLCLKS;
    wc.lpfnWndProc:=@OWindowProc;
    wc.cbClsExtra:=0;
    wc.cbWndExtra:=4;
    wc.hInstance:=hinstance;
    wc.hIcon:=LoadIcon(HInstance, 'MAINICON'); // (see old GetWindowClass function!)
    wc.hCursor:=LoadCursor(0,IDC_ARROW);
    wc.hbrBackground:=ClassBackgroundBrush;
    wc.lpszMenuName:=nil;
    wc.lpszClassName:=ClassName;
    RegisterWindowClass:=RegisterClass(wc);
end;

function OWND.SetWindowText(const aText:pchar):BOOL;
begin
	SetWindowText:=windows.SetWindowText(handle,aText);	
end;

function OWND.SendMessage(aMsg:UINT):LRESULT;
begin
	SendMessage:=SendMessage(aMsg,0);
end;

function OWND.SendMessage(aMsg:UINT;wParam:WPARAM):LRESULT;
begin
	SendMessage:=SendMessage(aMsg,wParam,0);
end;

function OWND.SendMessage(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LRESULT;
begin
	SendMessage:=windows.SendMessage(handle,aMsg,wParam,lParam);
end;

function OWND.ClientWidth:UINT;
begin
	ClientWidth:=GetClientWd(handle);
end;

function OWND.ClientAreaHeight:UINT;
begin
	ClientAreaHeight:=ClientHeight;
end;

function OWND.ClientAreaWidth:UINT;
begin
	ClientAreaWidth:=ClientWidth;
end;

function OWND.ClientHeight:UINT;
begin
	ClientHeight:=GetClientHt(handle);
end;

function OWND.GetParent:HWND;
begin
	GetParent:=windows.GetParent(handle);
end;

function OWND.PostMessage(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):BOOL;
begin
	PostMessage:=windows.PostMessage(handle,aMsg,wParam,lParam);
end;

function OWindow.OnNotify(aCmdId:UINT;info:PNMHDR):LONG;
begin //writeln('OWindow.OnNotify(',aCmdId,',{',info^.idFrom,',',info^.code,'})');
	OnNotify:=0;
end;

function OWND.ShowWindow(nCmdShow:int):BOOL;
begin
	ShowWindow:=windows.ShowWindow(Handle,nCmdShow);
end;

constructor FileVersionInfo.Construct(const aPath:pchar);
var
	aHandle:DWORD;
	
begin //WriteLn('FileVersionInfo.Construct(''',aPath,''')');
	myData:=NIL;
	mySize:=ApiCheck(GetFileVersionInfoSize(aPath,aHandle));
	myData:=GetMem(mySize);
	ApiCheck(LongInt(GetFileVersionInfo(aPath,aHandle,mySize,myData)));
end;

destructor FileVersionInfo.Destruct;
begin
	if myData<>NIL then FreeMem(myData,mySize);
end;

//	VERIFY(VerQueryValue(myData, TEXT("\\"), &buffer, &dwLen) > 0); 
//	VS_FIXEDFILEINFO * pFixedFileInfo = (VS_FIXEDFILEINFO *) buffer;

function FileVersionInfo.ExtractStringInfo(info:string):string;
var
	len:UINT;
	buffer:LPVOID;
begin
	if VerQueryValue(myData,PChar(AnsiString('\StringFileInfo\040904B0\'+info)),@buffer,@len) 
		then ExtractStringInfo:=StrPas(PChar(buffer))
		else ExtractStringInfo:='';
end;

function FileVersionInfo.ProductName:string;
begin
	ProductName:=ExtractStringInfo('ProductName');
end;

function FileVersionInfo.LegalCopyright:string;
begin
	LegalCopyright:=ExtractStringInfo('LegalCopyright');
end;

function FileVersionInfo.ProductVersion:string;

begin
	ProductVersion:=ExtractStringInfo('ProductVersion');
end;

function OWND.GetClientRect(var r:RECT):BOOL;
begin
	GetClientRect:=windows.GetClientRect(handle,r);	
end;

function OWND.ClientRect:RECT;
var
	r:RECT;
begin
	GetClientRect(r);
	ClientRect:=r;
end;

function OWND.EnableWindow(bEnabled:BOOL):BOOL;
begin
	EnableWindow:=windows.EnableWindow(handle,bEnabled)	;
end;

function OWND.UpdateWindow:BOOL;
begin
	UpdateWindow:=windows.UpdateWindow(handle);
end;

function XWND(h:HWND):OWND; 

var
	o:OWND;
	
begin
	o.handle:=h;
	XWND:=o;
end;

function OWND.WindowStyle:LONG;

begin
	WindowStyle:=GetWindowLong(handle,GWL_STYLE);
end;

function OWND.Height:word;
var
	aRect:TRect;
begin
	GetWindowRect(handle,aRect);
	Height:=aRect.bottom-aRect.top;
end;

function OWND.Width:word;
var
	aRect:TRect;
begin
	GetWindowRect(handle,aRect);
	Width:=aRect.right-aRect.left;
end;

function OWND.IsValid:boolean;

begin
	IsValid:=(handle<>NULL_HANDLE);
end;

{$ifdef TEST}
procedure Test_IsValid;

var
	testee:OWindow;
	
begin
	testee.Construct;
	testee.handle:=NULL_HANDLE;
	AssertIsFalse(testee.IsValid);
	testee.handle:=123;
	AssertIsTrue(testee.IsValid);
end;
{$endif}

{$ifdef TEST}
begin
	Suite.Add(@Test_OnSize);
	Suite.Add(@Test_OnLButtonDown);
	Suite.Add(@Test_OnRButtonDown);
	Suite.Add(@Test_OnLButtonUp);
	Suite.Add(@Test_OnMove);
	Suite.Add(@Test_OnCmd);
	Suite.Add(@Test_OnMsg);
	Suite.Add(@Test_OnMsg_WM_ERASEBKGND);
	Suite.Add(@Test_OnMsg_WM_PAINT);
	Suite.Add(@Test_OnMsg_WM_MOUSEMOVE);
	Suite.Add(@Test_OnMsg_WM_SIZE);
	Suite.Add(@Test_OnMsg_WM_LBUTTONDOWN);
	Suite.Add(@Test_OnMsg_WM_LBUTTONUP);
	Suite.Add(@Test_OnMsg_WM_RBUTTONDOWN);
	Suite.Add(@Test_OnMsg_WM_MOVE);
	Suite.Add(@Test_OnMsg_WM_COMMAND_from_control);
	Suite.Add(@Test_OnMsg_WM_COMMAND_from_menu);
	Suite.Add(@Test_OnMsg_WM_COMMAND_from_accelerator);
	Suite.Add(@Test_OnMsg_WM_CREATE);
	Suite.Add(@Test_IsValid);
	Suite.Run('owindows');
{$endif TEST}
end.
