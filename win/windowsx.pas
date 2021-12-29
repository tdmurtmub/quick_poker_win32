{ (C) 2007 Wesley Steiner }

{$MODE FPC}

unit windowsx; { non-object based extensions to the Windows SDK }

interface

uses
	windows;

const
	NULL_HANDLE=0;
	NULL_BITMAP=NULL_HANDLE;

function CreateRect(x,y,w,h:longint):TRect;
function DevFontHt(aDC:hDC):integer;
function DevLineHt(aDC:hDC):integer; { recommended space between lines }
function DumpToString(const r:TRect):string;
function GetHdcTextWidth(a_hdc:HDC; a_pStr:PChar):Word;
function GetMenuItemText(hMenu:HMENU;uItem:UINT;fByPosition:BOOL):string;
function IsValidHandle(aHandle:HANDLE):boolean;
function LoadResString(resId:UINT):string;
function LoadBitmapFromFile(path:pchar):HBITMAP;
function SetMenuItemText(hMenu:HMENU;uItem:UINT;fByPosition:BOOL;const text:string):BOOL;
function SysFontHt:integer; { actual font height }
function SysLineHt:integer; { recommended space between lines }
function XpIntersectRect(var Dest: TRect; Src1,Src2: TRect): bool;

procedure CenterWindow(source,target:HWND);
procedure Delay(milliSeconds:integer);
procedure SetMenuBoolean(Menu:HMENU;Item:UINT;V:boolean);

implementation

uses
	punit,std,stringsx,sdkex;

const
	MIIM_STRING=64;

function SetMenuItemText(hMenu:HMENU;uItem:UINT;fByPosition:BOOL;const text:string):BOOL;

var
	info:MENUITEMINFO;

begin
	info.cbSize:=SizeOf(MENUITEMINFO);
	info.fMask:=MIIM_STRING;
	info.dwTypeData:=LPTSTR(PChar(AnsiString(text)));
	SetMenuItemText:=SetMenuItemInfo(hMenu,uItem,fByPosition,@info);
end;

function GetMenuItemText(hMenu:HMENU;uItem:UINT;fByPosition:BOOL):string;

var
	buffer:stringBuffer;

begin
	GetMenuString(hMenu,uItem,@buffer,LongInt(SizeOf(buffer)),UINT(Q(Boolean(fByPosition=TRUE),MF_BYPOSITION,MF_BYCOMMAND)));
	GetMenuItemText:=StrPas(buffer);
end;

procedure Test_GetMenuItemText;

var
	menu:HMENU;

begin
	menu:=CreateMenu;
	AppendMenu(menu,MF_STRING,101,'menu item by command');
	AppendMenu(menu,MF_STRING,102,'menu item by position');
	punit.Assert.EqualStrings('menu item by command',GetMenuItemText(menu,101,FALSE));
	punit.Assert.EqualStrings('menu item by position',GetMenuItemText(menu,1,TRUE));
end;

function LoadResString(resId:UINT):string;

var
	buffer:stringbuffer;

begin
	LoadString(hInstance,resId,buffer,sizeof(buffer));
	LoadResString:=StrPas(buffer);
end;

function LoadBitmapFromFile(path:pchar):HBITMAP;

begin //writeln('LoadBitmapFromFile(',path,')');
	LoadBitmapFromFile:=LoadImage(0,path,IMAGE_BITMAP,0,0,LR_LOADFROMFILE);
end;

function IsValidHandle(aHandle:HANDLE):boolean;

begin
	IsValidHandle:=(aHandle<>NULL_HANDLE);
end;

{$ifdef TEST}

procedure Test_IsValidHandle;

const
	NON_NULL_HANDLE=1;
	
begin
	AssertIsTrue(IsValidHandle(NON_NULL_HANDLE));
	AssertIsFalse(IsValidHandle(NULL_HANDLE));
end;

{$endif}

function CreateRect(x,y,w,h:longint):TRect;

var
	r:TRect;
	
begin
	SetRect(r,x,y,x+w,y+h);
	CreateRect:=r;
end;

function DevFontHt(aDC:hDC):integer; { actual font height }
{ Returns the height of a device's font not including the recommended external leading space. }
var
	T:TTextMetric;
begin
	GetTextMetrics(aDC,T);
	DevFontHt:=T.tmHeight;
end;

function DevLineHt(aDC:hDC):integer; { recommended space between lines }

	{ Returns the height of the applications font including the
		recommended external leading space. }

	var
		T:TTextMetric;

	begin
		GetTextMetrics(aDC,T);
		DevLineHt:=T.tmHeight+T.tmExternalLeading;
	end;

function SysFontHt:integer; { actual font height }

	var
		aDC:HDC;

	begin
		aDC:=GetDC(GetDesktopWindow);
		SysFontHt:=DevFontHt(aDC);
		ReleaseDC(GetDesktopWindow,aDC);
	end;

function SysLineHt:integer; { recommended space between lines }

	var
		aDC:hDC;

	begin
		aDC:=GetDC(GetDesktopWindow);
		SysLineHt:=DevLineHt(aDC);
		ReleaseDC(GetDesktopWindow,aDC);
	end;

function GetHdcTextWidth(a_hdc:HDC; a_pStr:PChar):Word;
var 
	aSize:TSize;
begin
	GetTextExtentPoint(a_hdc, a_pStr, StrLen(a_pStr), aSize);
	GetHdcTextWidth:= aSize.cx;
end;

function XpIntersectRect(var Dest: TRect; Src1,Src2: TRect): bool;
begin
	XpIntersectRect:= IntersectRect(Dest, Src1, Src2);
end;

procedure CenterWindow(source,target:HWND);
var
	R1,R2:TRect;
begin
	GetWindowRect(target,R2);
	GetWindowRect(source,R1);
	SetWindowPos(source,source,
		Center(GetRectWd(R1),R2.left,R2.right),
		Center(GetRectHt(R1),R2.top,R2.bottom),
		0,0,swp_NoSize or swp_NoZOrder);
end;

procedure Delay(milliSeconds:integer);
var
	aStartTick:LongWord;
begin
	aStartTick:=GetTickCount;
	while (GetTickCount-aStartTick) < milliSeconds do;
end;

procedure SetMenuBoolean(Menu:HMENU;Item:UINT;V:boolean);
{ Set the current state of a boolean menu "Item"  to correspond with the state of the boolean variable "V". }
var
	Check:Word;
begin
	Check:=mf_ByCommand;
	if V then
		Check:=Check or mf_Checked
	else
		Check:=Check or mf_Unchecked;
	CheckMenuItem(Menu,Item,Check);
end;

function DumpToString(const r:TRect):string;
begin
	DumpToString:='L:'+NumberToString(r.left)+',T:'+NumberToString(r.top)+',R:'+NumberToString(r.right)+',B:'+NumberToString(r.bottom)+',W:'+NumberToString(GetRectWd(r))+',H:'+NumberToString(GetRectHt(r));
end;

{$ifdef TEST}
begin
	Suite.Add(@Test_GetMenuItemText);
	Suite.Add(@Test_IsValidHandle);
	Suite.Run('windowsx');
{$endif}
end.
