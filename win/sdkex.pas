{ (C) 2001-2007 Wesley Steiner }

{$MODE TP}

unit sdkex; { OBSOLETE: uses windowsx }

interface

uses
	windows;

const
	sl_INIVal = 127; { max len of an INI keyword value }
	sl_INISection = 64; { max length of the section name }

function GetRectWd(const aRect:TRect):LONG;
function GetRectHt(const aRect:TRect):LONG;

function GetWndLeft(aHWnd:HWND):LONG;
function GetWndTop(aHWnd:HWND):LONG;
function GetWndWd(aHWnd:HWND):LONG;
function GetWndHt(aHWnd:HWND):LONG;
function GetClientWd(aHWnd:HWND):LONG;
function GetClientHt(aHWnd:HWND):LONG;

function ApiCheck(irc:LONG):LONG;

function DeleteINIKey(const aFileName,Section,KeyWord:PChar):boolean;
procedure ReadINIBoolean(const aFileName,Section,KeyWord:PChar; Default:boolean; var AppVar:boolean);
procedure ReadINIInt(const aFileName,Section,KeyWord:PChar; Default, nMin, nMax:integer; var AppVar:integer);
procedure ReadINILongint(const aFileName,Section,KeyWord:PChar; Default,nMin,nMax:longint; var AppVar:longint);
procedure ReadINIString(const aFileName,Section,KeyWord:PChar;const Default:PChar;AppVar:PChar);
procedure ReadINIWord(const aFileName,Section,KeyWord:PChar;const Default,nMin,nMax:Word;var AppVar:Word);
function WriteINIBoolean(const aFileName,Section,KeyWord:PChar; AppVar:boolean):boolean;
function WriteINIInt(const aFileName,Section,KeyWord:PChar;AppVar:integer):boolean;
function WriteINILongint(const aFileName,Section,KeyWord:PChar;AppVar:longint):boolean;
function WriteINIString(const aFileName,Section,KeyWord:PChar;AppVar:PChar):boolean;
function WriteINIWord(const aFileName,Section,KeyWord:PChar;AppVar:Word):boolean;

implementation

uses
	strings,
	std;

procedure ReadINIInt;

var
	szD,szR:array[0..sl_INIVal] of Char;

begin
	i2s(Default, szD);
	GetPrivateProfileString(Section,KeyWord,szD,szR,SizeOf(szD),aFileName);
	AppVar:=s2i(szR);
	if (AppVar<nMin) or (AppVar>nMax) then AppVar:=Default;
end;

Function l2s(I:LongInt;P:PChar):PChar;
var
	s:string[11];
begin
	Str(i,s);
	L2S:=StrPCopy(P,S);
end;

procedure ReadINILongint(const aFileName, Section,KeyWord:PChar; Default,nMin,nMax:longint; var AppVar:longint);

var
	szD,szR:array[0..sl_LongInt] of Char;

begin
	l2s(Default, szD);
	GetPrivateProfileString(Section,KeyWord,szD,szR,SizeOf(szD),aFileName);
	AppVar:=s2l(szR);
	if (AppVar<nMin) or (AppVar>nMax) then AppVar:=Default;
end;

procedure ReadINIBoolean;
{ Read a boolean INI variable status into "AppVar" via "KeyWord" in "Section"
	of "aFileName". boolean INI variable are of the form KeyWord=[T|F]. }

var
	szD,szR:array[0..1] of Char;

begin
	if Default then StrCopy(szD,'T') else StrCopy(szD,'F');
	GetPrivateProfileString(Section,KeyWord,szD,szR,SizeOf(szD),aFileName);
	AppVar:=(StrIComp(szR,'T')=0);
end;

procedure ReadINIWord;

var
	szD,szR:array[0..sl_INIVal] of Char;

begin
	w2s(Default,szD);
	GetPrivateProfileString(Section,KeyWord,szD,szR,SizeOf(szD),aFileName);
	AppVar:=s2w(szR);
	if (AppVar<nMin) or (AppVar>nMax) then AppVar:=Default;
end;

procedure ReadINIString;

var
	szD:array[0..sl_INIVal] of Char;

begin
	StrCopy(szD,Default);
	GetPrivateProfileString(Section,KeyWord,szD,AppVar,SizeOf(szD),aFileName);
end;

function WriteINIBoolean;

var
	szD:array[0..1] of Char;

begin
	if AppVar then StrCopy(szD,'T') else StrCopy(szD,'F');
	WriteINIBoolean:=WritePrivateProfileString(Section,KeyWord,szD,aFileName);
end;

function DeleteINIKey;

	begin
		DeleteINIKey:=WritePrivateProfileString(Section,KeyWord,nil,aFileName);
	end;

function WriteINIString;

	{ Update a string INI variable "AppVar" via "KeyWord" in "Section"
		of "aFileName". }

	begin
		WriteINIString:=WritePrivateProfileString(Section,KeyWord,AppVar,aFileName);
	end;

function WriteINIWord;

	{ Update a Word INI variable "AppVar" via "KeyWord" in "Section"
		of "aFileName". }

	var
		szD:array[0..sl_Word] of Char;
	begin
		WriteINIWord:=WritePrivateProfileString(Section,KeyWord,w2s(AppVar,szD),aFileName);
	end;

function WriteINIInt;

var
	szD:array[0..sl_Integer] of Char;

begin
	WriteINIInt:=WritePrivateProfileString(Section, KeyWord, i2s(AppVar,szD), aFileName);
end;

function WriteINILongint(const aFileName,Section,KeyWord:PChar;AppVar:longint):boolean;

var
	szD:array[0..50] of Char;

begin
	WriteINILongint:=WritePrivateProfileString(Section, KeyWord, l2s(AppVar,szD), aFileName);
end;

function GetRectWd(const aRect:TRect):LONG;

begin
	GetRectWd:=aRect.right-aRect.left;
end;

function GetRectHt(const aRect:TRect):LONG;

begin
	GetRectHt:=aRect.bottom-aRect.top;
end;

function GetWndLeft(aHWnd:HWND):LONG;

var
	r:TRect;

begin
	GetWindowRect(aHWnd, r);
	GetWndLeft:=r.left;
end;

function GetWndTop(aHWnd:HWND):LONG;

var
	r:TRect;

begin
	GetWindowRect(aHWnd,r);
	GetWndTop:=r.top;
end;

function GetWndWd(aHWnd:HWND):LONG;

var
	r:TRect;

begin
	GetWindowRect(aHWnd, r);
	GetWndWd:=GetRectWd(r);
end;

function GetWndHt(aHWnd:HWND):LONG;

var
	r:TRect;

begin
	GetWindowRect(aHWnd,r);
	GetWndHt:=GetRectHt(r);
end;

function GetClientWd(aHWnd:HWND):LONG;

var
	r:TRect;

begin
	GetClientRect(aHWnd,r);
	GetClientWd:=GetRectWd(r);
end;

function GetClientHt(aHWnd:HWND):LONG;

var
	r:TRect;

begin
	GetClientRect(aHWnd,r);
	GetClientHt:=GetRectHt(r);
end;

function ApiCheck(irc:LONG):LONG;

var
	lastError:DWORD;
	aMessage:LPVOID;

begin
	{$ifdef DEBUG}
	if irc=0 then begin
		lastError:=GetLastError;
		FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,NIL,lastError,MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),@aMessage,0,NIL);
		WriteLn('Win32 API Error: #',lastError,' ',PChar(aMessage));
		Dump_Stack(output,get_caller_frame(get_frame));
		Halt;
	end;
	{$endif}
	ApiCheck:=irc;
end;

end.
