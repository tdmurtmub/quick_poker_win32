{ Windows GDI Extensions }
{ (C) 2005 Wesley Steiner }

unit gdiex;

interface

uses
	windows;

const
	RGB_BLACK:TColorRef 		= $00000000;
	RGB_BLUE:TColorRef 			= $00FF0000;
	RGB_DARK_GRAY:TColorRef 	= $00808080;
	RGB_LIGHT_GRAY:TColorRef 	= $00C0C0C0;
	RGB_WHITE:TColorRef 		= $00FFFFFF;

function Red:TColorRef;
function Green:TColorRef;
function DarkGreen:TColorRef;
function Blue:TColorRef;
function DarkBlue:TColorRef;
function DarkRed:TColorRef;
function Yellow:TColorRef;
function DarkYellow:TColorRef;
function DarkGray:TColorRef;
function gray:TColorRef;
function Cyan:TColorRef;
function DarkCyan:TColorRef;

function GetBitmapWd(P:HBITMAP):integer;
function GetBitmapHt(P:HBITMAP):integer;
function MirrorBitmap(source:HBITMAP):HBITMAP;
function TextOut2(DC: HDC; X, Y: Integer; Str: PChar):Bool;

procedure GetBitmap(aDC:hDC; x,y:integer; Rop:LongInt; a_hbitmap:hBitmap);
procedure PutBitmap(aDC:hDC; a_hbitmap:hBitmap; xo, yo:integer; Rop:LongInt);

implementation

uses
	{$ifdef TEST} PUnit, {$endif}
	sdkex,strings;

function TextOut2(DC: HDC; X, Y: Integer; Str: PChar):Bool;

begin
	TextOut2:=TextOut(DC, X, Y, Str, StrLen(Str));
end;

function MirrorBitmap(source:HBITMAP):HBITMAP;
var
	bm:TBitmap;
	sourceDC,targetDC:HDC;
	target,targetBM,sourceBM:HBITMAP;
begin
	GetObject(source,SizeOf(bm),@bm);
	sourceDC:=CreateCompatibleDC(0);
	sourceBM:=SelectObject(sourceDC,source);
	targetDC:=CreateCompatibleDC(sourceDC);
	target:=CreateCompatibleBitmap(sourceDC,bm.bmWidth,bm.bmHeight);
	targetBM:=SelectObject(targetDC,target);
	StretchBlt(targetDC,0,0,bm.bmWidth,bm.bmHeight,sourceDC,bm.bmWidth-1,bm.bmHeight-1,-bm.bmWidth,-bm.bmHeight,SRCCOPY);
	SelectObject(targetDC,targetBM);
	SelectObject(sourceDC,sourceBM);
	DeleteDC(targetDC);
	DeleteDC(sourceDC);
	MirrorBitmap:=target;
end;

function GetBitmapWd(P:HBITMAP):integer;
var
	T:TBitmap;
begin
	GetBitmapWd:=0;
	if P<>0 then begin
		GetObject(P,SizeOf(T),@T);
		GetBitmapWd:=T.bmWidth;
	end;
end;

function GetBitmapHt(P:HBITMAP):integer;
var
	T:TBitmap;
begin
	GetBitmapHt:=0;
	if P<>0 then begin
		GetObject(P,SizeOf(T),@T);
		GetBitmapHt:=T.bmHeight;
	end;
end;

procedure PutBitmap(aDC:hDC; a_hbitmap:hBitmap; xo, yo:integer; Rop:LongInt);
{ Display a bitmap "a_hbitmap" at xo,yo in aDC using "Rop". }
var
	hdcAbout:HDC;
	hdcBitmap:HBITMAP;
	tbm:TBitmap;
begin
	GetObject(a_hbitmap,SizeOf(tbm),@tbm);
	hdcAbout:=CreateCompatibleDC(aDC);
	hdcBitmap:=SelectObject(hdcAbout,a_hbitmap);
	BitBlt(aDC,xo,yo,tbm.bmWidth,tbm.bmHeight,hdcAbout,0,0,Rop);
	SelectObject(hdcAbout,hdcBitmap);
	DeleteDC(hdcAbout);
end;

procedure GetBitmap(aDC:hDC; x,y:integer; Rop:LongInt; a_hbitmap:hBitmap);
{ Get a bitmap at x,y wd,ht in aDC using "Rop". }
var
	hdcAbout:HDC;
	hdcBitmap:HBITMAP;
	tbm:TBitmap;
begin
	GetObject(a_hbitmap,SizeOf(tbm),@tbm);
	hdcAbout:=CreateCompatibleDC(aDC);
	hdcBitmap:=SelectObject(hdcAbout,a_hbitmap);
	BitBlt(hdcAbout,0, 0, tbm.bmWidth, tbm.bmHeight, aDC, x, y, Rop);
	SelectObject(hdcAbout,hdcBitmap);
	DeleteDC(hdcAbout);
end;

function Blue:TColorRef; begin Blue:= RGB_BLUE; end;
function DarkBlue:TColorRef; begin DarkBlue:= RGB(0, 0, 127); end;
function Red:TColorRef; begin Red:= RGB(255, 0, 0); end;
function DarkGreen:TColorRef; begin DarkGreen:= RGB(0, 127, 0); end;
function Green:TColorRef; begin Green:= RGB(0, 255, 0); end;
function DarkRed:TColorRef; begin DarkRED:= RGB(127, 0, 0); end;
function Yellow:TColorRef; begin Yellow:= RGB(255, 255, 0); end;
function DarkYellow:TColorRef; begin DarkYellow:= RGB(127, 127, 0); end;
function DarkGray:TColorRef; begin DarkGray:= RGB_DARK_GRAY; end;
function Cyan:TColorRef; begin Cyan:= RGB(0, 255, 255); end;
function gray:TColorRef; begin gray:= RGB(128, 128, 128); end;
function DarkCyan:TColorRef; begin DarkCyan:= RGB(0, 127, 127); end;

{$ifdef TEST}

{procedure Test_Object_Method; 

	begin
		Assert.Fail;
	end;
}

{$endif TEST}

begin

	{$ifdef TEST}
{	Suite.Add(Test_Object_Method);
	Suite.Run('gdiex');
}	{$endif TEST}

end.
