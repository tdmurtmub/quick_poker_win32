{ (C) 2005 Wesley Steiner }

{$MODE FPC}

unit stringsx;

interface

const
	NEWLINE = #13#10;

type
	strToTextBuffer = array[0..255] of char;
	stringBuffer=strToTextBuffer;

function Capitalize(const aStr:string):string;
function Pluralize(aStr:string; aPlural:boolean):string;
function StrAlloc(aLength:word):pchar;
function StrLastChar(pText:pchar):char;

implementation

uses
	{$ifdef TEST} punit, {$endif}
	Std,Strings;

function Capitalize(const aStr:string):string;

var
	aString:string;

begin
	aString:= aStr;
	if Length(aString) > 0 then aString[1]:= UpCase(aString[1]);
	Capitalize:=aString;
end;

{$ifdef TEST}

procedure TestCapitalize;

begin
	Assert.EqualStr('', Capitalize(''));
	Assert.EqualStr('Capitalized', Capitalize('capitalized'));
end;

{$endif TEST}

function StrAlloc(aLength:word):pchar;

var
	p:pchar;

begin
	p:= nil;
	if aLength > 0 then begin
		GetMem(p, aLength + 1);
		FillChar(p^, aLength, ' ');
		p[aLength]:= #0;
	end;
	StrAlloc:= p;
end;

{$ifdef TEST}

procedure TestStrAlloc;

var
	p:pchar;

begin
	{ len = 0 should return a nil }
	Assert.EqualPtr(nil, StrAlloc(0));

	{ len > 0 }
	p:= StrAlloc(9);
	Assert.IsTrue(nil <> p);
	Assert.Equal(9, StrLen(p));
	Assert.EqualText('         ', p);

	{ should be able to dispose of the string }
	StrDispose(p);
end;

{$endif TEST}

function Pluralize(aStr:string; aPlural:boolean):string;

begin
	if aPlural
		then Pluralize:= aStr + 's'
		else Pluralize:= aStr;
end;

{$ifdef TEST}

procedure TestPluralize; 

begin
	Assert.EqualStr('table', Pluralize('table', false));
	Assert.EqualStr('xs', Pluralize('x', true));
end;

{$endif TEST}

function StrLastChar(pText:pchar):char;

begin
	StrLastChar:= pText[StrLen(pText) - 1];
end;

{$ifdef TEST}

procedure TestStrLastChar; 

begin
	Assert.EqualChar('.', StrLastChar('.'));
	Assert.EqualChar('.', StrLastChar('abc.'));
end;

{$endif TEST}

{$ifdef TEST}
begin
	Suite.Add(@TestCapitalize);
	Suite.Add(@TestStrAlloc);
	Suite.Add(@TestPluralize);
	Suite.Add(@TestStrLastChar);
	Suite.Run('StringX');
{$endif TEST}
end.
