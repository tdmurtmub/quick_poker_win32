{ (C) 1991 Wesley Steiner }

{$MODE FPC}

unit std;

{$I platform}

interface

uses
	dos;

const
	EMPTY_STRING='';
	{ string lengths }
	sl_FileDir=67; { d:\ppp\ppp... }
	sl_FileName=8; { "XXXXXXXX" }
	sl_FileExt=4; { ".XXX" }
	sl_Path=sl_FileDir+sl_FileName+sl_FileExt;
	sl_Byte=3; { nnn }
	sl_Word=5; { nnnnn }
	sl_Integer=6; { -nnnnn }
	sl_LongInt=11;
	sl_Real=16;
	sl_Single=12;
	
	{ common ASCII codes }
	TAB      = #$09;
	LF       = #$0A;
	FF       = #$0C;
	CR       = #$0D;
	BS       = #$08;
	SO       = #$0E;
	SI       = #$0F;
	ESC      = #$1B;
	SPACE    = #$20;
	BLANK		 = #$20;
	KEY_F1    = #59;
	KEY_F2    = #60;
	KEY_F3    = #61;
	KEY_F4    = #62;
	KEY_F5    = #63;
	KEY_F6    = #64;
	KEY_F7    = #65;
	KEY_F8    = #66;
	KEY_F9    = #67;
	KEY_F10   = #68;
	KEY_HOME  = #71;
	KEY_END   = #79;
	KEY_INS   = #82;
	KEY_DEL   = #83;
	KEY_PGUP  = #73;
	KEY_PGDN  = #81;
	KEY_LARR  = #75;
	KEY_RARR  = #77;
	KEY_UARR  = #72;
	KEY_DARR  = #80;

	MAX_INT=32767;
	MAX_LONG=2147483647;
	MAX_LONGWORD=$FFFFFFFF;
	MAX_NUMBER=MAX_LONGWORD;
	MAX_WORD=$FFFF;
	MAX_DOUBLE=1.7e38;
	MAX_REAL=MAX_DOUBLE;
	MAX_QUANTITY=MAX_LONGWORD;
	MAX_ORDINAL=MAX_LONGWORD;
	MAX_STACK_SIZE=100;
	MIN_INT=-32768;
	MIN_LONG=-2147483648;
	MIN_LONGWORD=0;
	MIN_NUMBER=1;
	MIN_ORDINAL=0;
	MIN_QUANTITY=0;
	MIN_DOUBLE=2.9e-39;
	MIN_REAL=MIN_DOUBLE;

type
	bit32=longint;
	bitflags=bit32;
	charPtr=^char;
	flags=bitflags;
	int8=byte;
	int16=smallint;
	int32=longint;
	quantity=MIN_QUANTITY..MAX_QUANTITY;
	ordinal=MIN_ORDINAL..MAX_ORDINAL;
	number=MIN_NUMBER..MAX_NUMBER;
	booleanPtr=^boolean;
	bytePtr=^byte;
	integerPtr=^integer;
	longintPtr=^longInt;
	byteArray=array[0..MAX_WORD-1] of byte;
	intArray=array[0..MAX_INT-1] of integer;
	PString=^String;
	{$ifdef WINDOWS}
	TString=PChar;
	{$else}
	TString=String;
	{$endif}
	DriveLetter='A'..'Z';
	word16=word;
	word32=longword;

	gender=(MALE, FEMALE);

	sortByteCmpFunc=function(a, b:byte):integer; { return - 0 + }
	
	Stack32=object
		constructor Construct;
		function Size:word;
		procedure Push(aValue:longint);
		function Peek:longint;
		function Pop:longint;
	private
		mySize:word;
		myStack:array [1..MAX_STACK_SIZE] of longint;
	end;

	type PointerStack=object (Stack32)
		procedure Push(aPointer:pointer);
		function Peek:pointer;
		function Pop:pointer;
	end;

	type PointerStackPtr=^PointerStack;

	xypair=longword;
	xypairWrapper=record
		x,y:smallint;
	end;

var
	SystemDir:string[sl_FileDir]; { DOS directory }

function CurrentDrive:DriveLetter;
function PointingDeviceActive:boolean; { true if a mouse is operational }
function Q(test:boolean;trueVal,falseVal:char):char;
function Q(test:boolean; trueVal,falseVal:integer):integer;
function Q(test:boolean; trueVal,falseVal:longint):longint;
function Q(test:boolean; trueVal,falseVal:string):string;
function IsWriteProtected(aDrive:DriveLetter):boolean;
function IsWritable(aDrive:DriveLetter):boolean;
function MakeXYPair(x, y:smallint):xypair;
function Max(a,b:longint):longint;
function Max(a,b:longword):longword;
function Max(a,b:real):real;
function Min(a,b:longint):longint;
function Min(a,b:longword):longword;
function Min(a,b:real):real;
function ForceOddUp(i:integer):integer;
function MaxWord(w1,w2:word):word;
function minW(w1,w2:word):word; // obsolete
function MinR(a,b:real):real; // obsolete
function Nudge(aValue,aDelta:integer):integer;
function NumberToString(source:integer):string;
function NumberToString(source:longint):string;
function NumberToString(source:quantity):string;
function NumberToString(source:word):string;
function NumberToText(source:word):pchar; // WARNING: Only the last conversion is valid. Each use overwrites the previous call!
function Center(aSize,LowVal,HighVal:integer):integer;
function Center(aSize,LowVal,HighVal:longint):longint;
function QInteger(Test:boolean;TResult,FResult:integer):integer;
function Probability(likely:integer):boolean;
function Search(var a;n:integer;s:integer;var Target):Word;
function SearchWord(const WordArray:array of Word; n:Word; SearchValue:Word):integer;
function StringEndsWith(source,suffix:string):boolean;
function StringUpper(source:string):string;
function StringToText(source:string):pchar; // WARNING: Only the last conversion is valid. Each use overwrites the previous call!

procedure DumpStack;
procedure PlaceHolder;
procedure RandomShuffleBytes(p:bytePtr;n:word);
procedure SortByte(var a; n:integer; cmp:sortByteCmpFunc);

function b2s(i:Byte;p:pchar):pchar;
function CircInt(aMinVal,aMaxVal,aStartVal,aCycle:integer):integer;
function Deg2Rad(a:Real):Real;
function DegToRad(a:Real):Real;
function strClear(p_string:pchar):pchar;
function i2s(i:integer;p:pchar):pchar;
function l2s(i:longint;p:pchar):pchar;
function LongMul(X, Y: Integer): Longint;
function LongDiv(X: Longint; Y: Integer): Integer;
function s2i(P:pchar):integer;
function w2s(n:Word;p:pchar):pchar;
function itoz(iData:integer; zBuffer:pchar):pchar;
function wtoz(wData:Word; zBuffer:pchar):pchar;
function ztow(z:pchar):Word;
function IsEmptyString(aString:ansistring):boolean;
function IsEmptyString(aString:Pointer):boolean;
function Centered(A,P1,P2:longint):longint; // obsolete
function StringEmpty(const aString:string):boolean;
procedure StringClear(var aString:string);
function wtos(w:Word):string; // OBSOLETE: use NumberToString
function Int2Str(i:integer):string; // OBSOLETE: use NumberToString
function long2str(i:longint):string; // OBSOLETE: use NumberToString
function str2int(s:string):integer; // OBSOLETE: use NumberToString
function IntToStr(i:integer):string; // OBSOLETE: use NumberToString
procedure Upstr(var s);
function Upper(var s):string;
function NewStr(const S:string): PString;
function RadToDeg(a:Real):Real;
function RandomReal:real;
function s2l(P:pchar):LongInt;
function s2w(P:pchar):word;
function StrIsEmpty(const aString:pchar):boolean;
function StrEmpty(aString:Pointer):boolean;
function ExtractFileFromPath(path:pathstr):string;
function ExtractDirectoryFromPath(path:pathstr):dirstr;

procedure Beep;
procedure ClearTypeAhead;
procedure DisposeStr(P:PString);
procedure EmptyStr(var aString:TString);
procedure Toggle(var v:boolean);
procedure NotImplemented;
procedure WriteHexWord(w: Word);

const
	UNITEDSTATES=001;
	CANADA=002;
	UNITEDKINGDOM=044;

function CountryCode:integer;

implementation

uses
	sysutils,
	{$ifdef TEST} punit, {$endif}
	{$ifdef DOS} crt,stdd,msmouse, {$endif}
	{$ifdef WINDOWS} windows, {$endif}
	strings;

var
	defaultHeapFunc:pointer;

function NilHeapFunc(size:word):integer;

	{ Returns a nil value when not enough space. }

	begin
		nilHeapFunc:=1;
	end;

function MsgHeapFunc(size:word):integer;

	{ Abort with the given message. }

	begin
		if size>0 then begin
			halt(1);
		end;
		MsgHeapFunc:=0;
	end;

function wtos(w:Word):string;

	begin
		wtos:=NumberToString(w);
	end;

function int2str(i:integer):string;

	var
		s:string[20];

	begin
		str(i,s);
		int2str:=s;
	end;

function long2str(i:longint):string;

	var

		s:string[11];

	begin

		str(i,s);
		long2str:=s;

	end;

function IntToStr(i:integer):string; begin intToStr:=sysutils.IntToStr(i); end;

function str2int(s:string):integer;

var
	i:longint;
	code:word;

begin
	val(s,i,code);
	str2int:=integer(i);
end;

procedure UpStr(var s);

	{ Converts a string s to upper case }

	var
		i:shortint;

	begin { upstr }
		for i:= 1 to length(string(s)) do string(s)[i]:=upcase(string(s)[i]);
	end; {upstr }

function Upper(var s):string;

begin
	UpStr(s);
	Upper:=String(S);
end;

function StringUpper(source:string):string;
var
	s:String;
begin
	s:=source;
	StringUpper:=Upper(s);
end;

function Min(a,b:longint):longint;
begin
	if a<b then Min:=a else Min:=b;
end;

function Min(a,b:longword):longword;
begin
	if a<b then Min:=a else Min:=b;
end;

function minW(w1,w2:word):word;
begin
	if w1<w2 then
		minW:=w1
	else
		minW:=w2;
end;

function Max(a,b:longint):longint;
begin
	if a>b then Max:=a else Max:=b;
end;

function Max(a,b:longword):longword;
begin
	if a>b then Max:=a else Max:=b;
end;

function Max(a,b:real):real;
begin
	if a>b then Max:=a else Max:=b;
end;

function Min(a,b:real):real;
begin
	if a<b then min:=a else min:=b;
end;

function MinR(a,b:real):real;
begin
	MinR:=Min(a, b);
end;

function maxW(w1,w2:word):word;
begin
	if w1>w2 then
		maxW:=w1
	else
		maxW:=w2;
end;

function RandomReal:real;

{ Return a random number between 0.0 and 1.0 }

begin
	RandomReal:=Random(10001)/10000;
end;

function CountryCode:integer;

begin
	CountryCode:=UNITEDSTATES;
end;

(*function GetYear:word;

	{ Return the current year. }

	var
		cYr,cMt,cDy,Dow:word;

	begin
		GetDate(cYr,cMt,cDy,dow);
		GetYear:=cYr;
	end;
*)
procedure Toggle(var v:boolean);

	{ Toggles a boolean variable. }

	begin
		v:=not v;
	end;

procedure ClearTypeAhead;

	begin
		{$ifdef WINDOWS}
		{$else}
		mem[0:1050]:=mem[0:1052];
		{$endif}
	end;

function PointingDeviceActive:boolean; { true if a mouse is operational }

	{ return true if the system has an active pointing device }

	begin
		PointingDeviceActive:=(GetSystemMetrics(sm_MousePresent)<>0);
	end;

function NewStr(const S: String): PString;
var
	P: PString;
begin
	if S = '' then P := nil else
	begin
		GetMem(P, Length(S) + 1);
		P^ := S;
	end;
	NewStr := P;
end;

procedure DisposeStr(P: PString);

begin
	if P <> nil then FreeMem(P, Length(P^) + 1);
end;

function StrEmpty(aString:Pointer):boolean;

	{ works for both String and PChar }

	begin
		StrEmpty:=(CharPtr(aString)^=#0);
	end;

function StrIsEmpty(const aString:pchar):boolean;

	begin
		StrIsEmpty:=StrEmpty(Pointer(aString));
	end;

function StringEmpty(const aString:string):boolean;

	{ works only for type "string" }

	begin
		StringEmpty:=(Length(aString)=0);
	end;

procedure EmptyStr(var aString:TString);

	begin
		{$ifdef WINDOWS}
		aString^
		{$else}
		aString[0]
		{$endif}
		:=#0;
		end;

procedure StringClear(var aString:string);

	begin
		aString[0]:=#0;
	end;

function QInteger(Test:boolean;TResult,FResult:integer):integer;

	begin
		if Test then
			QInteger:=TResult
		else
			QInteger:=FResult;
	end;

function IsWriteProtected(aDrive:DriveLetter):boolean;

	{ !!! this function and "IsWritable" should temporarily trap the
		current critical error handler since an application may
		have set up its own that will get called when you try
		and rewrite }

	{ return true if "aDrive" is write protected }

	var
		aFile:file;
		i:integer;

	begin
		Assign(aFile,aDrive+':$123456$');
		{$I-} Rewrite(aFile); {$I+}
		i:=IOResult;
		if i=19 then
			IsWriteProtected:=True
		else begin
			{$I-}
			Close(aFile);
			Erase(aFile);
			{$I+}
			i:=IOResult;
			IsWriteProtected:=False
		end;
	end;

function IsWritable(aDrive:DriveLetter):boolean;

	{ return true if "aDrive" is accessable for writing to }

	var
		aFile:file;
		i:integer;

	begin
		Assign(aFile,aDrive+':$123456$');
		{$I-} Rewrite(aFile); {$I+}
		i:=IOResult;
		if i<>0 then
			IsWritable:=False
		else begin
			{$I-}
			Close(aFile);
			Erase(aFile);
			{$I+}
			i:=IOResult;
			IsWritable:=True
		end;
	end;

function CurrentDrive:DriveLetter;

	{ return the current drive letter }

	var
		s:string;

	begin
		GetDir(0,s);
		CurrentDrive:=UpCase(s[1]);
	end;

function IsEmptyString(aString:Pointer):boolean;

{ the nice thing about Strings and PChars is that the first byte
	is zero for empty strings for both type. }

begin
	IsEmptyString:=(Byte(aString^)=0);
end;

function IsEmptyString(aString:ansistring):boolean;

begin
	IsEmptyString:=(Length(aString)=0);
end;

{$ifdef TEST}

procedure Test_IsEmptyString_ansistring;

begin
	AssertIsTrue(IsEmptyString(''));
	AssertIsFalse(IsEmptyString(' '));
end;

{$endif}

procedure Beep;

{ play a standard system beep }

begin
	{$ifdef WINDOWS}
	{$else}
	Sound(355);
	Delay(250);
	NoSound;
	{$endif}
end;

function itoz(iData:integer; zBuffer:pchar):pchar;

	var
		sBuffer:String;

	begin
		Str(iData, sBuffer);
		itoz:= StrPCopy(zBuffer, sBuffer);
	end;

function wtoz(wData:Word;zBuffer:pchar):pchar;

	var
		sBuffer:String;

	begin
		Str(wData,sBuffer);
		wtoz:=StrPCopy(zBuffer,sBuffer);
	end;

function ztow(z:pchar):Word;
var
	s:String;
	w:longint;
	code:integer;
begin
	s:=StrPas(z);
	val(s,w,code);
	ztow:=Word(w);
end;

function Probability(likely:integer):boolean;

begin
	Probability:= (Random(1000) < likely * 10);
end;

function Search(var a; n:integer; s:integer; var Target):Word;

begin
	Search:= n;
end;

type
	TWordArray = array[0..1] of Word;

function SearchWord(const WordArray:array of Word; n:Word; SearchValue:Word):integer;

{ Linear Word search.  Returns the index into "WordArray" (0..n-1) or n if not found }

var
	i:Word;

begin
	for i:= 0 to n - 1 do if (WordArray[i] = SearchValue) then Break;
	SearchWord:= i;
end;

function Centered(A,P1,P2:longint):longint;

{ Return the coordinate between "P1" and "P2" where P1<P2, that centers something that is "A" points wide or high. }

begin
	Centered:=P1+((P2-P1+1-A) div 2);
end;

function Center(aSize,LowVal,HighVal:integer):integer;

begin
	Center:=Integer(Centered(aSize,LowVal,HighVal));
end;

function Center(aSize,LowVal,HighVal:longint):longint;

begin
	Center:=Centered(aSize,LowVal,HighVal);
end;

procedure sortByte(var a; n:integer; cmp:sortByteCmpFunc);

{ Sort the byte array "a" of "n" bytes using the "cmp" function.
	Uses a Straight Selection sort technique. }

var
	i,j,k:integer;
	t:byte;

begin
	for i:= 1 to n - 1 do begin
		k:= i;
		t:= byteArray(a)[i - 1];
		for j:= i + 1 to n do
			if (cmp(byteArray(a)[j - 1], t) < 0) then begin
				k:= j;
				t:= byteArray(a)[j - 1];
			end;
		byteArray(a)[k - 1]:= byteArray(a)[i - 1];
		byteArray(a)[i - 1]:= t;
	end;
end;

function strClear(p_string:pchar):pchar;

begin
	p_string[0]:= #0;
	strClear:= p_string;
end;

function i2s(i:integer;p:pchar):pchar;

var
	S:String[sl_Integer];

begin
	Str(I,S);
	I2S:=StrPCopy(P,S);
end;

function L2S(i:longint;p:pchar):pchar;

var
	S:String[sl_longint];

begin
	Str(I,S);
	L2S:=StrPCopy(P,S);
end;

function DegToRad(a:real):real;

begin
	DegToRad:= Deg2Rad(a);
end;

function deg2rad(a:real):real;

begin
	deg2rad:= a * pi / 180;
end;

function RadToDeg(a:real):Real;

begin
	RadToDeg:= a * 180 / pi;
end;

function S2I(P:pchar):integer;
var
	i:longint;
	code:word;
	S:String;
begin
	S:=StrPas(P);
	Val(S,i,code);
	S2I:=Integer(i);
end;

function b2s(i:Byte;p:pchar):pchar;
var
	S:String[sl_Byte];
begin
	Str(i,s);
	b2s:=StrPCopy(p,s);
end;

function w2s(n:Word;p:pchar):pchar;
var
	S:String[6];
begin
	Str(n,S);
	w2s:=StrPCopy(p,s);
end;

function s2w(p:pchar):word;
var
	code:word;
	i:longint;
	s:string;
begin
	s:=StrPas(P);
	Val(S,i,code);
	s2w:=Word(i);
end;

function s2l(P:pchar):LongInt;
var
	code:integer;
	i:LongInt;
	s:string[sl_LongInt];
begin
	s:=StrPas(P);
	Val(S,I,Code);
	s2l:=i;
end;

function Q(test:boolean;trueVal,falseVal:integer):integer;

begin
	Q:=Integer(Q(test,LongInt(trueVal),LongInt(falseVal)));
end;

function Q(test:boolean;trueVal,falseVal:longint):longint;

begin
	if test then Q:=trueVal else Q:=falseVal;
end;

function Q(test:boolean;trueVal,falseVal:char):char;

begin
	if test then Q:=trueVal else Q:=falseVal;
end;

function Q(test:boolean;trueVal,falseVal:string):string;

begin
	if test then Q:=trueVal else Q:=falseVal;
end;

{$ifdef TEST}

procedure Test_Q; 

begin
	AssertAreEqual(1,Q(TRUE,1,2));
	AssertAreEqual(2,Q(FALSE,1,2));
	AssertAreEqual('a',Q(TRUE,'a','b'));
	AssertAreEqual('b',Q(FALSE,'a','b'));
	AssertAreEqual('string1',Q(TRUE,'string1','string2'));
	AssertAreEqual('string2',Q(FALSE,'string1','string2'));
end;

{$endif}

procedure WriteHexWord(w: Word);
const
  hexChars: array [0..$F] of Char =
	 '0123456789ABCDEF';
begin
  Write(hexChars[Hi(w) shr 4],
		  hexChars[Hi(w) and $F],
		  hexChars[Lo(w) shr 4],
		  hexChars[Lo(w) and $F]);
end;

function CircInt(aMinVal,aMaxVal,aStartVal,aCycle:integer):integer;

begin
	system.Assert(aMaxVal>=aMinVal,'aMaxVal must be >= aMinVal');
	system.Assert(aStartVal<=(aMaxVal - aMinVal + 1),'aStartVal must be >= aMinVal and <= aMaxVal');
	system.Assert(aCycle>0,'aCycle must be > 0');
	CircInt:=aMinVal+(aStartVal-aMinVal+aCycle-1) mod (aMaxVal-aMinVal+1);
end;

{$ifdef TEST}

procedure Test_CircInt; 

begin
	AssertAreEqual(0, CircInt(0, 0, 0, 1));
	AssertAreEqual(1, CircInt(1, 1, 1, 100));
	AssertAreEqual(-13, CircInt(-14, +17, -14, 2));
	AssertAreEqual(5, CircInt(-5, +5, -5, 11));
	AssertAreEqual(-5, CircInt(-5, +5, -5, 12));
	AssertAreEqual(6, CircInt(-3, +8, +6, 1));
	AssertAreEqual(-2, CircInt(-3, +8, +6, 5));
end;

{$endif}

procedure PlaceHolder; begin end;

{$ifndef FPC}

procedure Unimplemented;

begin
	Abstract;
end;

{$endif}

function LongMul(X, Y: Integer): Longint;
begin
	LongMul:=X*Y;
end;

function LongDiv(X: Longint; Y: Integer): Integer;
begin
	LongDiv:=X div Y;
end;

procedure RandomShuffleBytes(p:bytePtr;n:word);
var
	i,a,b:word;
	t:byte;
begin
	for i:=1 to n*3 do begin
		a:=Word(random(n));
		b:=wORD(random(n));
		t:=byteArray(p)[a];
		byteArray(p)[a]:=byteArray(p)[b];
		byteArray(p)[b]:=t;
	end;
end;

function Nudge(aValue,aDelta:integer):integer;
begin
	Nudge:=aValue+(aDelta-Random(abs(aDelta)*2));
end;

function MaxWord(w1,w2:word):word;
begin
	MaxWord:=MaxW(w1,w2);
end;

function Stack32.Size:word;

begin
	Size:=mySize;
end;

constructor Stack32.Construct;
begin
	mySize:=0;
end;

procedure PointerStack.Push(aPointer:pointer);
begin
	inherited Push(LongInt(aPointer));
end;

function PointerStack.Pop:pointer;
begin
	Pop:=Pointer(inherited Pop);
end;

function PointerStack.Peek:pointer;
begin
	Peek:=Pointer(inherited Peek);
end;

procedure Stack32.Push(aValue:longint);
begin
	if (Size=MAX_STACK_SIZE) then RunError(999);
	inc(mySize);
	myStack[mySize]:=aValue;
end;

function Stack32.Pop:longint;
begin
	if (Size=0) then RunError(999);
	Pop:=myStack[mySize];
	Dec(mySize);
end;

function Stack32.Peek:longint;
begin
	if (Size=0) then RunError(999);
	Peek:=myStack[mySize];
end;

{$ifdef TEST}

procedure Test_Stack32_Push;
var 
	aStack:Stack32;
begin
	aStack.Construct;
	AssertAreEqual(0,aStack.Size);
	aStack.Push(1827);
	AssertAreEqual(1,aStack.Size);
	AssertAreEqual(1827,aStack.myStack[aStack.Size]);
end;

procedure Test_Stack32_Pop;
var 
	aStack:Stack32;
begin
	aStack.Construct;
	aStack.Push(245);
	AssertAreEqual(245,aStack.Pop);
	AssertAreEqual(0,aStack.Size);
end;

procedure Test_Stack32_Peek;
var 
	aStack:Stack32;
begin
	aStack.Construct;
	aStack.Push(245);
	aStack.Push(246);
	AssertAreEqual(246,aStack.Peek);
	AssertAreEqual(2,aStack.Size);
end;

{$endif TEST}

function StringEndsWith(source,suffix:string):boolean;
begin
	if Length(suffix)=0 
		then StringEndsWith:=FALSE
		else StringEndsWith:=(Copy(source,Length(source)-Length(suffix)+1,Length(suffix))=suffix);
end;

{$ifdef TEST}

procedure Test_StringEndsWith;

begin
	punit.Assert.IsTrue(StringEndsWith('a test string','string'));
	punit.Assert.IsFalse(StringEndsWith('a test string','not'));
	punit.Assert.IsFalse(StringEndsWith('','suffix'));
	punit.Assert.IsFalse(StringEndsWith('source',''));
end;

{$endif TEST}

function MakeXYPair(x,y:smallint):xypair;

begin
	MakeXYPair:=(LongWord(y) shl 16) or Word(x);	
end;

{$ifdef TEST}

procedure Test_MakeXYPair;

var
	xy:xypair;

begin
	xy:=MakeXYPair(1,2);
	AssertAreEqual(LongWord($00020001),xy);
	xy:=MakeXYPair(-1,2);
	AssertAreEqual(LongWord($0002FFFF),xy);
	xy:=MakeXYPair(-1,-2);
	AssertAreEqual(LongInt($FFFEFFFF),LongInt(xy));
end;

procedure Test_XYPairWrapper;

var
	xy:xypair;

begin
	xy:=MakeXYPair(3,4);
	AssertAreEqual(3,XYPairWrapper(xy).x);
	AssertAreEqual(4,XYPairWrapper(xy).y);
	xy:=MakeXYPair(-3,4);
	AssertAreEqual(-3,XYPairWrapper(xy).x);
	AssertAreEqual(4,XYPairWrapper(xy).y);
	xy:=MakeXYPair(3,-4);
	AssertAreEqual(3,XYPairWrapper(xy).x);
	AssertAreEqual(-4,XYPairWrapper(xy).y);
	xy:=MakeXYPair(-3,-4);
	AssertAreEqual(-3,XYPairWrapper(xy).x);
	AssertAreEqual(-4,XYPairWrapper(xy).y);
end;

{$endif TEST}

function ExtractFileFromPath(path:pathstr):string;

var
    aDir:dirstr;
	aName:namestr;
	aExt:extstr;

begin
	FSplit(path,aDir,aName,aExt);
	ExtractFileFromPath:=aName+aExt
end;

function ExtractDirectoryFromPath(path:pathstr):dirstr;

var
    aDir:dirstr;
	aName:namestr;
	aExt:extstr;

begin
	FSplit(path,aDir,aName,aExt);
	ExtractDirectoryFromPath:=aDir;
end;

function StringToText(source:string):pchar;
// WARNING: Only the last conversion is valid. Each use overwrites the previous call!
begin
	StringToText:=PChar(AnsiString(source));
end;

function NumberToString(source:word):string;
begin
	NumberToString:=long2str(source);
end;

function NumberToString(source:longint):string;
begin
	NumberToString:=long2str(source);
end;

function NumberToString(source:integer):string;
begin
	NumberToString:=sysutils.IntToStr(source);
end;

function NumberToString(source:quantity):string;
var
	s:string;
begin
	Str(source,s);
	NumberToString:=s;
end;

function NumberToText(source:word):pchar;
// WARNING: Only the last conversion is valid. Each use overwrites the previous call!
begin
	NumberToText:=StringToText(NumberToString(source));
end;

{$ifdef TEST}

procedure Test_ExtractFileFromPath;

begin
	AssertAreEqual('',ExtractFileFromPath(''));
	AssertAreEqual('name.ext',ExtractFileFromPath('C:\dir\name.ext'));
end;

procedure test_StringToText;

begin
	AssertAreEqual(PChar('test string'),StringToText('test string'));
end;

procedure test_NumberToString;

begin
	AssertAreEqual('0', NumberToString(Word(0)));
	AssertAreEqual('65535', NumberToString(Word(MAX_WORD)));
	AssertAreEqual('4294967295', NumberToString(Quantity(MAX_QUANTITY)));
end;

{$endif}

procedure NotImplemented;

begin
	Halt;
end;

function ForceOddUp(i:integer):integer;

begin
	ForceOddUp:=(i or 1);
end;

{$ifdef TEST}

procedure test_ForceOddUp;
begin
	AssertAreEqual(1,ForceOddUp(0));
	AssertAreEqual(3,ForceOddUp(3));
	AssertAreEqual(457,ForceOddUp(456));
	AssertAreEqual(-455,ForceOddUp(-456));
end;

procedure Test_MinMax;
begin
	AssertAreEqual(MAX_LONG, Max(MAX_LONG, MIN_LONG));
	AssertAreEqual(MAX_LONG, Max(MIN_LONG, MAX_LONG));
	AssertAreEqual(MAX_LONGWORD, Max(MAX_LONGWORD, MIN_LONGWORD));
	AssertAreEqual(MAX_LONGWORD, Max(MIN_LONGWORD, MAX_LONGWORD));
	AssertAreEqual(MAX_REAL, Max(MIN_REAL, MAX_REAL), 0.01);
	AssertAreEqual(MAX_REAL, Max(MAX_REAL, MIN_REAL), 0.01);
	AssertAreEqual(MIN_LONG, Min(MIN_LONG, MAX_LONG));
	AssertAreEqual(MIN_LONG, Min(MAX_LONG, MIN_LONG));
	AssertAreEqual(MIN_LONGWORD, Min(MAX_LONGWORD, MIN_LONGWORD));
	AssertAreEqual(MIN_LONGWORD, Min(MIN_LONGWORD, MAX_LONGWORD));
	AssertAreEqual(MIN_REAL, Min(MIN_REAL, MAX_REAL), 0.01);
	AssertAreEqual(MIN_REAL, Min(MAX_REAL, MIN_REAL), 0.01);
end;

{$endif}

procedure DumpStack;
var
	p:pointer;
begin
	p:=get_caller_frame(get_frame);
	p:=get_caller_frame(p);
	Dump_Stack(output,p);
end;

{$ifdef TEST}
begin
	Suite.Add(@Test_CircInt);
	Suite.Add(@Test_MinMax);
	Suite.Add(@Test_Q);
	Suite.Add(@Test_Stack32_Push);
	Suite.Add(@Test_Stack32_Pop);
	Suite.Add(@Test_Stack32_Peek);
	Suite.Add(@Test_StringEndsWith);
	Suite.Add(@Test_MakeXYPair);
	Suite.Add(@Test_XYPairWrapper);
	Suite.Add(@Test_ExtractFileFromPath);
	Suite.Add(@Test_IsEmptyString_ansistring);
	Suite.Add(@test_StringToText);
	Suite.Add(@test_NumberToString);
	Suite.Add(@test_ForceOddUp);
	Suite.Run('std');
{$endif}
end.
