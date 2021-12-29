{ (C) 2004 Wesley Steiner }

{ NOTE: This unit must be completely self-contained. It must not use any non-system units. }

{$MODE FPC}

unit punit;

interface

type
	Asserter=object // OBSOLETE: use Assert* procedures
		procedure Pass;
		procedure Fail;
		procedure IsTrue(aFlag:boolean);
		procedure IsFalse(aFlag:boolean);
		procedure AreEqual(expected,was:integer);
		procedure AreEqual(expected,was:longint);
		procedure AreEqual(expected,was:word);
		procedure AreEqual(expected,was:longword);
		procedure AreEqual(expected,was,tolerance:real);
		procedure AreEqual(expected,was:string);
		procedure AreEqualLong(expected,was:longint);
		procedure AreEqualWord(expected,was:word);
		procedure AreEqualLongWord(expected,was:Longword);
		procedure EqualLong(expected,was:longint);
		procedure EqualChar(expected,was:char);
		procedure EqualPtr(expected,was:pointer);
		procedure EqualText(expected,was:pchar);
		procedure AreEqualReal(expected,was,tolerance:real);
		procedure EqualStrings(expected,was:string);
		procedure NotNull(value:longword);
		procedure Equal(expected,was:longint); // obsolete
		procedure EqualStr(expected,was:string); // obsolete
	private
		procedure PreFail;
		procedure PostFail(depth:word);
	end;

	CallTelemetry=record
		WasCalled:boolean;
	end;

	TestProc=procedure;
	TestProc1=procedure(p:pointer);

type
	testProcArray=array[0..32767] of TestProc;
	testQueue=object 
		constructor Construct;
		destructor Destruct;
	private
		mySize:word;
		myTestProcQ:^testProcArray;
		function Size:word;
		procedure Clear;
		procedure Push(aTestProc:TestProc);
	end;

	TestSuite=object
		constructor Construct;
		procedure Add(aTestProc:TestProc);
		procedure Run(const aName:string);
	private
		myTestQueue:TestQueue;
		myCurrTestProc:TestProc;
	end;

	Fixture=object
	end;

var
	Assert:Asserter; // OBSOLETE: use Assert* procedures
	Suite:TestSuite;

procedure AssertAreEqual(expected,was:integer);
procedure AssertAreEqual(expected,was:longint);
procedure AssertAreEqual(expected,was:word);
procedure AssertAreEqual(expected,was:longword);
procedure AssertAreEqual(expected,was,tolerance:real);
procedure AssertAreEqual(expected,was:string);
procedure AssertAreEqual(expected,was:pchar);
procedure AssertEndsWith(expected,was:string);
procedure AssertFail;
procedure AssertIsFalse(aFlag:boolean);
procedure AssertIsNotNil(value:pointer);
procedure AssertIsTrue(aFlag:boolean);
procedure AssertNotNull(value:longword);
procedure RunTest(aTestProc:TestProc1; p:pointer);

implementation

uses
	Strings, WinCrt;

constructor TestSuite.Construct;
begin
	myTestQueue.Construct;
end;

const
	theQueueSize:word=25;

constructor TestQueue.Construct;
begin
	myTestProcQ:=Getmem(theQueueSize*sizeof(Testproc));
	Clear;
end;

destructor TestQueue.Destruct;
begin
	Freemem(myTestProcQ);
end;

procedure TestQueue.Push(aTestProc:TestProc);
begin
	if (Size=theQueueSize) then begin
		theQueueSize:=Word(theQueueSize*2);
		myTestProcQ:=ReAllocMem(myTestProcQ,theQueueSize*sizeof(Testproc));
	end;
	myTestProcQ^[mySize]:=aTestProc;
	inc(mySize);
end;

function TestQueue.Size:word;
begin
	Size:=mySize;
end;

procedure TestQueue.Clear;
begin
	mySize:=0;
end;

const
	theTotalTestCounter:word=0;
	theTotalFailedCounter:word=0;
	theTestCounter:word=0;

var
	theFailedCounter:word;

procedure TestSuite.Run(const aName:string);
var
	iTest:word;
begin
	Writeln(aName);
	theFailedCounter:=0;
	if myTestQueue.Size>0 then begin
		for iTest:=0 to Word(myTestQueue.Size-1) do begin
			theTestCounter:=Word(iTest+1);
			myCurrTestProc:=myTestQueue.myTestProcQ^[iTest];
			myCurrTestProc; { invoke the procedure }
			Write('.');
		end;
	end;
	Inc(theTotalTestCounter, myTestQueue.Size);
	Writeln;
	Writeln(myTestQueue.Size,' Tests, ',theTotalTestCounter,' Total');
	Writeln;
	myTestQueue.Clear;
end;

procedure RunTest(aTestProc:TestProc1; p:pointer);
begin
	aTestProc(p); { invoke the procedure }
	Write('.');
	Inc(theTotalTestCounter);
end;

procedure WriteHexWord(w:Word);
const
  hexChars:array [0..$F] of Char = '0123456789ABCDEF';
begin
  Write(hexChars[Hi(w) shr 4],
		  hexChars[Hi(w) and $F],
		  hexChars[Lo(w) shr 4],
		  hexChars[Lo(w) and $F]);
end;

procedure Asserter.Fail;
begin
	PreFail;
	PostFail(1);
end;

procedure Asserter.PostFail(depth:word);
var
	p:pointer;
begin
	Inc(theFailedCounter);
	Inc(theTotalFailedCounter,theFailedCounter);
	Write('Unit Test #',theTestCounter,' Failed @ ');
	p:=@Suite.myCurrTestProc;
	Writeln(hexStr(p));
	p:=get_caller_frame(get_frame);
	while depth>0 do begin
		p:=get_caller_frame(p);
		Dec(depth);
	end;
	Dump_Stack(output,p);
	Halt(1);
end;

procedure Asserter.Pass;
begin
end;

procedure Asserter.IsTrue(aFlag:boolean);
begin
	if (aFlag) then Pass else Fail;
end;

procedure Asserter.IsFalse(aFlag:boolean);
begin
	if (not aFlag) then Pass else Fail;
end;

procedure Asserter.NotNull(value:longword);
begin
	if (value<>0) then Pass else Fail;
end;

procedure Asserter.AreEqual(expected,was:integer);
begin
	AreEqualLong(expected,was);
end;

procedure Asserter.AreEqual(expected,was:word);
begin
	AreEqualWord(expected,was);	
end;

procedure Asserter.AreEqual(expected,was:longword);
begin
	AreEqualLongWord(expected,was);	
end;

procedure Asserter.Equal(expected,was:longint);
begin
	AreEqual(expected,was);
end;

procedure Asserter.AreEqual(expected,was:longint);

begin
	AreEqualLong(expected,was);
end;

procedure Asserter.AreEqual(expected,was:string);

begin
	EqualStrings(expected,was);
end;

procedure AssertEndsWith(expected,was:string);

begin
	Assert.EqualStrings(expected,Copy(was,Length(was)-Length(expected)+1,Length(expected)));
end;

procedure Asserter.AreEqualLong(expected,was:longint);

begin
	if was = expected then begin
		Pass;
	end
	else begin
		PreFail;
		Writeln('Expected ', expected, ' but was ', was);
		PostFail(1);
	end;
end;

procedure Asserter.EqualText(expected,was:pchar);

begin
	if StrComp(was, expected) = 0 then begin
		Pass;
	end
	else begin
		PreFail;
		Writeln('Expected "', expected, '"');
		Writeln(' but was "', was, '"');
		PostFail(1);
	end;
end;

procedure Asserter.EqualStrings(expected,was:string);

begin
	if was = expected then begin
		Pass;
	end
	else begin
		PreFail;
		Writeln('Expected "', expected, '"');
		Writeln(' but was "', was, '"');
		PostFail(1);
	end;
end;

procedure Asserter.EqualStr(expected, was:string);

begin
	EqualStrings(expected,was);
end;

procedure TestSuite.Add(aTestProc:TestProc);

begin
	myTestQueue.Push(aTestProc);
end;

procedure Asserter.EqualPtr(expected, was:pointer);

begin
	if was = expected then begin
		Pass;
	end
	else begin
		PreFail;
		Write('Expected ');
		WriteHexWord(Word(Seg(expected)));
		Write(':');
		WriteHexWord(Word(Ofs(expected)));
		Write(' but was ');
		WriteHexWord(Word(Seg(was)));
		Write(':');
		WriteHexWord(Word(Ofs(was)));
		WriteLn;
		PostFail(1);
	end;
end;

procedure Asserter.EqualLong(expected, was:longint);
begin
	if was = expected then begin
		Pass;
	end
	else begin
		PreFail;
		Writeln('Expected ', expected, ' but was ', was);
		PostFail(1);
	end;
end;

procedure Asserter.AreEqualWord(expected, was:word);
begin
	if was = expected then begin
		Pass;
	end
	else begin
		PreFail;
		Writeln('Expected ',expected,' but was ',was);
		PostFail(1);
	end;
end;

procedure Asserter.AreEqualLongWord(expected,was:Longword);
begin
	if was = expected then begin
		Pass;
	end
	else begin
		PreFail;
		Writeln('Expected ',expected,' but was ',was);
		PostFail(1);
	end;
end;

procedure Asserter.EqualChar(expected, was:char);
begin
	if was = expected then begin
		Pass;
	end
	else begin
		PreFail;
		Writeln('Expected ''', expected, ''' but was ''', was, '''');
		PostFail(1);
	end;
end;

procedure Asserter.AreEqualReal(expected,was,tolerance:real);

begin
	AreEqual(expected,was,tolerance);
end;

procedure Asserter.AreEqual(expected, was, tolerance:real);

begin
	if (was >= (expected-tolerance)) and (was<=(expected+tolerance)) 
		then Pass
		else begin
			PreFail;
			Writeln('Expected ''', expected:0:3, ''' but was ''', was:0:3, '''');
			PostFail(1);
		end;
end;

procedure Asserter.PreFail;

begin
	Write('F');
	Writeln;
end;

procedure AssertFail;

begin
	Assert.Fail;
end;

procedure AssertAreEqual(expected,was:integer); begin Assert.AreEqual(expected,was); end;
procedure AssertAreEqual(expected,was:longint); begin Assert.AreEqual(expected,was); end;
procedure AssertAreEqual(expected,was:word); begin Assert.AreEqual(expected,was); end;
procedure AssertAreEqual(expected,was:longword); begin Assert.AreEqual(expected,was); end;
procedure AssertAreEqual(expected,was,tolerance:real); begin Assert.AreEqualReal(expected,was,tolerance); end;
procedure AssertAreEqual(expected,was:string); begin Assert.AreEqual(expected,was); end;
procedure AssertAreEqual(expected,was:pchar); begin Assert.EqualText(expected,was); end;
procedure AssertIsFalse(aFlag:boolean); begin Assert.IsFalse(aFlag); end;
procedure AssertIsNotNil(value:pointer); begin Assert.NotNull(LongWord(value)); end;
procedure AssertIsTrue(aFlag:boolean); begin Assert.IsTrue(aFlag); end;
procedure AssertNotNull(value:longword); begin Assert.NotNull(value); end;

begin
	Suite.Construct;
end.
