{ (C) 2011 Wesley Steiner }

{$MODE FPC}

unit mathxTests;

interface

implementation

uses 
	punit,
	mathx;

procedure TestFactorial; 
begin
	punit.Assert.EqualLong(1, Factorial(0));
	punit.Assert.EqualLong(1, Factorial(1));
	punit.Assert.EqualLong(2, Factorial(2));
	punit.Assert.EqualLong(6, Factorial(3));
	punit.Assert.EqualLong(24, Factorial(4));
end;

procedure TestSpanOfNThingsSeparatedBySpace;
begin
	Assert.AreEqual(0,SpanOf(0,1,2));
	Assert.AreEqual(10,SpanOf(1,10,5));
	Assert.AreEqual(25,SpanOf(2,10,5));
end;

procedure TestOffsetOfTheIthThingSeparatedBySpace;
begin
	Assert.AreEqual(0,SpanOffset(0,1,2));
	Assert.AreEqual(15,SpanOffset(1,10,5));
	Assert.AreEqual(30,SpanOffset(2,10,5));
end;

begin
	Suite.Add(@TestFactorial);
	Suite.Add(@TestSpanOfNThingsSeparatedBySpace);
	Suite.Add(@TestOffsetOfTheIthThingSeparatedBySpace);
	Suite.Run('mathxTests');
end.
