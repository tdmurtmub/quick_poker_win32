{ (C) 2006 Wesley Steiner }

{$MODE FPC}

unit mathx;

interface

function Factorial(n:word):longint;
function SpanOf(n:word;length,spacing:integer):integer;
function SpanOffset(ith:word;length,spacing:integer):integer;

implementation

{$ifdef TEST} uses mathxTests; {$endif}

function Factorial(n:word):longint;

var
	f:longInt;

begin
	if n=0 then n:=1;
	f:=1;
	while n>1 do begin
		f:=f*n;
		dec(n);
	end;
	factorial:=f;
end;

function SpanOf(n:word;length,spacing:integer):integer;
begin
	if n=0 then SpanOf:=0 else SpanOf:=n*(length+spacing)-spacing;
end;

function SpanOffset(ith:word;length,spacing:integer):integer;
begin
	SpanOffset:=(length+spacing)*ith;
end;

end.
