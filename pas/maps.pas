{ (C) 2006 Wesley Steiner }

unit maps;

interface

const
	MAP_MAX_ENTRIES=50;

type
	IntToIntMap=object
		constructor Construct;
		function Count:word;
		function FindKey(aKey:integer):word;
		function HasKey(aKey:integer):boolean;
		function Lookup(aKey:integer):integer;
		procedure Add(aKey,aValue:integer);
	private
		myCount:word;
		myData:array[1..MAP_MAX_ENTRIES] of record myKey,myValue:integer; end;
	end;

implementation

{$ifdef TEST} uses punit; {$endif}

constructor IntToIntMap.Construct;

begin
	myCount:=0;
end;

function IntToIntMap.Count:word;

begin
	Count:=myCount;
end;

function IntToIntMap.FindKey(aKey:integer):word;

var
	i:word;

begin
	for i:=1 to Count do if myData[i].myKey=aKey then begin
		FindKey:=i;
		Exit;
	end;
	FindKey:=0;
end;

function IntToIntMap.HasKey(aKey:integer):boolean;

begin
	HasKey:=FindKey(aKey)<>0;
end;

procedure IntToIntMap.Add(aKey,aValue:integer);

begin
	if HasKey(aKey) then begin
		myData[FindKey(aKey)].myValue:=aValue;
	end
	else begin
		with myData[myCount+1] do begin
			myKey:=aKey;
			myValue:=aValue;
		end;
		Inc(myCount);
	end;
end;

function IntToIntMap.Lookup(aKey:integer):integer;

var
	i:word;

begin
	for i:=1 to Count do if myData[i].myKey=aKey then begin
		Lookup:=myData[i].myValue;
		Exit;
	end;
	RunError;
end;

{$ifdef TEST}

procedure Test_IntToInt; 

var
	map:IntToIntMap;

begin
	map.Construct;
	map.Add(123,456);
	punit.Assert.Equal(1,map.Count);
	map.Add(987,654);
	punit.Assert.Equal(654,map.Lookup(987));
	map.Add(123,-456);
	punit.Assert.Equal(2,map.Count);
	punit.Assert.Equal(-456,map.Lookup(123));
end;

{$endif TEST}
begin
	{$ifdef TEST}
	Suite.Add(Test_IntToInt);
	Suite.Run('maps');
	{$endif TEST}
end.
