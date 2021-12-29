{ (C) 2011 Wesley Steiner }

{$MODE FPC}

unit winqcktbltestable;

interface

uses
	std,
	winqcktbl;
	
type
	testable_OHotspotP=^testable_OHotspot;
	testable_OHotspot=object(Hotspot)
		constructor Construct;
		constructor Construct(cx,cy:number);
		constructor Construct(x,y:integer;w,h:number);
	end;

	FakeHotspotP=^FakeHotspot;
	FakeHotspot=object(testable_OHotspot)
		OnPressed_state:boolean;
		OnCapturedRelease_was_called:boolean;
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
		function OnPressed(dx,dy:integer):boolean; virtual;
		function OnCapturedRelease(dx,dy:integer):boolean; virtual;
		procedure SetPosition(aPosition:xypair); virtual;
	end;

var
	GetAnchorPoint_call_count:word;
	GetAnchorPoint_wd_arg:word;
	GetAnchorPoint_ht_arg:word;

implementation

constructor testable_OHotspot.Construct;
begin
	Enable;
end;

constructor testable_OHotspot.Construct(cx,cy:number);
begin
	Anchor.x:=0;
	Anchor.y:=0;
	Enable;
end;

constructor testable_OHotspot.Construct(x,y:integer;w,h:number);
begin
	Anchor.x:=x;
	Anchor.y:=y;
	Enable;
end;

function FakeHotspot.GetAnchorPoint(table_width,table_height:word):xypair;
begin
	Inc(GetAnchorPoint_call_count);
	GetAnchorPoint_wd_arg:= table_width;
	GetAnchorPoint_ht_arg:= table_height;
	GetAnchorPoint:=MakeXYPair(self.Anchor.x, self.Anchor.y);
end;

function FakeHotspot.OnPressed(dx,dy:integer):boolean;

begin //writeln('FakeHotspot.OnPressed(',dx,',',dy,')');
	OnPressed:=OnPressed_state;
end;

function FakeHotspot.OnCapturedRelease(dx,dy:integer):boolean;
begin
	OnCapturedRelease_was_called:=TRUE;
	OnCapturedRelease:=FALSE;
end;

procedure FakeHotspot.SetPosition(aPosition:xypair); 
begin 
end;

end.
