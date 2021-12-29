{ (C) 2008 Wesley Steiner }

{$MODE FPC}

unit xy;

interface

uses std;

function Intersection(L1X1, L1Y1, L1X2, L1Y2, L2X1, L2Y1, L2X2, L2Y2:integer):LongInt;
function LineOverlap(l1p1,l1p2,l2p1,l2p2:integer):boolean;
function OverlapBlocks(b1x1,b1y1,b1x2,b1y2,b2x1,b2y1,b2x2,b2y2:integer):boolean;
function MidPoint(X1, Y1, X2, Y2:integer):LongInt;
function OverlapArea(b1x1,b1y1,b1x2,b1y2,b2x1,b2y1,b2x2,b2y2:integer):integer;

implementation

function blockOverlap(b1x1,b1y1,b1x2,b1y2,b2x1,b2y1,b2x2,b2y2:integer):boolean;
{ Return true if block "b1" overlaps block "b2". }
begin
	blockOverlap:=lineOverlap(b1x1,b1x2,b2x1,b2x2) and lineOverlap(b1y1,b1y2,b2y1,b2y2);
end;

function LineOverlap(l1p1,l1p2,l2p1,l2p2:integer):boolean;
// Returns true if line 1 overlaps line 2.
begin
	lineOverlap:=((not (l2p1>l1p2)) and (not (l2p2<l1p1)));
end;

function overlapArea(b1x1,b1y1,b1x2,b1y2,b2x1,b2y1,b2x2,b2y2:integer):integer;
{ Return the area (in pixels) of block "b1" that overlaps block "b2". }
begin
	if blockOverlap(b1x1,b1y1,b1x2,b1y2,b2x1,b2y1,b2x2,b2y2) then
			overlapArea:=abs(min(b2x2,b1x2)-max(b2x1,b1x1))*abs(min(b2y2,b1y2)-max(b2y1,b1y1))
		else
			overlapArea:=0;
end;

function Intersection(L1X1, L1Y1, L1X2, L1Y2, L2X1, L2Y1, L2X2, L2Y2:integer):LongInt;
var
	l:LongInt;
begin
	l := L1Y1;
	Intersection:= (l shl 16) + L1X1;
end;

function MidPoint(X1, Y1, X2, Y2:integer):LongInt;
var
	RX1, RY1, RX2, RY2:Real;
begin
	RX1:= X1;
	RY1:= Y1;
	RX2:= X2;
	RY2:= Y2;
	MidPoint:=(Round(RY1 + (RY2 - RY1) / 2) shl 16)+Round(RX1 + (RX2 - RX1) / 2);
end;

function OverlapBlocks(b1x1,b1y1,b1x2,b1y2,b2x1,b2y1,b2x2,b2y2:integer):boolean;
{ Returns true if block 2 overlaps block 1. }
begin
	overlapBlocks:=lineOverlap(b2x1,b2x2,b1x1,b1x2) and lineOverlap(b2y1,b2y2,b1y1,b1y2);
end;

end.
