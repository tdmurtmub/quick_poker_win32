{ (C) 1991 Wesley Steiner }

interface

function DegToRad(a:Real):Real;
function RadToDeg(a:Real):Real;
function Deg2Rad(a:Real):Real;

implementation

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

end.
