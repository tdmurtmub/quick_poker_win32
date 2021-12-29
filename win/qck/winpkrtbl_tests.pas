{ (C) 2012 Wesley Steiner }

{$MODE FPC}

unit winpkrtbl_tests;

{$I platform}
{$I punit.inc}

interface

implementation

uses
	punit,
	winpkrtbl;
	
procedure BackwardCompatibility;
begin
	AssertAreEqual(250,CM_OPTIONSDRAW);
	AssertAreEqual(251,CM_OPTIONSSTUD);
	AssertAreEqual(252,CM_GAMEDRAW);
	AssertAreEqual(253,CM_GAMESTUD);
	AssertAreEqual(254,CM_STATS);	
end;

begin
	Suite.Add(@BackwardCompatibility);
	Suite.Run('winpkrtbl_tests');
end.