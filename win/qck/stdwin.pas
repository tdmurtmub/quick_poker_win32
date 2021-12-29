{ (C) 2006 Wesley Steiner }

{$MODE FPC}

unit stdwin;

interface

const
	CM_REDEALHAND			=204;
	CM_OPTIONSSOUND			=205;
	CM_TABLETOP				=207;
	CM_TABLETOP_ANIMATION	=208;
	CM_UNDOLASTMOVE			=230;
	CM_BASE					=240;
	CM_SCOREPAD				=241;
	CM_RULES				=243;
	CM_EXIT					=246;
	CM_FILENEW				=247;
	CM_VARIATION			=250;
	CM_NEXT					=250;
	CM_HELPABOUT			=909;

implementation

uses
	punit;

{$ifdef TEST}
procedure Test_unit;
begin
	AssertAreEqual(204,CM_REDEALHAND);
	AssertAreEqual(205,CM_OPTIONSSOUND);
	AssertAreEqual(207,CM_TABLETOP);
	AssertAreEqual(208,CM_TABLETOP_ANIMATION);
	AssertAreEqual(230,CM_UNDOLASTMOVE);
	AssertAreEqual(246,CM_EXIT);
	AssertAreEqual(247,CM_FILENEW);
	AssertAreEqual(250,CM_VARIATION);
	AssertAreEqual(909,CM_HELPABOUT);
end;
{$endif}

begin
	{$ifdef TEST}
	Suite.Add(@Test_unit);
	Suite.Run('stdwin');
	{$endif}
end.
