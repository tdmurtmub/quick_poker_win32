{ (C) 2011 Wesley Steiner }

{$MODE FPC}

unit qcktbl_tests;

interface

uses
	qcktbl;

type
	GameStub=object(Game)
		constructor Construct;
	end;

implementation

uses
	std,punit;
	
type
	GameTester=object(GameStub)
		VariationCount_result:variationIndex;
		function VariationCount:variationIndex; virtual;
	end;

constructor GameStub.Construct; begin end;

function GameTester.VariationCount:variationIndex; begin VariationCount:=VariationCount_result; end;

procedure Test_SetVariation;
var
	testee:GameTester;
begin
	testee.Construct;
	testee.VariationCount_result:=25;
	testee.SetVariation(25);
	punit.Assert.Equal(25,testee.Variation);
	testee.SetVariation(26);
	punit.Assert.Equal(0,testee.Variation);
end;

procedure PropConstruction;
var
	prop:OProp;
begin
	prop.Construct;
	AssertIsTrue(prop.IsVisible);
	prop.Construct(FALSE);
	AssertIsFalse(prop.IsVisible);
end;

begin
	Suite.Add(@PropConstruction);
	Suite.Add(@test_SetVariation);
	Suite.Run('qcktbl_tests');
end.
