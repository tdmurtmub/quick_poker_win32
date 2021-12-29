{  (C) 2009 Wesley Steiner }

{$MODE FPC}

unit quickTests;

{$I platform}
{$I punit.inc}

interface

uses
	qcktbl,
	quick;
	
type
	testable_OApplication=object(Application)
		constructor Construct;
		function GetBooleanData(aKey,aSubKey:pchar;aDefaultValue:boolean):boolean; virtual;
		function GetIntegerData(aKey,aSubKey:pchar;aDefaultValue:longint):longint; virtual;
		function GetIntegerDataRange(aKey,aSubKey:pchar;aLowValue,aHighValue,aDefaultValue:longint):longint; virtual;
		function GetStringData(aKey,aSubKey:pchar;const aDefaultValue:ansistring):ansistring; virtual;
		procedure SetBooleanData(aKey,aSubKey:pchar;aValue:boolean); virtual;
		procedure SetIntegerData(aKey,aSubKey:pchar;aValue:longint); virtual;
	end;

implementation

uses
	punit,
	qcktbl_tests;
		
procedure Test_backwards_compatibility;
begin
	AssertAreEqual('Tabletop',KEY_TABLETOP);
	AssertAreEqual('Animation',KEY_TABLETOP_ANIMATION);
	AssertAreEqual('ImagePath',KEY_TABLETOP_IMAGEPATH);
	AssertAreEqual('UseImage',KEY_TABLETOP_USEIMAGE);
	AssertAreEqual('ShowVariationDialog',KEY_SHOWVARIATIONDIALOG);
end;

constructor testable_OApplication.Construct; begin end;
function testable_OApplication.GetBooleanData(aKey,aSubKey:pchar;aDefaultValue:boolean):boolean; begin GetBooleanData:=FALSE; end;
function testable_OApplication.GetIntegerData(aKey,aSubKey:pchar;aDefaultValue:longint):longint; begin GetIntegerData:=0; end;
function testable_OApplication.GetIntegerDataRange(aKey,aSubKey:pchar;aLowValue,aHighValue,aDefaultValue:longint):longint; begin GetIntegerDataRange:=0; end;
function testable_OApplication.GetStringData(aKey,aSubKey:pchar;const aDefaultValue:ansistring):ansistring; begin GetStringData:=''; end;
procedure testable_OApplication.SetBooleanData(aKey,aSubKey:pchar;aValue:boolean); begin end;
procedure testable_OApplication.SetIntegerData(aKey,aSubKey:pchar;aValue:longint); begin end;

type
	ApplicationProxy=object(testable_OApplication)
		arg_key,arg_subkey:string;
		arg_value:longint;
		constructor Construct;
		procedure SetIntegerData(aKey,aSubKey:pchar;aValue:longint); virtual;
	end;

constructor ApplicationProxy.Construct; 
begin 
end;

procedure ApplicationProxy.SetIntegerData(aKey,aSubKey:pchar;aValue:longint);
begin
	arg_key:=StrPas(aKey);
	arg_subkey:=StrPas(aSubKey);
	arg_value:=aValue;
end;

procedure test_PersistVariationSelection;
var
	app:ApplicationProxy;
begin
	app.Construct;
	app.PersistVariationSelection(35,123);
	punit.Assert.EqualStr('Game-35',app.arg_key);
	punit.Assert.EqualStr(KEY_VARIATION,app.arg_subkey);
	punit.Assert.Equal(123,app.arg_value);
end;

type
	ChooseVariationTester=object(ApplicationProxy)
		ShowVariationDialog_result:boolean;
		SelectGameVariation_result:variationIndex;
		function SelectGameVariation:variationIndex; virtual;
		function ShowVariationDialog:boolean; virtual;
		procedure PersistVariationSelection(aGameId:gameIndex;n:variationIndex); virtual;
	end;

procedure ChooseVariationTester.PersistVariationSelection(aGameId:gameIndex;n:variationIndex); begin end;
function ChooseVariationTester.ShowVariationDialog:boolean; begin ShowVariationDialog:=ShowVariationDialog_result; end;

function ChooseVariationTester.SelectGameVariation:variationIndex; 
begin 
	SelectGameVariation:=SelectGameVariation_result; 
end;

type
	FakeGame=object(GameStub)
		myTitle:pchar;
		VariationCount_result:word;
		VariationName_result:pchar;
		constructor Construct;
		function PackCount:word; virtual;
		function Title:pchar; virtual;
		function VariationName(n:variationIndex):pchar; virtual;
		function VariationCount:variationIndex; virtual;
	end;

constructor FakeGame.Construct; begin end;
function FakeGame.VariationName(n:variationIndex):pchar; begin VariationName:=VariationName_result; end;
function FakeGame.Title:pchar; begin Title:=myTitle; end;
function FakeGame.VariationCount:variationIndex; begin VariationCount:=VariationCount_result; end;
function FakeGame.PackCount:word; begin PackCount:=1; end;

procedure test_ChooseVariation;
var
	testee:ChooseVariationTester;
	fake_game:FakeGame;
begin
	testee.Construct;
	fake_game.Construct;
	testee.SetCurrentGame(@fake_game);
	fake_game.VariationCount_result:=5;
	fake_game.SetVariation(1);
	testee.SelectGameVariation_result:=2;
	AssertIsTrue(testee.ChooseVariation);
	fake_game.SetVariation(3);
	testee.SelectGameVariation_result:=3;
	AssertIsFalse(testee.ChooseVariation);
end;

begin
	Suite.Add(@test_backwards_compatibility);
	Suite.Add(@test_PersistVariationSelection);
	Suite.Add(@test_ChooseVariation);
	Suite.Run('quick');
end.
