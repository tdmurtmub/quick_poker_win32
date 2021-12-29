{ (C) 2012 Wesley Steiner }

{$MODE FPC}

unit winTabletopChipsTests;

interface

{$I punit.inc}

implementation 

uses
	windows,
	punit,
	casino,
	winqcktbl,
	winTabletopChips;
	
type
	testable_OOChipstackProp = object(OChipstackProp)
		clank_sound_was_called:boolean;
		constructor Construct;
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure Hide; virtual;
		procedure Refresh; virtual;
		procedure Show; virtual;
	private
		procedure ClankSound; virtual;
	end;

constructor testable_OOChipstackProp.Construct;
begin
	TheChips:= New(PPileOfChips,Init(10));
end;

procedure testable_OOChipstackProp.Refresh; begin end;
procedure testable_OOChipstackProp.Hide; begin end;
procedure testable_OOChipstackProp.Show; begin end;
procedure testable_OOChipstackProp.GetSpanRect(var rRect:TRect); begin end;
procedure testable_OOChipstackProp.ClankSound; begin clank_sound_was_called:=true; end;

procedure TestChipStackPop;
var
	aStack:testable_OOChipstackProp;
	aChipType:TypeOfChip;
begin
	aStack.Construct;
	aStack.TheChips^.AddAmount(6);
	aChipType:= aStack.Pop;
	punit.Assert.Equal(5, Round(aStack.Value));
	punit.Assert.Equal(Ord(CHIP2), Ord(aChipType));
end;

procedure Test_chip_stack_SnapTo;
var
	source,target:testable_OOChipstackProp;
begin
	source.Construct;
	source.Push(CHIP3);
	source.Push(CHIP2);
	source.Push(CHIP2);
	source.Push(CHIP2);
	source.Push(CHIP2);
	source.Push(CHIP2);
	source.Push(CHIP2);
	target.Construct;
	target.Push(CHIP4);
	source.SnapTo(@target);
	punit.Assert.Equal(0, source.ChipCount);
	punit.Assert.Equal(8, target.ChipCount);
	punit.Assert.AreEqualReal(36.0, target.Value, 0.1);
end;

procedure Test_ChipStack_SnapTo_sound_effects;
var
	source,target:testable_OOChipstackProp;
begin
	source.Construct;
	source.Push(CHIP3);
	target.Construct;
	target.Push(CHIP2);
	source.clank_sound_was_called:=false;
	source.SnapTo(@target);
	punit.Assert.IsTrue(source.clank_sound_was_called);
end;

procedure Test_ChipStack_SnapTo_sound_effects_with_empty_source;
var
	source,target:testable_OOChipstackProp;
begin
	source.Construct;
	target.Construct;
	target.Push(CHIP2);
	source.clank_sound_was_called:=false;
	source.SnapTo(@target);
	punit.Assert.IsFalse(source.clank_sound_was_called);
end;

procedure Test_ChipStack_SnapTo_sound_effects_with_empty_target;
var
	source,target:testable_OOChipstackProp;
begin
	source.Construct;
	source.Push(CHIP2);
	target.Construct;
	source.clank_sound_was_called:=false;
	source.SnapTo(@target);
	punit.Assert.IsFalse(source.clank_sound_was_called);
end;

procedure Test_chip_stack_Push;
var
	tester:testable_OOChipstackProp;
begin
	tester.Construct;
	tester.Push(CHIP3);
	punit.Assert.Equal(1, tester.ChipCount);
	punit.Assert.Equal(Ord(CHIP3), Ord(tester.Pop));
end;

procedure Test_chip_stack_Push_sound_effects;
var
	tester:testable_OOChipstackProp;
begin
	tester.Construct;
	tester.Push(CHIP3);
	tester.clank_sound_was_called:=false;
	tester.Push(CHIP2);
	punit.Assert.IsTrue(tester.clank_sound_was_called);
end;

procedure Test_chip_stack_Push_sound_effects_when_empty;
var
	tester:testable_OOChipstackProp;
begin
	tester.Construct;
	tester.clank_sound_was_called:=false;
	tester.Push(CHIP2);
	punit.Assert.IsFalse(tester.clank_sound_was_called);
end;

type
	testable_OChipbundleProp = object(OChipbundleProp)
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure Refresh; virtual;
	end;

procedure testable_OChipbundleProp.GetSpanRect(var rRect:TRect); begin end;
procedure testable_OChipbundleProp.Refresh; begin end;

procedure Test_ChipPile_AddChips;
var
	aPile:testable_OChipbundleProp;
begin
	aPile.Init(New(PBundleOfChips, Init(0.0)));
	aPile.AddChips(CHIP2, 0);
	punit.Assert.AreEqualReal(0.0, aPile.TheBundle^.DollarValue, 0.1);
	aPile.AddChips(CHIP2, 1);
	punit.Assert.AreEqualReal(1.0, aPile.TheBundle^.DollarValue, 0.1);
	aPile.AddChips(CHIP3, 1);
	punit.Assert.AreEqualReal(6.0, aPile.TheBundle^.DollarValue, 0.1);
	aPile.AddChips(CHIP2, 2);
	punit.Assert.AreEqualReal(8.0, aPile.TheBundle^.DollarValue, 0.1);
end;

procedure TestOnChipsAddedToPile;

var
	prop:testable_OChipbundleProp;

begin
	prop.Init(New(PBundleOfChips, Init(0.0)));
	prop.TheBundle^.Stacks[CHIP2]^.AddChips(PILE_STACK_LIMIT-1);
	prop.OnChipsAdded(CHIP2, 0);
	punit.Assert.Equal(PILE_STACK_LIMIT-1, prop.TheBundle^.Stacks[CHIP2]^.Size);
	prop.TheBundle^.Stacks[CHIP2]^.AddChips(1);
	prop.OnChipsAdded(CHIP2, 0);
	punit.Assert.Equal(PILE_STACK_LIMIT, prop.TheBundle^.Stacks[CHIP2]^.Size);
	prop.TheBundle^.Stacks[CHIP2]^.AddChips(1);
	prop.OnChipsAdded(CHIP2, 0);
	punit.Assert.Equal((PILE_STACK_LIMIT+1) mod 5, prop.TheBundle^.Stacks[CHIP2]^.Size);
end;

procedure TestPopChipsFromPile;
var
	prop:testable_OChipbundleProp;
begin
	prop.Init(New(PBundleOfChips, Init(18.0)));
	punit.Assert.Equal(0,prop.PopUnits(0));
	punit.Assert.Equal(4,prop.PopUnits(4));
	punit.Assert.AreEqualReal(14.0,prop.Value,0.0);
	punit.Assert.Equal(14,prop.PopUnits(15));
end;

procedure TestPopChipFromPile;
var
	prop:testable_OChipbundleProp;
begin
	prop.Init(New(PBundleOfChips, Init(1.0)));
	punit.Assert.Equal(Ord(CHIP2),Ord(prop.PopChip(CHIP2)));
	punit.Assert.Equal(0,prop.TheBundle^.Stacks[CHIP2]^.Size);
end;

procedure TestIsPileStackEmpty;
var
	prop:testable_OChipbundleProp;
begin
	prop.Init(New(PBundleOfChips, Init(1.0)));
	punit.assert.IsFalse(prop.IsStackEmpty(CHIP2));
	punit.assert.IsTrue(prop.IsStackEmpty(CHIP3));
end;

procedure TestChipStackPushChips;
var
	prop:testable_OOChipstackProp;
begin
	prop.Construct;
	prop.PushChips(CHIP2,0);
	punit.Assert.Equal(0,prop.ChipCount);
	prop.PushChips(CHIP3,2);
	punit.Assert.Equal(2, prop.ChipCount);
	punit.Assert.Equal(Ord(CHIP3),Ord(prop.Pop));
end;

procedure wart_updates_content_from_value;
var
	prop:testable_OOChipstackProp;
	wart:OValueWart;
begin
	prop.Construct;
	prop.PushChips(CHIP2, 10);
	wart.Construct(@prop);
	prop.AddWart(@wart);
	AssertAreEqual('10', wart.GetContent);
	prop.PushChips(CHIP1, 1);
	AssertAreEqual('10.5', wart.GetContent);
end;

begin
	Suite.Add(@TestChipStackPop);
	Suite.Add(@Test_ChipPile_AddChips);
	Suite.Add(@TestOnChipsAddedToPile);
	Suite.Add(@TestPopChipsFromPile);
	Suite.Add(@TestPopChipFromPile);
	Suite.Add(@TestIsPileStackEmpty);
	Suite.Add(@TestChipStackPushChips);
	Suite.Add(@Test_chip_stack_SnapTo);
	Suite.Add(@Test_ChipStack_SnapTo_sound_effects);
	Suite.Add(@Test_ChipStack_SnapTo_sound_effects_with_empty_source);
	Suite.Add(@Test_ChipStack_SnapTo_sound_effects_with_empty_target);
	Suite.Add(@Test_chip_stack_Push);
	Suite.Add(@Test_chip_stack_Push_sound_effects);
	Suite.Add(@Test_chip_stack_Push_sound_effects_when_empty);
	Suite.Add(@wart_updates_content_from_value);
	Suite.Run('winTabletopChipsTests');
end.
