{ (C) 2006 Wesley Steiner }

unit winTabletopChips;

{$MODE FPC}

interface

uses
	windows,
	winqcktbl,
	casino;

{$I punit.inc}

const
	PILE_STACK_LIMIT=12;

type
	OChipbundleProp_ptr=^OChipbundleProp;

	OChipsProp_ptr=^OChipsProp;
	OChipsProp=object(HotSpot)
		constructor Construct(w:word; valuewart_position:relativeposition);
		function Value:real; virtual; abstract;
		procedure OnValueChanged; virtual;
	private
		valuewart_position:relativeposition;
	end; 
	
	// A stack of chips of varying denominations. 
	// The anchor point is the top left corner of the bitmap for the bottom chip in the pile. }
	OChipstackProp_ptr=^OChipstackProp;
	OChipstackProp=object(OChipsProp)
		TheChips:PPileOfchips;
		constructor Init(aPile:PPileOfChips);
		function ChipCount:word;
		function Discard:real;
		function Value:real; virtual;
		function Pop:TypeOfChip;
		procedure AddAmount(Amount:Real);
		procedure Double;
		procedure Redraw(dc:HDC;aX,aY:integer); virtual;
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure MatchIt;
		procedure MinimizeChipCount;
		procedure PayBJ;
		procedure Push(aChip:TypeOfChip); virtual;
		procedure PushChips(aChip:TypeOfChip;aChipCount:word);
		procedure RemoveAmount(Amount:Real);
		procedure SplitToPile(TargetPile:OChipstackProp_ptr);
		procedure SnapTo(target:OChipstackProp_ptr);
		procedure TransferTo(target:OChipbundleProp_ptr);
	private
		procedure ClankSound; test_virtual
		procedure IncChips(toc:TypeOfChip; N:integer);
	end;

	OValueWart_ptr=^OValueWart;
	OValueWart=object(OPropwart)
		function GetContent:string; virtual;
		procedure Redraw(aDC:HDC; x,y:integer); virtual;
	end; 
	
	// A collection of chips, arranged in stacks by denomination.
	// The anchor point is the bottom left corner of the rectangle that contains all the chips.
	OChipbundleProp=object(OChipsProp)
		TheBundle:PBundleOfChips;
		constructor Init(aBundle:PBundleOfChips);
		destructor Done; virtual;
		function Discard:real;
		function IsStackEmpty(aChipType:TypeOfChip):boolean;
		function OnPressed(X:integer;Y:integer):boolean; virtual;
		function PopChip(aChipType:TypeOfChip):TypeOfChip; virtual;
		function PopUnits(nUnits:word):word; virtual;
		function Value:real; virtual;
		procedure AddChip(aChip:TypeOfChip);
		procedure AddChips(aChip:TypeOfChip; aCount:word);
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure MinimumChips;
		procedure OnChipsAdded(aChip:TypeOfChip;aCount:word);
		procedure OnStackClicked(aChipType:TypeOfChip); virtual;
		procedure Redraw(dc:HDC;aX,aY:integer); virtual;
		procedure SetAmount(Amt:Real); virtual;
	private
		function PtHitsChips(const aX:integer; const aY:integer; var ChipDenomination:TypeOfChip):boolean;
	end;

function ChipWidth:integer;
function ChipHeight:integer;

procedure ChipClankSound;

implementation

uses
	{$ifdef TEST} winTabletopChipsTests, {$endif}
	strings,mmsystem,
	sysutils,
	std,
	stringsx,
	gdiex,
	windowsx;

const
	CHIP_STACK_DY=4;
	bundle_pile_dx  = 23; { pixels of space between chip piles }
	bundle_pile_dy  = 23; { pixels of space between chip piles }

var
	hwChipWaggle:THandle;
	hwChipClank:array[1..4] of THandle;
	topChipMask:HBITMAP;
	topChipImage:array[TypeOfChip] of HBITMAP; { images of top view chips, there are four rotations of each chip }
	hwpChipClank:array[1..4] of Pointer;
	hwpChipWaggle:Pointer;

procedure LoadChipRsc;
var
	d:TypeOfChip;
begin
	topChipMask:=LoadBitmap(hInstance,PChar(100));
	for d:= Low(TypeOfChip) to High(TypeOfChip) do
		topChipImage[d]:=LoadBitmap(hInstance,PChar(101 + Ord(d)));
end;

procedure FreeChipRsc;
var
	d:TypeOfChip;
begin
	DeleteObject(topChipMask);
	for d:= Low(TypeOfChip) to High(TypeOfChip) do DeleteObject(topChipImage[d]);
end;

procedure OChipstackProp.MinimizeChipCount;
begin //writeln('OChipstackProp.MinimizeChipCount');
	theChips^.minimize;
	Refresh;
end;

procedure OChipstackProp.ReDraw(dc:HDC;aX,aY:integer);
var
	i:integer;
	y:integer;
begin //writeln('OChipstackProp.ReDraw(dc:HDC;aX,aY:integer);');
	inherited Redraw(dc,aX,aY);
	if (not IsVisible) then Exit;
	y:= aY;
	for i:= 0 to TheChips^.count - 1 do begin
		PutBitmap(DC, topChipMask, aX, y, SrcAnd);
		PutBitmap(DC, topChipImage[PCasinoChip(TheChips^.At(i))^.Denomination], aX, y, SrcPaint);
		y:= y - CHIP_STACK_DY;
	end;
end;

procedure PlayClankSound(i:integer);
begin
	if x_SoundStatus then sndPlaySound(hwpChipClank[i],Snd_NoDefault or Snd_Memory or SND_ASYNC);
end;

procedure ChipClankSound;
begin
	PlayClankSound(Random(4)+1);
end;

procedure ChipWaggleSound;
begin
	if x_SoundStatus then SndPlaySound(hwpChipWaggle,Snd_NoDefault or Snd_Memory);
end;

function OValueWart.GetContent:string;
begin
	GetContent:=FloatToStr(OChipsProp_ptr(self.Parent)^.Value);
end;

procedure OValueWart.Redraw(aDC:HDC; x,y:integer);
begin
	if OChipsProp_ptr(self.Parent)^.Value>1.0 then inherited Redraw(aDC, x, y);
end;

constructor OChipstackProp.Init(aPile:PPileOfChips);
begin
	inherited Construct(GetBitmapWd(topChipMask), BOTTOM_LEFT);
	TheChips:=aPile;
	AddWart(new(OValueWart_ptr, Construct(@self)), self.valuewart_position);
end;

procedure OChipstackProp.GetSpanRect(var rRect:TRect);
{ the anchor point is the top left corner of the bitmap on the bottom of the pile }
var
	i:integer;
begin //writeln('OChipstackProp.GetSpanRect(', DumpToString(rRect),')');
	rRect.left:=Anchor.X;
	rRect.right:=rRect.left + GetWidth;
	rRect.bottom:=Anchor.Y + GetBitmapHt(topChipMask);
	rRect.top:=Anchor.Y;
	if (TheChips^.Count>0) then rRect.top:= rRect.top-CHIP_STACK_DY*(TheChips^.Count-1);
end;

procedure OChipstackProp.IncChips(toc:TypeOfChip; n:integer);
var
	i:integer;
begin
	for i:= 1 to n do TheChips^.Insert(New(PCasinoChip, Init(toc)));
end;

function OChipstackProp.Discard:real;
var
	aValue:real;
begin
	aValue:=Value;
	TheChips^.FreeAll;
	Refresh;
	Discard:=aValue;
	OnValueChanged;
end;

procedure OChipstackProp.MatchIt;
var
	i, n:integer;
begin
	with TheChips^ do begin
		n:= Count;
		for i:= 0 to (n - 1) do Insert(New(PCasinoChip, Init(PCasinoChip(At(i))^.Denomination)));
	end;
	Refresh;
	OnValueChanged;
end;

procedure OChipstackProp.Double;
begin
	MatchIt;
	OnValueChanged;
end;

procedure OChipstackProp.PayBJ;
var
	i, n:integer;
begin
	AddAmount(TheChips^.DollarValue * 1.5);
	OnValueChanged;
end;

procedure OChipstackProp.AddAmount(Amount:Real);
begin
	TheChips^.AddAmount(Amount);
	Refresh;
	OnValueChanged;
end;

procedure OChipstackProp.RemoveAmount(Amount:Real);
begin
	TheChips^.RemoveAmount(Amount);
	Refresh;
	OnValueChanged;
end;

function OChipstackProp.Value:real;
begin
	Value:=TheChips^.DollarValue;
end;

procedure OChipstackProp.SplitToPile(TargetPile:OChipstackProp_ptr);
{ split this pile of chips in half and transer half to the target pile }
var
	d:Real;
begin
	d:= Value / 2;
	RemoveAmount(d);
	TargetPile^.AddAmount(d);
end;

procedure OChipbundleProp.GetSpanRect(var rRect:TRect);
	function HighestPile:integer;
	var
		d:TypeOfChip;
		h:integer;
		function hght(d:typeOfChip):integer;
		var
			h:integer;
		begin
			h:= (TheBundle^.Stacks[d]^.Size - 1) * Chip_Stack_dy + ChipHeight;
			if (d in [Chip1, Chip3, Chip5]) then Inc(h, bundle_pile_dy);
			h:= h + 5 * Chip_Stack_dy; {? fudge!!! to always give more height }
			hght:= h;
		end;
	begin
		h:= 0;
		for d:= Low(TypeOfChip) to High(TypeOfChip) do h:= max(Hght(d), h);
		HighestPile:= h;
	end;
begin
	with rRect do begin
		left:= Anchor.X;
		bottom:= Anchor.Y;
		right:= left + GetWidth;
		top:= bottom - HighestPile;
	end;
end;

procedure OChipbundleProp.MinimumChips;
var
	total:Real;
	toc:TypeOfChip;
	n:integer;
begin
	total:= TheBundle^.DollarValue;
	for toc:= High(TypeOfChip) downto Low(TypeOfChip) do begin
		TheBundle^.Stacks[toc]^.Discard;
		n:= trunc(total / ChipDollarValue(toc));
		TheBundle^.Stacks[toc]^.AddChips(n);
		total:= total - n * ChipDollarValue(toc);
	end;
end;

function ChipWidth:integer;
begin
	ChipWidth:= GetBitmapWd(topChipMask);
end;

function ChipHeight:integer;
begin
	ChipHeight:= GetBitmapHt(topChipMask);
end;

(*
procedure OChipbundleProp.MaximizeChips;

var
	total:Real;
	toc:TypeOfChip;
	n:integer;

begin
	total:= TheBundle^.DollarValue;

	{ first the 50 cent pieces }

	TheBundle^.Stacks[Chip1]^.Discard;
	n:= Round(total / ChipDollarValue(Chip1)) mod 2;
	TheBundle^.Stacks[Chip1]^.AddChips(n);
	total:= total - n * ChipDollarValue(Chip1);

	for toc:= Chip2 to High(TypeOfChip) do begin
		if total = 0 then break;
		if (toc = High(TypeOfChip)) then
			n:= Round(total / ChipDollarValue(toc))
		else begin
			n:= Round(total) - Round(ChipDollarValue(Succ(toc))) * (Round(total) div Round(ChipDollarValue(Succ(toc))) - 1);
			n:= n div Round(ChipDollarValue(toc));
		end;
		TheBundle^.Stacks[toc]^.Discard;
		TheBundle^.Stacks[toc]^.AddChips(n);
		total:= total - n * ChipDollarValue(toc);
	end;
end;
*)

function OChipbundleProp.Discard:real;
begin //writeln('OChipbundleProp.Discard');
	Discard:=TheBundle^.DollarValue;
	TheBundle^.Discard;
	Refresh;
	OnValueChanged;
end;

procedure OChipbundleProp.SetAmount(Amt:Real);
begin
	TheBundle^.SetAmount(Amt);
	Refresh;
	OnValueChanged;
end;

procedure OChipbundleProp.ReDraw(dc:HDC;aX,aY:integer);
	procedure DrawIt(toc:TypeOfChip; aX, aY:integer);
	var
		i:integer;
	begin
		with TheBundle^.Stacks[toc]^ do for i:= 0 to (Size - 1) do begin
			PutBitmap(DC, topChipMask, aX, aY - i * CHIP_STACK_DY, SrcAnd);
			PutBitmap(DC, topChipImage[toc], aX, aY - i * CHIP_STACK_DY, SrcPaint);
		end;
	end;
begin //writeln('OChipbundleProp.ReDraw(dc:HDC;aX,aY:integer);');
	DrawIt(Chip5, aX + bundle_pile_dx * 1, aY - GetBitmapHt(topChipMask) - bundle_pile_dy);
	DrawIt(Chip3, aX + bundle_pile_dx * 3, aY - GetBitmapHt(topChipMask) - bundle_pile_dy);
	DrawIt(Chip1, aX + bundle_pile_dx * 5, aY - GetBitmapHt(topChipMask) - bundle_pile_dy);
	DrawIt(Chip6, aX + bundle_pile_dx * 0, aY - GetBitmapHt(topChipMask));
	DrawIt(Chip4, aX + bundle_pile_dx * 2, aY - GetBitmapHt(topChipMask));
	DrawIt(Chip2, aX + bundle_pile_dx * 4, aY - GetBitmapHt(topChipMask));
end;

constructor OChipsProp.Construct(w:word; valuewart_position:relativeposition);
begin
	inherited Construct(w, 0);
	self.valuewart_position:=valuewart_position;
end;

procedure OChipsProp.OnValueChanged; 
begin //writeln('OChipsProp.OnValueChanged');
	if (GetWartAt(self.valuewart_position)<>NIL) then with GetWartAt(self.valuewart_position)^ do if (Value=0.0) then Hide else if IsVisible then Refresh else Show;
end;

function OChipstackProp.ChipCount:word;
begin
	ChipCount:=TheChips^.Count;
end;

function OChipstackProp.Pop:TypeOfChip;
var
	aChipType:TypeOfChip;
	pChip:PCasinoChip;
begin
	pChip:=PCasinoChip(TheChips^.At(TheChips^.Count-1));
	aChipType:= pChip^.Denomination;
	TheChips^.Free(pChip);
	Refresh;
	OnValueChanged;
	Pop:=aChipType;
end;

procedure OChipstackProp.ClankSound;
begin
	PlayClankSound(Random(4)+1);
end;

procedure OChipstackProp.SnapTo(target:OChipstackProp_ptr);
var
	i:integer;
	playSoundEffects:boolean;
begin
	Hide;
	playSoundEffects:=(ChipCount>0) and (target^.ChipCount>0);
	for i:= 1 to TheChips^.Count do target^.TheChips^.AddAmount(PCasinoChip(TheChips^.At(i-1))^.DollarValue);
	TheChips^.RemoveAmount(TheChips^.DollarValue);
	Show;
	OnValueChanged;
	if playSoundEffects then ClankSound;
	target^.Refresh;
	target^.OnValueChanged;
end;

procedure OChipstackProp.Push(aChip:TypeOfChip);
begin
	if ChipCount>0 then Clanksound;
	IncChips(aChip, 1);
	Refresh;
	OnValueChanged;
end;

function OChipbundleProp.PtHitsChips(const aX:integer; const aY:integer; var ChipDenomination:TypeOfChip):boolean;
	function Hits(n, x, y:integer):boolean;
	begin
		Hits:=
			(n > 0)
			and
			((aX >= x) and (aX <= x + GetBitmapWd(topChipMask)))
			and
			(
			(aY >= (Height  - y - ((n - 1) * CHIP_STACK_DY) - chipHeight))
			and
			(aY < Height - y)
			);
	end;
begin
	PtHitsChips:= False;
	if Hits(TheBundle^.Stacks[Chip6]^.Size, bundle_pile_dx * 0, 0) then begin
		ChipDenomination:= Chip6;
		PtHitsChips:= True;
		Exit;
	end;
	if Hits(TheBundle^.Stacks[Chip4]^.Size, bundle_pile_dx * 2, 0) then begin
		ChipDenomination:= Chip4;
		PtHitsChips:= True;
		Exit;
	end;
	if Hits(TheBundle^.Stacks[Chip2]^.Size, bundle_pile_dx * 4, 0) then begin
		ChipDenomination:= Chip2;
		PtHitsChips:= True;
		Exit;
	end;
	if Hits(TheBundle^.Stacks[Chip5]^.Size, bundle_pile_dx * 1, bundle_pile_dy) then begin
		ChipDenomination:= Chip5;
		PtHitsChips:= True;
		Exit;
	end;
	if Hits(TheBundle^.Stacks[Chip3]^.Size, bundle_pile_dx * 3, bundle_pile_dy) then begin
		ChipDenomination:= Chip3;
		PtHitsChips:= True;
		Exit;
	end;
end;

constructor OChipbundleProp.Init(aBundle:PBundleOfChips);
	function ChipBundleWidth:integer;
	begin
		ChipBundleWidth:= bundle_pile_dx * (Ord(High(TypeOfChip)) - Ord(Low(TypeOfChip))) + ChipWidth;
	end;
begin
	inherited Construct(ChipBundleWidth, CENTER_CENTER);
	TheBundle:=aBundle;
	AddWart(new(OValueWart_ptr, Construct(@self)));
end;

destructor OChipbundleProp.Done;
begin
	Dispose(TheBundle, Done);
end;

procedure OChipbundleProp.OnStackClicked(aChipType:TypeOfChip); 
begin 
	{place holder} 
end;

procedure OChipbundleProp.AddChip(aChip:TypeOfChip);
begin //writeln('OChipbundleProp.AddChip(',Ord(aChip),')');
	TheBundle^.AddChip(aChip);
	PlayClankSound(Random(4)+1);
	Refresh;
	OnChipsAdded(aChip,1);
	OnValueChanged;
end;

procedure OChipbundleProp.AddChips(aChip:TypeOfChip; aCount:word);
begin //writeln('OChipbundleProp.AddChips(aChip:TypeOfChip,',aCount,')');
	if aCount > 0 then begin
		while aCount > 0 do begin
			TheBundle^.AddChip(aChip);
			Dec(aCount);
		end;
		Refresh;
		Delay(BaseDelay);
		OnChipsAdded(aChip, aCount);
	end;
	OnValueChanged;
end;

procedure OChipbundleProp.OnChipsAdded(aChip:TypeOfChip; aCount:word);
begin //writeln('OChipbundleProp.OnChipsAdded(aChip:TypeOfChip,',aCount,')');
	if TheBundle^.Stacks[aChip]^.Size > PILE_STACK_LIMIT then begin
		TheBundle^.MaximizeChips;
		Refresh;
	end;
end;

function OChipbundleProp.PopUnits(nUnits:word):word;
var
	aRect:TRect;
begin //writeln('OChipbundleProp.PopUnits(',nUnits,')');
	if nUnits>0 then begin
		nUnits:=Min(nUnits,Integer(Round(TheBundle^.DollarValue)));
		TheBundle^.RemoveAmount(nUnits);
		Refresh;
	end;
	PopUnits:=nUnits;
	OnValueChanged;
end;

function OChipbundleProp.PopChip(aChipType:TypeOfChip):TypeOfChip;
begin
	TheBundle^.Stacks[aChipType]^.RemoveChips(1);
	Refresh;
	PopChip:=aChipType;
	OnValueChanged;
end;

function OChipbundleProp.Value:real;
begin
	Value:=theBundle^.DollarValue;
end;

function OChipbundleProp.IsStackEmpty(aChipType:TypeOfChip):boolean;
begin
	IsStackEmpty:=(theBundle^.stacks[aChipType]^.Size=0);;
end;

procedure OChipstackProp.PushChips(aChip:TypeOfChip;aChipCount:word);
begin
	if aChipCount > 0 then begin
		IncChips(aChip,aChipCount);
		Refresh;
		OnValueChanged;
	end;
end;

procedure LoadSoundRsc;
var
	s:string;
	rname:stringBuffer;
	i:integer;
begin
	hwChipWaggle:=FindResource(hInstance,'Chip_Waggle','WAVE');
	hwChipWaggle:=LoadResource(hInstance,hwChipWaggle);
	hwpChipWaggle:=LockResource(hwChipWaggle);
	for i:=1 to 4 do begin
		s:='Chip_Clank_'+NumberToString(i);
		StrPCopy(rname,s);
		hwChipClank[i]:=FindResource(hInstance,rName,'WAVE');
		hwChipClank[i]:=LoadResource(hInstance,hwChipClank[i]);
		hwpChipClank[i]:=LockResource(hwChipClank[i]);
	end;
end;

procedure FreeSoundRsc;
var
	i:integer;
begin
	for i:=1 to 4 do FreeResource(hwChipClank[i]);
	FreeResource(hwChipWaggle);
end;

procedure OChipstackProp.TransferTo(target:OChipbundleProp_ptr);
begin
	while Value>0 do begin
		target^.AddChip(Pop); 
		Delay(BaseDelay*3); 
	end;
end;

function OChipbundleProp.OnPressed(X:integer;Y:integer):boolean; 
var
	aChip:TypeOfChip;
begin
	if PtHitsChips(X,Y,aChip) then OnStackClicked(aChip);
	OnPressed:=FALSE;
end;

begin
	LoadChipRsc;
	LoadSoundRsc;
end.