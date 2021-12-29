{ (C) 1998 Wesley Steiner }

{$MODE FPC}

unit casview;

interface

uses
	winqcktbl,
	casino,
	winTabletopChips;

type
	PlayerBetProp = object(OChipstackProp)
		constructor Construct(pPile:PPileOfChips);
	end;

	PSeatChipsView=^TSeatChipsView;
	TSeatChipsView=object(OChipbundleProp)
		amountBorrowed:real;
		constructor Construct(aSeatNum:SeatIndex; aDollarAmt:real);
		procedure Borrow(aAmount:Real);
		function verifyWager(p_amount:Real):integer;
		function gross:real;
		function net:real;
		function SeatNum:SeatIndex;
	private
		sc_SeatNum:integer;
		procedure Initialize;
	end;

implementation

uses
	windows;

procedure TSeatChipsView.Initialize;
begin
	AmountBorrowed:= 0.0;
end;

constructor TSeatChipsView.Construct(aSeatNum:SeatIndex; aDollarAmt:real);
begin
	inherited Init(new(PBundleOfChips, Init(aDollarAmt)));
	Initialize;
	sc_SeatNum:= aSeatNum;
end;

procedure TSeatChipsView.Borrow(aAmount:Real);
begin
	AmountBorrowed:= AmountBorrowed + aAmount;
	SetAmount(TheBundle^.DollarValue + aAmount);
end;

function TSeatChipsView.verifyWager(p_amount:Real):integer;

	begin
		if (TheBundle^.DollarValue >= p_amount) then
			verifyWager:= 1
		else begin
			if sc_seatNum <> 11 then
				while (TheBundle^.DollarValue < p_amount) do Borrow(ChipDollarValue(High(TypeOfChip)))
			else case MessageBox(myTabletop^.handle,
				'Would you like to borrow from the house?', 'You can''t afford that!',
				MB_YESNO or MB_ICONEXCLAMATION) of
				IDYES:begin
					while (TheBundle^.DollarValue < p_amount) do
						Borrow(ChipDollarValue(High(TypeOfChip)));
					verifyWager:= 1;
				end;
				IDNO:
					verifyWager:= 0;
			end;
		end;
	end;

function TSeatChipsView.gross:real;

begin
	gross:= theBundle^.dollarValue;
end;

function TSeatChipsView.net:real;

begin
	net:= gross - amountBorrowed;
end;

function TSeatChipsView.SeatNum:SeatIndex;

begin
	SeatNum:= sc_SeatNum;
end;

constructor PlayerBetProp.Construct(pPile:PPileOfChips);

begin
	inherited Init(pPile);
end;

end.
