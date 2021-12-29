{ (C) 2005 Wesley Steiner }

unit studplib;

interface

uses
	poker,
	pokerlib,
	pokerTable;

type
	studPokerVar=(
		stud5card,
		stud6card,
		stud7card
		{$ifndef VER1}
		,baseball
		{$endif}
		);

const
	MAX_BETTING_ROUNDS = 5;

	StudPokerVarDesc:array[StudPokerVar] of PChar=('5-Card','6-Card','7-Card'
		{$ifndef VER1}
		,'Baseball'
		{$endif}
		);

type
	tStudPokerOptions = record
		gType:studPokerVar; { variation of stud poker we are playing }
		AnteChips:array[StudPokerVar] of word; { current ante, # of unit chips by the dealer }
		m_aScoringMode:scoringType;
		m_aRoundBettingLimits:record { betting limits on each betting round }
			minOpen:array[1..MAX_BETTING_ROUNDS] of word; { min # of unit chips to open with }
			maxOpen:array[1..MAX_BETTING_ROUNDS] of word; { max # of unit chips to open with }
		end;
		raiselmts:pokerRaiseLimits;
		wType:wildType; { current wild card type }
		wildcards:wildCardList; { wild card list for when "wType" is custom }
	end;

const
	theStudPokerOptions:tStudPokerOptions=(
		gType:stud5card;
		AnteChips:(0,0,0);
		m_aScoringMode:PlayHigh;
		m_aRoundBettingLimits:(
			minOpen:(1,1,1,1,1);
			maxOpen:(DEFAULT_TABLELIMIT,DEFAULT_TABLELIMIT,DEFAULT_TABLELIMIT,DEFAULT_TABLELIMIT,DEFAULT_TABLELIMIT);
		);
		raiselmts:(
			lmtPerRnd:3;
			lmtPerPsn:3;
		);
		wType:WILDNONE;
		wildcards:(
			wList:(
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE),
				(FALSE,FALSE,FALSE,FALSE)
			);
			jWild:FALSE;
		);
	);

function Ante(t:StudPokerVar):Word;
function Variation:StudPokerVar;

implementation

function Variation:StudPokerVar;

begin
	Variation:=theStudPokerOptions.gType;
end;

function Ante(t:StudPokerVar):Word;

begin
	Ante:=theStudPokerOptions.AnteChips[t];
end;

end.
