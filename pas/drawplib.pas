{ (C) 2005 Wesley Steiner }

unit drawplib;

interface

uses
	poker,
	pokerlib,
	pokerTable;

type
	drawPokerVar=(ANYTHINGOPENS,JACKPOTS);

const
	drawPokerVarDesc:array[drawPokerVar] of PChar=('Anything Opens','Jackpots');

type
	tDrawPokerOptions=record
		gType:DrawPokerVar; { variation of draw poker we are playing }
		PassOK:array[DrawPokerVar,0..1] of boolean; { allow a Pass before and after the draw for each variation }
		m_aScoringMode:scoringType;
		AnteChips:array[DrawPokerVar] of Word; { # of unit chips for the ante by the dealer }
		beforeDrawOpen:word; { min # of unit chips to open with before the draw }
		beforeDrawOpenMax:word; { max # of unit chips to open with before the draw }
		afterDrawOpen:word; { min # of unit chips to open with after the draw }
		afterDrawOpenMax:word; { max # of unit chips to open with after the draw }
		raiselmts:pokerRaiseLimits;
		wType:wildType; { current wild card type }
		wildcards:WildCardList; { wild card list for when "wType" is custom }
	end;

const
	theDrawPokerOptions:tDrawPokerOptions=(
		gType:ANYTHINGOPENS;
		PassOk:((FALSE,TRUE),(TRUE,TRUE));
		m_aScoringMode:PlayHigh;
		AnteChips:(2,2);
		beforeDrawOpen:1;
		beforeDrawOpenMax:DEFAULT_TABLELIMIT;
		afterDrawOpen:2;
		afterDrawOpenMax:DEFAULT_TABLELIMIT;
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

function Ante(t:DrawPokerVar):Word;
function Variation:DrawPokerVar;

implementation

function Ante(t:DrawPokerVar):Word;

begin
	Ante:=theDrawPokerOptions.AnteChips[t];
end;

function Variation:DrawPokerVar;

begin
	Variation:=theDrawPokerOptions.gType;
end;

end.
