{ (C) 2009 Wesley Steiner }

{$MODE FPC}

unit qcktbl; (* generic tabletop surface to play games on. *)

{$I punit.inc}

interface

uses
	std;
	
const
	MIN_EDGE_MARGIN_IN_PIXELS=5;
	MIN_EDGE_MARGIN=MIN_EDGE_MARGIN_IN_PIXELS;

	OPPONENT_POOL_SIZE=8;
	MINSKILLLEVEL=-2;
	KNOWSSWS=-1; { this player knows about the safe way system }
	BASICBETLEVEL=0; { this or better to know about the "Basic Betting Principle" }
	BASICRAISELEVEL=1; { this or better to know about the "Raising Principle" }
	POSITIONALOPEN=2; { player knows about positional openers }
	MAXSKILLLEVEL=+3;

type
	skillLevel=MINSKILLLEVEL..MAXSKILLLEVEL;
	opponentPoolIndex=1..OPPONENT_POOL_SIZE;
	opponentPoolIndexSet=set of opponentPoolIndex;
	
	OObject=object
		constructor Construct;
		function ToString:string; virtual;
	end;
	
	gameIndex=ordinal;
	variationIndex=ordinal;
	GameP=^Game;
	Game=object(OObject)
		constructor Construct;
		constructor Construct(n:gameIndex);
		function MyIndex:gameIndex;
		function Variation:variationIndex;
		function VariationCount:variationIndex; virtual;
		function VariationName(n:variationIndex):pchar; virtual;
		procedure SetVariation(n:variationIndex);
	private
		my_index:gameIndex;
		my_variation:variationIndex;
		procedure PostConstruct(n:gameIndex);
	end;

	OProp=object(OObject)
		Ordinal:integer;
		constructor Construct;
		constructor Construct(visibility:boolean);
		function IsVisible:boolean;
		procedure Hide; test_virtual
		procedure OnHidden; virtual;		
		procedure OnHiding; virtual;
		procedure OnShown; virtual;		
		procedure OnShowing; virtual;
		procedure Refresh; virtual;
		procedure Show; test_virtual
	private
		is_visible:boolean;
		procedure PostConstruct(visibility:boolean);
	end;

const
	current_game:GameP=NIL;

	{ data base of computer players, at the start of a new game player's are
		chosen at random from this bunch, the user can then choose which
		player's he would like after that. }
	the_opponent_pool:array[opponentPoolIndex] of record
		pnn:string[8];
		gndr:gender;
		slv:skillLevel;
		wlm:real; { for players that don't understand the basic betting principle, stay when the odds of winning are greater than this value (0.0 to 1.0) }
		rlm:real; { see "raiseLmt" }
		pof:real; { see "potOddsFactor" }
		smpd:integer; { see "smplDepth" }
		gfi:real; { see "GoForIt" }
	end=(
		(pnn:'Data';gndr:male;slv:basicRaiseLevel;wlm:0.0;rlm:0.99;pof:0.0;smpd:50;gfi:0.0),
		(pnn:'Blondie';gndr:female;slv:minSkilllevel;wlm:0.15;rlm:0.8;pof:0.1;smpd:5;gfi:0.3),
		(pnn:'Spock';gndr:male;slv:maxSkillLevel;wlm:0.0;rlm:0.0;pof:0.03;smpd:30;gfi:0.05),
		(pnn:'Pistol';gndr:male;slv:minSkilllevel;wlm:0.2;rlm:0.9;pof:0.1;smpd:10;gfi:0.2),
		(pnn:'Junior';gndr:male;slv:basicBetLevel;wlm:0.0;rlm:0.85;pof:0.07;smpd:20;gfi:0.3),
		(pnn:'Mercedes';gndr:female;slv:maxSkillLevel;wlm:0.0;rlm:0.0;pof:0.05;smpd:20;gfi:0.15),
		(pnn:'Dunce';gndr:male;slv:minSkilllevel;wlm:0.1;rlm:0.9;pof:0.2;smpd:5;gfi:0.35),
		(pnn:'Sharky';gndr:male;slv:basicBetLevel;wlm:0.0;rlm:0.75;pof:0.08;smpd:15;gfi:0.28)
		);

implementation

function Game.VariationCount:variationIndex;
begin
	VariationCount:=0;
end;

function Game.Variation:variationIndex;
begin
	Variation:=my_variation;
end;

procedure Game.SetVariation(n:variationIndex);
begin
	if (n>VariationCount) 
		then my_variation:=0
		else my_variation:=n;
end;

function Game.VariationName(n:variationIndex):pchar;
begin
	VariationName:='Standard';	
end;

constructor Game.Construct;
begin
	inherited Construct;
	PostConstruct(0);
end;

constructor Game.Construct(n:gameIndex);
begin
	inherited Construct;
	PostConstruct(n);
end;

procedure Game.PostConstruct(n:gameIndex);
begin
	my_index:=n;
end;

function Game.MyIndex:gameIndex;
begin
	MyIndex:=my_index;
end;

procedure OProp.PostConstruct(visibility:boolean);
begin
	Ordinal:=1;
	is_visible:=visibility;
end;

constructor OProp.Construct;
begin
	inherited Construct;
	PostConstruct(TRUE);
end;

constructor OProp.Construct(visibility:boolean);
begin
	inherited Construct;
	PostConstruct(visibility);
end;

procedure OProp.Refresh;
begin
end;

function OProp.IsVisible:boolean;
begin
	IsVisible:=is_visible;
end;

procedure OProp.Hide;
begin //writeln('OProp.Hide');
	OnHiding;
	is_visible:=FALSE;
	Refresh;
	OnHidden;
end;

procedure OProp.Show;
begin //writeln('OProp.Show');
	OnShowing;
	is_visible:=TRUE;
	Refresh;
	OnShown;
end;

procedure OProp.OnHidden;
begin
end;

procedure OProp.OnHiding;
begin
end;

procedure OProp.OnShown;
begin //writeln('OProp.OnShown');
end;

procedure OProp.OnShowing;
begin
end;

constructor OObject.Construct;
begin
end;

function OObject.ToString:string; 
begin 
	ToString:='';
end;

end.
