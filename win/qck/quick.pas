{  (C) 2009 Wesley Steiner }

{$MODE FPC}

unit quick; // A generic "Quick" application.

{$I platform}
{$I punit.inc}

interface

uses
	std,
	qcktbl;

const
	REGKEY_ROOT='';
	KEY_TABLETOP='Tabletop';
	KEY_SOUND_EFFECTS='Sound';
	KEY_TABLETOP_COLOR_R='ColorR';
	KEY_TABLETOP_COLOR_G='ColorG';
	KEY_TABLETOP_COLOR_B='ColorB';
	KEY_SHOWVARIATIONDIALOG='ShowVariationDialog';
	KEY_TABLETOP_ANIMATION='Animation';
	KEY_TABLETOP_IMAGEPATH='ImagePath';
	KEY_TABLETOP_USEIMAGE='UseImage';
	KEY_VARIATION='Variation';
	HOMEPAGE_DIR='http://www.wesleysteiner.com/quickgames/';
	
	ID_BASE 							= 17301;

	IDS_NEXT 						= ID_BASE + 0;
	IDS_COUNT_PANEL				= IDS_NEXT + 2;
	IDS_DRAW_TABLE_HARD			= IDS_NEXT + 3;
	IDS_DRAW_TABLE_SOFT			= IDS_NEXT + 10;
	IDS_DD_TABLE_HARD				= IDS_NEXT + 11;
	IDS_DD_TABLE_SOFT				= IDS_NEXT + 12;
	IDS_SPLIT_TABLE				= IDS_NEXT + 21;
	IDS_DD_TBL_SOFT_MULT			= IDS_NEXT + 23;
	IDS_VIEW_SCOREPAD				= IDS_NEXT + 30;

	IDD_NEXT 						= IDS_NEXT + 31; {17332}
	IDD_RULES						= IDD_NEXT + 33;
	
	IDB_NEXT							= IDD_NEXT + 36;
	IDB_DOLLARS						= IDB_NEXT + 27;
	IDD_INSURANCE					= IDB_NEXT + 54;

type
	ApplicationP=^Application;
	Application=object
		FriendlyName:pchar;
		constructor Construct(friendly_name:pchar);
		destructor Destruct;
		function ChooseVariation:boolean; test_virtual
		function CurrentGame:GameP;
		function GetBooleanData(aKey,aSubKey:pchar;aDefaultValue:boolean):boolean; virtual; abstract;
		function GetIntegerData(aKey,aSubKey:pchar;aDefaultValue:longint):longint; virtual; abstract;
		function GetIntegerDataRange(aKey,aSubKey:pchar;aLowValue,aHighValue,aDefaultValue:longint):longint; virtual; abstract;
		function GetStringData(aKey,aSubKey:pchar;const aDefaultValue:ansistring):ansistring; virtual; abstract;
		function SelectGameVariation:variationIndex; virtual;
		function ShowVariationDialog:boolean; test_virtual
		procedure OnNew; virtual;
		procedure SetCurrentGame(game:GameP);
		procedure SetBooleanData(aKey,aSubKey:pchar;aValue:boolean); virtual; abstract;
		procedure SetIntegerData(aKey,aSubKey:pchar;aValue:longint); virtual; abstract;
	protected
		procedure PersistVariationSelection(aGameId:gameIndex;n:variationIndex); test_virtual
	end;
	
function GetGameKey(aGameId:gameIndex):string;
	
implementation

uses
	{$ifdef TEST} quickTests, {$endif}
	strings,stringsx;

constructor Application.Construct(friendly_name:pchar);
begin //writeln('Application.Construct');
	FriendlyName:=StrNew(friendly_name);
end;

function Application.ShowVariationDialog:boolean;
begin
	ShowVariationDialog:=GetBooleanData(REGKEY_ROOT,KEY_SHOWVARIATIONDIALOG,FALSE);
end;

destructor Application.Destruct;
begin
	StrDispose(FriendlyName);
end;

function GetGameKey(aGameId:gameIndex):string;
begin
	GetGameKey:='Game-'+NumberToString(Integer(aGameId));
end;

procedure Application.PersistVariationSelection(aGameId:gameIndex;n:variationIndex);
var
	buffer:stringBuffer;
begin
	SetIntegerData(StrPCopy(buffer,GetGameKey(aGameId)),KEY_VARIATION,n);
end;

function Application.ChooseVariation:boolean;
var
	new_selection:variationIndex;
begin
	new_selection:=SelectGameVariation;
	if new_selection<>CurrentGame^.Variation then begin
		CurrentGame^.SetVariation(new_selection);
		PersistVariationSelection(CurrentGame^.MyIndex,new_selection);
		ChooseVariation:=TRUE;
	end
	else ChooseVariation:=FALSE;
end;

procedure Application.OnNew;
begin
	current_game:=NIL;
end;

procedure Application.SetCurrentGame(game:GameP);
begin
	current_game:=game;
end;

function Application.CurrentGame:GameP;
begin
	CurrentGame:=current_game;
end;

function Application.SelectGameVariation:variationIndex;
begin
	SelectGameVariation:=0;
end;

begin
	randomize;
end.
