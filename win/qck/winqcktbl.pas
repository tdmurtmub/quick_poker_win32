{ (C) 2009 Wesley Steiner }

{$MODE FPC}

{$ifdef DEBUG}
{!$define SHOW_HOTSPOTS}
{$endif}

unit winqcktbl; (* Windows implementation of a qcktbl *)

{$I platform}
{$I punit.inc}

{$ifdef DEBUG}
{$define QUICK_PLAY}
{$endif}

interface

uses
	cardFactory,
	cards,
	drawing,
	objects,
	odlg,
	owindows,
	qcktbl,
	std,
	stdwin,
	windows;
	
const
	fdStep=16; { factory default number of points to plot a moving image between two end points, including the end points. }
	HCLASS:record BASE,GENPILEOFCARDS:byte; end=(BASE:0;GENPILEOFCARDS:1);
	
	POC_NORMAL		=$0000;
	POC_OUTLINED	=$0001;
	
	WM_BASE=WM_USER+100;
	WM_START=WM_BASE+1;
	WM_DEAL=WM_BASE+7; { deal a new hand }
	WM_CONTINUE=WM_BASE+11;
	WM_SIGNAL=WM_BASE+36;
	WM_NEXT=WM_BASE+100;

	DISPLAY_FONT_HEIGHT=20;
	WART_HEIGHT=DISPLAY_FONT_HEIGHT+1;
	WART_WIDTH=WART_HEIGHT;
	WART_OVERLAP=(WART_HEIGHT div 2);

type
	pausefactor=quantity;
	relativeposition=(
		CENTER_LEFT,
		CENTER_CENTER, 
		CENTER_RIGHT,
		TOP_LEFT, 
		TOP_CENTER, 
		TOP_RIGHT,
		BOTTOM_LEFT, 
		BOTTOM_CENTER, 
		BOTTOM_RIGHT
	);
	PTabletop=^OTabletop;
	PCardProp=^OCardProp;

	Game=object(qcktbl.Game)
		MyTabletop:PTabletop;
		constructor Construct(tabletop:PTabletop);
		constructor Construct(n:gameIndex;tabletop:PTabletop);
		function ColumnOffset(columnIndex:word):integer;
		function ColumnSpan(nPiles:word):integer;
		function PileRows:word; virtual;
		function PileColumns:word; virtual;
		function PileSpacing:integer; virtual;
		function RowOffset(rowIndex:word):integer;
		function RowSpan(nPiles:word):integer;
		function TotalHeight:word;
		function TotalWidth:word;
	private
		procedure Initialize(tabletop:PTabletop);
	end;
	GameP=^Game;

	HotspotClass=byte; { for classifying instances of polymorphic hotspot objects }

	PPropwart=^OPropwart;

	Hotspot=object(qcktbl.OProp)
		Anchor:TPoint;
		hrFlags:Word;
		MyTabletop:PTabletop;
		m_tag:pchar;
		constructor Construct;
		constructor Construct(w,h:integer);
		constructor Construct(w,h:integer; properties:flags);
		destructor Destruct; virtual;
		function Bottom:integer;
		function Enabled(const p_state:boolean):boolean; virtual;
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
		function GetHeight:word; virtual;
		function GetWartAt(where:relativeposition):PPropwart;
		function GetWidth:word; virtual;
		function Height:word;
		function HitTest(dx,dy:integer):boolean;
		function IsEnabled:boolean;
		function Left:integer;
		function PointIn(x:integer; y:integer):boolean;
		function ObjectClass:HotspotClass; virtual;
		function OffsetFromCenter:xypair;
		function OnPressed(dx,dy:integer):boolean; virtual;
		function OnReleased(dx,dy:integer):boolean; virtual;
		function OnCapturedRelease(dx,dy:integer):boolean; virtual;
		function OverRectangle(const rSrcRect:TRect):boolean;
		function Right:integer;
		function Top:integer;
		function ToString:string; virtual;
		function Width:word;
		procedure AddWart(wart:PPropwart);
		procedure AddWart(wart:PPropwart; where:relativeposition);
		procedure Disable; virtual;
		procedure Enable; virtual;
		procedure GetSpanRect(var rRect:TRect); virtual;
		procedure OnEnabled; virtual;
		procedure OnHiding; virtual;
		procedure OnShown; virtual;
		procedure Redraw(dc:HDC;x,y:integer); virtual;
		procedure Refresh; virtual;
		procedure RMousePress(dx,dy:integer); virtual;
		procedure Selected; virtual;
		procedure SetStickyPos(where:relativeposition);
		procedure RefreshRect(const rPrevSpan:TRect); test_virtual
		procedure SetPosition(point:xypair); test_virtual
		procedure SetPosition(x,y:integer);
		procedure SnapTo(new_position:xypair);
	test_private
		current_span:TRect;
		offset_from_center:xypair;
		propwarts:array[relativeposition] of PPropwart;
		relative_position:relativeposition;
		function GetOwnerWnd:HWND;
		procedure PostConstruct(w,h:integer);
		procedure SetRelativeOffset(table_w,table_h:number);
	end;
	PHotspot=^Hotspot;

	TCollectionOfHotspots=object(TCollection)
		function Last:PHotspot;
		procedure DiscardLast;
	test_private
		{ required overrides when items are not TObject pointers }
		procedure FreeItem(Item:Pointer); virtual;
		function GetItem(var S:TStream):pointer; virtual;
		procedure PutItem(var S:TStream; Item:Pointer); virtual;
	end;

	// A prop that displays a single line of text.
	OTextProp=object(Hotspot)
		function GetContent:string; virtual;
		function GetWidth:word; virtual;
		procedure Redraw(dc:HDC; x,y:integer); virtual;
	end;
	
	// A small text window that attaches to an existing host prop.
	OPropwart=object(OTextProp)
		constructor Construct(host:PHotspot);
		constructor Construct(host:PHotspot; initially_on:boolean);
		function ToString:string; virtual;
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
		function IsOn:boolean;
		function Parent:PHotspot;
		procedure Off;
		procedure On;
	private
		hostprop:PHotspot;
		onoff:integer;
		procedure PostConstruct(host:PHotspot; initially_on:boolean);
	end;
	
	OCardCountWart=object(OPropwart)
		constructor Construct(host:PHotspot);
		function GetContent:string; virtual;
	end;
	PCardCountWart=^OCardCountWart;
	
	PlayerPrompt=object(OWND)
		constructor Construct(parent:PTabletop);
		procedure Show; virtual;
		procedure Hide; virtual;
		function IsVisible:boolean;
		function MyParent:PWindow;
		procedure SetText(aTextString:PChar); virtual;
	private
		my_parent:PWindow;
		previous_font:HFONT;
		procedure Draw(dc:HDC);
		procedure Reposition;
		procedure SelectFont(dc:HDC);
		procedure RestoreFont(dc:HDC);
	end;

	GenPileOfCardsP=^GenPileOfCards;
	GenPileOfCards=object(Hotspot) // OBSOLETE: use OCardpileProp
		HasTarget:boolean; { display a target in the middle when pile is empty }
		TargetState:boolean; { state of the target }
		m_outlined:boolean;
		ThePile:PPileOfCards; { the logical pile of cards }
		Desc:pchar;
		constructor Construct(pPile:PPileOfCards;p_flags:word;aCardDx,aCardDy:integer);
		destructor Destruct; virtual;
		function AddFacedown:boolean; virtual; { for the next card to be added }
		function Accepts(aCard:TCard):boolean; virtual;
		function CanGrabCardAt(aIndex:integer):boolean; virtual;
		function CanGrabUnit(aIndex:integer):boolean; virtual;
		function CanSelectCardAt(n:number):boolean; virtual;
		function CardAt(n:number):TCard; virtual;
		function CardOffsetRect(n:number):TRect;
		function CardOffsetX(n:number):integer;
		function CardOffsetY(n:number):integer;
		function CardDx:integer;
		function CardDy:integer;
		function CardIsFacedown(n:number):boolean;
		function CardIsFaceup(n:number):boolean;
		function GetAnchorX:integer; virtual;
		function GetAnchorY:integer; virtual;
		function GetCardAtRect(n:number):TRect;
		function GetCardX(nth:integer):integer; virtual;
		function GetCardY(nth:integer):integer; virtual;
		function GetHeight:word; virtual;
		function GetWidth:word; virtual;
		function IsDblClkTarget:boolean; virtual;
		function IsEmpty:boolean; virtual; // 01-04-09: WARNING! removing this virtual causes New game to crash!
		function OnCardAtTapped(n:number):boolean; virtual;
		function OnPressed(dx,dy:integer):boolean; virtual;
		function OnReleased(dx,dy:integer):boolean; virtual;
		function OnTopcardTapped:boolean; virtual;
		function Overlaps(Target:TRect; var Intersection:TRect):boolean;
		function ObjectClass:HotspotClass; virtual;
		function PointHitsCard(dx,dy:integer):integer; test_virtual
		function Removetop:TCard; virtual;
		function Size:integer; virtual;
		function Topcard:TCard; test_virtual
		function TopcardIsFacedown:boolean;
		function TopFacedown:boolean; virtual;
		function TopFaceup:boolean; virtual;
		procedure AddCard(aCard:TCard);
		procedure AppendDesc(const pText:pchar);
		procedure CardAtTo(n:number; target:GenPileOfCardsP);
		procedure CardAtTo(n:number; target:GenPileOfCardsP; flip_it:boolean; pause_after:pauseFactor);
		procedure Discard; virtual;
		procedure DiscardTop;
		procedure DrawCard(DC:HDC; nth:integer;  const x:integer; const y:integer); virtual;
		procedure Drag(dx,dy,aIndex:integer);
		procedure DropOntop(dx,dy:integer); virtual;
		procedure Flip; virtual; { physically flip the pile over }
		procedure FlipCard(n:number);
		procedure FlipCard(n:number; pause_after:pauseFactor);
		procedure FlipTopcard;
		procedure FlipTopcard(pause_after:pauseFactor);
		procedure Help; virtual;
		procedure LMousePressCard(dx,dy,aIndex:integer); virtual;
		procedure OnCardAdded virtual;
		procedure OnCardFlipped(n:number); virtual;
		procedure OnDragging; virtual;
		procedure OnDropped; virtual;
		procedure OnTopcardFlipped; virtual;
		procedure SetCardDx(aCardDx:integer);
		procedure SetCardDy(aCardDy:integer);
		procedure SnapAllTo(target:GenPileOfCardsP);
		procedure SnapTopTo(target:GenPileOfCardsP); virtual;
		procedure TopcardTo(target:GenPileOfCardsP); virtual;
		procedure TopcardTo(target:GenPileOfCardsP; flip_it:boolean; pause_after:pauseFactor);
		procedure TransferTo(target:GenPileOfCardsP);
		procedure TryTopcardToDblClkTargets; virtual;
		procedure RefreshCard(n:number); test_virtual
		procedure RMousePress(dx,dy:integer); virtual; { right mouse button pressed at x,y relative to anchor }
		procedure Redraw(dc:HDC; x,y:integer); virtual;
		procedure SetDesc(const pText:pchar);
		procedure Shuffle; virtual;
		procedure TopSelected; virtual; // OBSOLETE! use OnTopcardTapped
		function Get(nth:integer):TCard; // OBSOLETE: use CardAt
		procedure GetSpanRect(var rRect:TRect); virtual; // OBSOLETE: use GetWidth, GetHeight
		function IsFacedown(nth:integer):boolean; // OBSOLETE: use CardIsFacedown
		function IsFaceup(aIndex:word):boolean; // OBSOLETE: use CardIsFaceup
	test_private
		my_card_dx,my_card_dy:integer;
		function CardIsCovered(aIndex:integer):boolean; test_virtual
		function CardIsPlaceHolder(aIndex:integer):boolean; test_virtual
		function FindPlaceHolder:word;
		function IsCardExposed(aIndex:integer):boolean;
		procedure AddCardAt(aIndex:integer; aCard:TCard);
		procedure DiscardPlaceHolder;
		procedure GetGrabbed;
		procedure GetGrabbedAt(aIndex:integer);
		procedure GrabCardAt(n:number);
		procedure GrabCardsAt(n:number);
		procedure MoveAbs(NewX, NewY:integer);
		procedure MoveTo(NewX, NewY:integer);
		procedure PostConstruct;
	end;

	signalId=ordinal;
	
	OTabletop=object(OWindow)
		CapturedReleaseTarget:^Hotspot;
		HotList:TCollectionOfHotspots;
		MyGame:GameP;
		player_prompt:PlayerPrompt;
		tabletop_image:HBITMAP;
		UseBgImage:boolean;
		constructor Construct(background_color:TColorRef;background_image:HBITMAP;use_image:boolean);
		function AddProp(prop:PHotspot):PHotspot;
		function AddProp(prop:PHotspot;anchor_point:xypair):PHotspot;
		function AddProp(prop:PHotspot; where:relativeposition):PHotspot;
		function BgColor:TColorRef;
		function BgImage:HBITMAP; test_virtual
		function Bottom:integer; test_virtual
		function ClientAreaHt:integer; test_virtual
		function ClientAreaWd:integer; test_virtual
		function Height:word; test_virtual
		function Margin:integer; test_virtual
		function Create(frame:HWND;w,h:number):HWND; virtual;
		function MyFrame:HWND;
		function OnDoubleTapped(x,y:integer):boolean; virtual;
		function OnEraseBkGnd(dc:HDC):LONG; virtual;
		function OnLButtonDblClick(keys:UINT;x,y:integer):LONG; virtual;
		function OnLButtonDown(keys:uint;x,y:integer):LONG; virtual;
		function OnLButtonUp(keys:uint;x,y:integer):LONG; virtual;
		function OnRButtonDown(keys:uint;x,y:integer):LONG; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		function OnMouseMove(keys:uint;x,y:integer):LONG; virtual;
		function OnSize(resizeType:uint;newWidth,newHeight:integer):LONG; virtual;
		function OnTapped:boolean; virtual;
		function Top:integer; test_virtual
		function Width:word; test_virtual
		procedure Initialize(background_color:TColorRef;background_image:HBITMAP;use_image:boolean); virtual;
		procedure Paint(PaintDC:HDC; var PaintInfo:TPaintStruct); virtual;
		procedure RefreshRect(const rRect:TRect); virtual;
		procedure Render(target_DC:HDC; const rSrcRect:TRect); virtual;
		procedure RenderTabletopSurface(dc:HDC;xOffset,yOffset:integer); virtual;
		procedure ResetCapturedReleaseTarget;
		procedure SetBackground(color:TColorRef;bitmap:HBITMAP;use_image:boolean);
		procedure SetCapturedReleaseTarget(target:PHotspot);
	test_private
		function CreateTabletopBitmap(w,h:integer):HBITMAP;
		procedure PostConstruct;
	private
		frame:HWND;
		background_image:HBITMAP;
		function OnDrawItem(control_id:UINT;draw_info:LPDRAWITEMSTRUCT):LONG;
	end;

	OCardpileprop_ptr=^OCardpileprop;
	OCardpileprop=object(GenPileOfCards)
		constructor Construct;
		constructor Construct(n:number);
		constructor Construct(n:number; where:relativeposition);
		destructor Destruct; virtual;
		function AceUp:boolean;
		function CanGrabCardAt(a_index:integer):boolean; virtual;
		function Capacity:number;
		function CardCount:quantity;
		function DealCardTo(n:number;center_point:xypair;table:PTabletop):OCardpileProp_ptr;
		function DealTo(point:xypair):PCardProp;
		function DealTo(point:xypair;flip_it:boolean):PCardProp;
		function DealTo(x,y:integer):PCardProp;
		function TopX:integer; { client rel }
		function TopY:integer;
		procedure Collapse;
		procedure DealCardTo(n:number;target:OCardpileProp_ptr);
		procedure DealTo(target:OCardpileProp_ptr); virtual;
		procedure DealTo(target:OCardpileProp_ptr;flip_it:boolean); virtual;
		procedure Fan(dx,dy:integer;n:word);
		procedure FanLeft;
		procedure FanRight;
		procedure Help; virtual;
		procedure SlideTo(center_point:xypair);
		procedure SlideTo(center_x,center_y:integer);
	private
		my_pile:cards.PileOfCards;
		function MoveCardTo(n:number;center_point:xypair;table:PTabletop;animate:boolean):OCardpileProp_ptr;
	end;

	OSquaredpileprop=object(OCardpileProp)
		constructor Construct;
		constructor Construct(n:number);
		procedure CardCountOff;
		procedure CardCountOn;
		procedure OnDragging; virtual;
		procedure OnDropped; virtual;
	end;
	OSquaredpileprop_ptr=^OSquaredpileprop;

	OFannedPileProp=object(OCardpileProp)
		IsUnit:array[1..104] of boolean;
		constructor Construct;
		function CanGrabUnit(a_index:integer):boolean; virtual;
		function Covered:boolean; { true if covered by another pile }
		function OnPressed(x_offset,y_offset:integer):boolean; virtual;
		procedure DropOntop(x,y:integer); virtual;
		procedure TopRemoved; virtual; { called after top card is removed }
		procedure TopSetUnit(State:boolean);
		procedure UnitRemoved; virtual;
	end;
	OFannedPileProp_ptr=^OFannedPileProp;
	
	OCardProp=object(OCardpileProp)
		constructor Construct(card:TCard);
		function CanGrabCardAt(aIndex:integer):boolean; virtual;
		function GetCard:TCard;
	private
		function OnTopcardTapped:boolean; virtual;
	end;
	
	OHandprop=object(OCardpileProp)
	end;

	ODeckprop=object(OSquaredpileprop)
		constructor Construct(packs:number);
	end;

	TPileHelpDlg=object(ODialog)
		Pile:GenPileOfCardsP;
		constructor Construct(aPile:GenPileOfCardsP);
		function OnInitDialog:boolean; virtual;
	end;

	BaseCardGraphicsManagerP=^BaseCardGraphicsManager;
	ICardGraphicsManager=BaseCardGraphicsManagerP;
	BaseCardGraphicsManager=object
		constructor Construct;
		function BestFit(n_columns,n_rows:word;space_between:integer;total_width,total_height:word;edge_margin:integer):word; virtual; abstract;
		function CurrentWidth:word; virtual; abstract;
		function CurrentHeight:word; virtual; abstract;
		procedure SelectWidth(in_pixels:word); virtual; abstract;
	end;
	
const
	BASEDELAYMAX=25;

var
	grabbed_from:GenPileOfCardsP;

const
	animateSteps:integer=fdStep;
	baseDelay:quantity=BASEDELAYMAX div 2;
	card_graphics_manager:ICardGraphicsManager=nil;
	dragging:boolean=False;
	tabletopBrush:HBRUSH=0; { brush for painting the table top }
	theGrabbedCards:PPileOfCards=nil;
	unitDragging:boolean=False; { the item being dragged is a pile unit }
	x_SoundStatus:boolean=TRUE;
	x_table_top_color:TColorRef=$00008000; { current table top brush color }
	
function BestFit(n_columns,n_rows:word;space_between:integer;total_width,total_height:word;edge_margin:integer):word;
function CardCenterToAnchor(center_point:xypair):xypair;
function CardAnchorToCenter(anchor_point:xypair):xypair;
function CardPlayDelay:integer; // OBSOLETE: use DealDelay
function CurrentWidth:word;
function CurrentHeight:word;
function DealDelay:integer;

procedure DefaultWinnerSound;
procedure DisplayCard(dc:HDC;C:TCard; x,y:integer);
procedure DrawDCNoSymbol(dc:hDC;aRect:TRect);
procedure Terminate;
procedure MakeNewTTColor(color:TColorRef);
procedure SelectWidth(in_pixels:word);
procedure SndCardFlick;

// UNIT TESTING

type
	__CardGraphicsManager=object
		function Instance:ICardGraphicsManager;
	end;
	
	TheCardGraphicsManager=object(BaseCardGraphicsManager)
		constructor Construct(aCardFactory:ICardFactory);
		function BestFit(n_columns,n_rows:word;space_between:integer;total_width,total_height:word;edge_margin:integer):word; virtual;
		function CurrentWidth:word; virtual;
		function CurrentHeight:word; virtual;
		function GetBackBitmap:drawing.bitmap;
		function GetFaceBitmap(card:cards.card):drawing.bitmap;
		function GetMaskBitmap:drawing.bitmap;
		procedure SelectWidth(in_pixels:word); virtual;
	test_private
		myCardFactory:ICardFactory;
		function BestColumnFit(nPiles:word;space_between:integer;total_width:word;edge_margin:integer):word; test_virtual
		function BestRowFit(nPiles:word;space_between:integer;total_height:word;edge_margin:integer):word; test_virtual
		function LargestThatFits(n_things:word;space_between:integer;tableSize:word;edge_margin:integer;checkForWidth:boolean):word; 
	end;

var
	CardGraphicsManager:__CardGraphicsManager;
	the_instance:TheCardGraphicsManager;

function ConvertPauseToMillSeconds(pause:pauseFactor):quantity;
function IsDerivedFromGenPileOfCards(aHotSpotPtr:PHotspot):boolean;

procedure UpdateCardSize(newWidth,newHeight:word;var aSelector:winqcktbl.Game;manager:ICardGraphicsManager);

implementation 

uses
	strings,
	mathx,
	mmsystem,
	stringsx,
	sdkex,
	windowsx,
	gdiex,
	{$ifdef TEST} winCardFactoryTests, {$endif}
	winCardfactory;
	
const
	HR_ENABLED=$0001; { hot region is enabled }
	
type
	CardSizingMonitorP=^CardSizingMonitor;
	CardSizingMonitor=object(Hotspot)
		constructor Construct;
		function GetAnchorPoint(table_width,table_height:word):xypair; virtual;
	end;

var
	theGrabImage,theGrabRestore:HBITMAP;
	DragOrg:TPoint;
	GrabbedCard:TCard;
	bmd:TBitmap;
	hdcRAM,hdcImage,hdcUnder:HDC;
	hbmRAM,saved_bitmap:HBITMAP;
	rs:TRect;
	Drag_RestoreCursor:hCursor;
	hwndAnimate:HWND; { current animation takes place in the client area of this window }
	display_font:HFONT;

const
	BitmapTransfer:boolean=True; { default is to move bitmaps }
	isDragging:boolean=false;
	theGrabMask:HBITMAP=0;
	theMaskHDC:HDC=0;

procedure Outline(dc:hDC;r:TRect);
{ Outline the bitmap position at "r" in "dc". }
var
	saved_pen:hPen;
	saved_brush:hBrush;
begin
	saved_pen:=SelectObject(dc,GetStockObject(White_Pen));
	saved_brush:=SelectObject(dc,GetStockObject(Null_Brush));
	SetRop2(dc,r2_Not);
	with r do Rectangle(dc,left,top,right,bottom);
	SelectObject(dc,saved_brush);
	SelectObject(dc,saved_pen);
end;

procedure SmoothMove(BitMove:boolean;dc,hdcRAM:hDC;rs,rd:TRect;hDCImage,hDCUnder,hdcMask:hDC);
	{ Move a rectangular bitmap without flicker in a "dc" from "rs" to "rd".

		"BitMove" is true if the complete bitmap should be moved
		"dc" is the DC to do the move in
		"rs,rd" are the source and destination rectangles of the move
		"hdcRAM" is a RAM buffer to do the move in
		"hDCImage" is the bitmap to move
		"hDCUnder" is	 the bitmap to restore under the initial position (X1,Y1).
		"hdcMask" is the source bitmap mask to use or 0.

		On exit "hDCUnder" contains the bitmap under the final position. }
var
	ru:TRect;
	Rop:LongInt;
begin
	if not BitMove then begin
		{ erase the outline at the previous position }
		Outline(dc,rs);
		{ draw the outline at the new position }
		Outline(dc,rd);
	end
	else if XpIntersectRect(ru,rs,rd) then begin
		{ do the move in RAM }
		UnionRect(ru,rs,rd);
		{ get the whole thing from the screen }
		BitBlt(hdcRAM,0,0,ru.right-ru.left,ru.bottom-ru.top,dc,ru.left,ru.top,SRCCOPY);
		{ restore what was under the source bitmap position }
		BitBlt(hdcRAM,rs.left-ru.left,rs.top-ru.top,GetRectWd(rs),GetRectHt(rs),hdcUnder,0,0,SRCCOPY);
		{ get what is under the new destination position }
		BitBlt(hdcUnder,0,0,GetRectWd(rs),GetRectHt(rs),hdcRAM,rd.left-ru.left,rd.top-ru.top,SRCCOPY);
		{ place the image at the destination }
		if hdcMask<>0 then begin
			BitBlt(hdcRAM,rd.left-ru.left,rd.top-ru.top,GetRectWd(rs),GetRectHt(rs),hdcMask,0,0,SRCAND);
			Rop:=SRCPAINT;
		end
		else
			Rop:=SRCCOPY;
		BitBlt(hdcRAM,rd.left-ru.left,rd.top-ru.top,GetRectWd(rs),GetRectHt(rs),hdcImage,0,0,Rop);
		{ put the whole thing back to the screen }
		BitBlt(dc,ru.left,ru.top,ru.right-ru.left,ru.bottom-ru.top,hdcRAM,0,0,SRCCOPY);
	end
	else begin
		{ do the move on the screen }
		{ restore what was under the source }
		BitBlt(dc,rs.left,rs.top,GetRectWd(rs),GetRectHt(rs),hdcUnder,0,0,SRCCOPY);
		{ get what is under the destination }
		BitBlt(hdcUnder,0,0,GetRectWd(rs),GetRectHt(rs),dc,rd.left,rd.top,SRCCOPY);
		{ place the image at the destination }
		BitBlt(dc,rd.left,rd.top,GetRectWd(rs),GetRectHt(rs),hdcImage,0,0,SRCCOPY);
	end;
end;

procedure FloatBitmap(X1,Y1,X2,Y2:integer;hbmImage,hbmUnder,hbmMask:HBITMAP);

{ 'Float' a rectangular bitmap visually across the main
	current animation window's client area in a straight line from
	(X1,Y1) to (X2,Y2).

	The bitmap to move is already on the screen at (x1,y1).

	"hbmImage" is the bitmap to move
	"hbmUnder" is	 the bitmap to restore under the initial position (X1,Y1).

	If "hbmMask" is not 0 then the bitmap is not rectangular
	so use this to mask it.

	On exit "hbmUnder" contains the bitmap under the final
	position. }

var
	dc:hDC;
	px,py,nx,ny,xp,yp:integer;
	p:longInt;
	transferType:boolean;
	step:longInt;
	bmd:TBitmap;
	hdcRAM,hdcImage,hdcUnder,hdcMask:HDC;
	hbmRAM,saved_bitmap,hdcMaskBitmap:HBITMAP;
	rs,rd:TRect;

begin
	if (x1=x2) and (y1=y2) then exit;
	dc:=GetDC(hwndAnimate);
	TransferType:=bitmapTransfer; { need a local copy of this global flag }
	GetObject(hbmUnder,SizeOf(bmd),@bmd); { bitmap info }
	if TransferType then begin
		{ get a RAM bitmap big enough to hold 2 of the bitmaps overlapping
			by at exactly 1 pixel. This is the largest RAM buffer you'll need
			to move the image off screen:

			|<-- width -->|=(w-1)*2+1=2w-1
			+------+ 		 -
			|		 | 		 |
			|		 | 		 |
			+------+------+ height=(h-1)*2+1=2h-2+1=2h-1
						 | 	  | |
						 | 	  | |
						 +------+ -}

		hdcRAM:=CreateCompatibleDC(dc);
		hbmRAM:=CreateCompatibleBitmap(dc,bmd.bmWidth*2-1,bmd.bmHeight*2-1);
		saved_bitmap:=SelectObject(hdcRAM,hbmRAM);
	end;
	xp:=x2-x1; { # of points along the X not including the first point }
	yp:=y2-y1; { # of points along the Y not including the first point }
	if transferType then
		step:=Min(Max(abs(xp),abs(yp)),AnimateSteps)
	else
		step:=Min(Max(abs(xp),abs(yp)),AnimateSteps * 2);
	p:=0; { point counter }
	px:=x1;
	py:=y1;
	SetRect(rs,px,py,px+bmd.bmWidth,py+bmd.bmHeight); { source rect }
	{ get a DC for the image underneath the bitmap }
	hdcUnder:=CreateCompatibleDC(dc);
	SelectObject(hdcUnder,hbmUnder);
	{ get a DC for the image that will float across the screen }
	hdcImage:=CreateCompatibleDC(dc);
	SelectObject(hdcImage,hbmImage);
	{ get a DC for the mask if it has one }
	hdcMask:=0; { in case there is no mask }
	if hbmMask<>0 then begin
		hdcMask:=CreateCompatibleDC(dc);
		hdcMaskBitmap:=SelectObject(hdcMask,hbmMask);
	end;
	(*if TransferType then
		{ put the bitmap down in the first position }
		BitBlt(dc,rs.left,rs.top,rs.right-rs.left,rs.bottom-rs.top,hdcImage,0,0,SRCCOPY)
	*)
	if not TransferType then begin
		{ restore the image under the bitmap at the first position }
		BitBlt(dc,rs.left,rs.top,GetRectWd(rs),GetRectHt(rs),hdcUnder,0,0,SRCCOPY);
		{ outline the first rectangle position }
		{SetRop2(dc,r2_Not);}
		Outline(dc,rs);
	end;
	repeat
		inc(p);
		{ calculate the next (x,y) coordinate to move it to }
		nx:=x1+((p*xp) div step);
		ny:=y1+((p*yp) div step);
		SetRect(rd,nx,ny,nx+bmd.bmWidth,ny+bmd.bmHeight); { destination }
		SmoothMove(TransferType,dc,hdcRAM,rs,rd,hdcImage,hdcUnder,hdcMask);
		rs:=rd; { next source rect is current dest rect }
		{ make the current point the next point and do again }
		px:=nx;
		py:=ny;
		Delay(6);
	until p=step;
	if TransferType then begin
		{ dispose of the video RAM buffer }
		SelectObject(hdcRAM,saved_bitmap); { restore old one }
		DeleteObject(hbmRAM);
		DeleteDC(hdcRAM);
	end
	else begin
		Outline(dc,rd);
		{ get the image under the final position }
		BitBlt(hdcUnder,0,0,GetRectWd(rs),GetRectHt(rs),dc,rs.left,rs.top,SRCCOPY);
		{ put the bitmap down over the final outline position }
		BitBlt(dc,rs.left,rs.top,rs.right-rs.left,rs.bottom-rs.top,hdcImage,0,0,SRCCOPY);
	end;
	if hbmMask<>0 then begin
		SelectObject(hdcMask,hdcMaskBitmap);
		DeleteDC(hdcMask);
	end;
	SelectObject(hdcImage, NULL_BITMAP);
	DeleteDC(hdcImage);
	SelectObject(hdcUnder, NULL_BITMAP);
	DeleteDC(hdcUnder);
	ReleaseDC(hwndAnimate,dc);
end;

procedure DragStart(X,Y:integer;hbmImage,hbmUnder,hbmMask:HBITMAP);
var
	dc:hDC;
begin
	dc:=GetDC(hwndAnimate);
	GetObject(hbmUnder,SizeOf(bmd),@bmd); { bitmap info }
	if BitmapTransfer then begin
		hdcRAM:=CreateCompatibleDC(dc);
		hbmRAM:=CreateCompatibleBitmap(dc,bmd.bmWidth*2-1,bmd.bmHeight*2-1);
		saved_bitmap:=SelectObject(hdcRAM,hbmRAM);
	end;
	SetRect(rs,x,y,x+bmd.bmWidth,y+bmd.bmHeight); { source rect }
	theGrabMask:=hbmMask;
	if theGrabMask<>0 then begin
		theMaskHDC:=CreateCompatibleDC(dc);
		SelectObject(theMaskHDC,theGrabMask);
	end;
	{ get a DC for the image underneath the bitmap }
	hdcUnder:=CreateCompatibleDC(dc);
	SelectObject(hdcUnder, hbmUnder);
	{ get a DC for the image that will float across the screen }
	hdcImage:=CreateCompatibleDC(dc);
	SelectObject(hdcImage, hbmImage);
	if not BitmapTransfer then begin
		{ restore the image under the bitmap at the first position }
		BitBlt(dc,rs.left,rs.top,GetRectWd(rs),GetRectHt(rs),hdcUnder,0,0,SRCCOPY);
		{ outline the first rectangle position }
		{SetRop2(dc,r2_Not);}
		Outline(dc,rs);
	end;
	{Drag_RestoreCursor:=SetCursor(LoadCursor(hInstance,MakeIntResource(idc_Drag)));}
	ReleaseDC(hwndAnimate,dc);
end;

procedure DragBitmap(x,y:integer);
var
	dc:hDC;
	rd:TRect;
begin
	dc:=GetDC(hwndAnimate);
	if (rs.left<>x) or (rs.top<>y) then begin
		SetRect(rd,x,y,x+bmd.bmWidth,y+bmd.bmHeight);
		SmoothMove(BitmapTransfer,dc,hdcRAM,rs,rd,hdcImage,hdcUnder,theMaskHDC);
		rs:=rd;
	end;
	ReleaseDC(hwndAnimate,dc);
end;

procedure DragEnd;
var
	dc:hDC;
begin
	dc:=GetDC(hwndAnimate);
	if BitmapTransfer then begin
		{ dispose of the video RAM buffer }
		SelectObject(hdcRAM,saved_bitmap); { restore old one }
		DeleteObject(hbmRAM);
		DeleteDC(hdcRAM);
	end
	else begin
		Outline(dc,rs);
		{ get the image under the final position }
		BitBlt(hdcUnder,0,0,GetRectWd(rs),GetRectHt(rs),dc,rs.left,rs.top,SRCCOPY);
		{ put the bitmap down over the final outline position }
		BitBlt(dc,rs.left,rs.top,rs.right-rs.left,rs.bottom-rs.top,hdcImage,0,0,SRCCOPY);
	end;
	if theGrabMask<>0 then begin
		DeleteDC(theMaskHDC);
		theMaskHDC:=0;
	end;
	SelectObject(hdcImage,0);
	DeleteDC(hdcImage);
	hdcImage:=0;
	SelectObject(hdcUnder, NULL_BITMAP);
	DeleteDC(hdcUnder);
	hdcUnder:=0;
	ReleaseDC(hwndAnimate,dc);
end;

function CardWd:integer;
begin
	CardWd:=CardGraphicsManager.Instance^.CurrentWidth;
end;

function CardHt:integer;
begin
	CardHt:=CardGraphicsManager.Instance^.CurrentHeight;
end;

constructor TheCardGraphicsManager.Construct(aCardFactory:ICardFactory); 
begin
	myCardFactory:=aCardFactory;
end;

procedure TheCardGraphicsManager.SelectWidth(in_pixels:word);
begin
	SelectDesiredCardWidth(in_pixels);
end;

function TheCardGraphicsManager.LargestThatFits(n_things:word; space_between:integer; tableSize:word; edge_margin:integer; checkForWidth:boolean):word; 
var
	i:word;
	function WorH(aIndex:number):word;
	begin
		if checkForWidth 
			then WorH:=myCardFactory^.SupportedWidthAt(aIndex) 
			else WorH:=myCardFactory^.SupportedHeightAt(aIndex);
	end;
begin 
	i:=myCardFactory^.SupportedSizeCount;
	while (i>1) and ((WorH(number(i))*n_things+(n_things*space_between-space_between))>(tableSize-edge_margin*2)) do Dec(i);
	LargestThatFits:=myCardFactory^.SupportedWidthAt(number(i));
end;

function TheCardGraphicsManager.BestColumnFit(nPiles:word;space_between:integer;total_width:word;edge_margin:integer):word; 
begin
	BestColumnFit:=LargestThatFits(nPiles,space_between,total_width,edge_margin,TRUE); 
end;

function TheCardGraphicsManager.BestRowFit(nPiles:word;space_between:integer;total_height:word;edge_margin:integer):word; 
begin
	BestRowFit:=LargestThatFits(nPiles,space_between,total_height,edge_margin,FALSE);
end;

function TheCardGraphicsManager.BestFit(n_columns,n_rows:word;space_between:integer;total_width,total_height:word;edge_margin:integer):word;
begin
	BestFit:=MinW(BestColumnFit(n_columns,space_between,total_width,edge_margin),BestRowFit(n_rows,space_between,total_height,edge_margin));
end;

function TheCardGraphicsManager.CurrentWidth:word;
begin
	CurrentWidth:=CardImageWd;
end;

function TheCardGraphicsManager.CurrentHeight:word;
begin
	CurrentHeight:=CardImageHt;
end;

function CurrentWidth:word;
begin
	CurrentWidth:=CardGraphicsManager.Instance^.CurrentWidth;
end;

function CurrentHeight:word;
begin
	CurrentHeight:=CardGraphicsManager.Instance^.CurrentHeight;
end;

function BestFit(n_columns,n_rows:word;space_between:integer;total_width,total_height:word;edge_margin:integer):word;
begin
	BestFit:=CardGraphicsManager.Instance^.BestFit(n_columns,n_rows,space_between,total_width,total_height,edge_margin);
end;

procedure SelectWidth(in_pixels:word);
begin
	CardGraphicsManager.Instance^.SelectWidth(in_pixels);
end;

procedure UpdateCardSize(newWidth,newHeight:word;var aSelector:winqcktbl.Game;manager:ICardGraphicsManager);
var
	wd:word;
begin
	wd:=manager^.BestFit(aSelector.PileColumns,aSelector.PileRows,aSelector.PileSpacing,newWidth,newHeight,MIN_EDGE_MARGIN);
	if wd<>CurrentWidth then manager^.SelectWidth(wd);
end;

constructor CardSizingMonitor.Construct;
begin
	inherited Construct(0, 0);
end;

function CardSizingMonitor.GetAnchorPoint(table_width,table_height:word):xypair;
begin
	UpdateCardSize(table_width, table_height, PTabletop(MyTabletop)^.MyGame^, CardGraphicsManager.Instance);
	GetAnchorPoint:=MakeXYPair(0, 0);
end;

constructor OTabletop.Construct(background_color:TColorRef;background_image:HBITMAP;use_image:boolean);
begin
	inherited Construct;
	PostConstruct;
	CapturedReleaseTarget:=NIL;
	Initialize(background_color,background_image,use_image);
end;

function OTabletop.OnTapped:boolean;
begin
	OnTapped:=FALSE;
end;

function OTabletop.OnDoubleTapped(x,y:integer):boolean;
begin
	OnDoubleTapped:=FALSE;
end;

procedure OTabletop.Initialize(background_color:TColorRef;background_image:HBITMAP;use_image:boolean);
begin
	UseBgImage:=use_image;
	self.background_image:=background_image; 
end;

function OTabletop.OnMouseMove(keys:uint;x,y:integer):LONG;
begin
	if isDragging then begin
		DragBitmap(x-DragOrg.x,y-DragOrg.y);
	end;
	OnMouseMove:=0;
end;

procedure OTabletop.Paint(PaintDC:hDC; var PaintInfo:TPaintStruct);
var
	mDC:HDC;
	oBM,mBM:HBITMAP;
begin //writeln('OTabletop.Paint(', LongInt(PaintDC), ',var PaintInfo:TPaintStruct)');
	mDC:=CreateCompatibleDC(PaintDC);
	mBM:=CreateCompatibleBitmap(PaintDC,GetRectWd(PaintInfo.rcPaint),GetRectHt(PaintInfo.rcPaint));
	oBM:=SelectObject(mDC,mBM);
	Render(mDC, PaintInfo.rcPaint);
	BitBlt(PaintDC,PaintInfo.rcPaint.left, PaintInfo.rcPaint.top, GetRectWd(PaintInfo.rcPaint),GetRectHt(PaintInfo.rcPaint),mDC,0,0,SRCCOPY);
	SelectObject(mDC,oBM);
	DeleteObject(mBM);
	DeleteDC(mDC);
end;

function OTabletop.MyFrame:HWND;
begin
	MyFrame:=self.frame;
end;

procedure OTabletop.RenderTabletopSurface(dc:HDC;xOffset,yOffset:integer);
var
	memDC:HDC;
	tbm:TBitmap;
	R:Trect;
begin //writeln('OTabletop.RenderTabletopSurface(HDC,',xOffset,',',yOffset,');');
	GetClientRect(R);
	if TabletopBrush=0 then TabletopBrush:=CreateSolidBrush(x_table_top_color);
	UnrealizeObject(TabletopBrush);
	SetBrushOrgEx(dc, xOffset mod 8, yOffset mod 8, nil);
	FillRect(dc,R,TabletopBrush);
	if UseBgImage and (IsValidHandle(background_image)) then begin
		GetObject(background_image,SizeOf(tbm),@tbm);
		memDC:=CreateCompatibleDC(dc);
		SelectObject(memDC,background_image);
		SetStretchBltMode(dc, HALFTONE);
		StretchBlt(dc, 0, 0, Width, Height, memDC, 0, 0, tbm.bmWidth, tbm.bmHeight, SRCCOPY);
		DeleteDC(memDC);
	end;
end;

procedure OTabletop.SetBackground(color:TColorRef;bitmap:HBITMAP;use_image:boolean);
begin //writeln('OTabletop.SetBackground(color:TColorRef;bitmap:HBITMAP;use_image:boolean)');
	UseBgImage:=use_image;
	MakeNewTTColor(color);
	background_image:=bitmap;
	DeleteObject(tabletop_image);
	tabletop_image:=CreateTabletopBitmap(Width, Height);
end;

function OTabletop.BgImage:HBITMAP;
begin
	BgImage:=background_image;
end;

function OTabletop.BgColor:TColorRef;
begin
	BgColor:=x_table_top_color;
end;

function OTabletop.Create(frame:HWND;w,h:number):HWND;
begin //writeln('OTabletop.Create(frame,',w,',',h,');');
	self.frame:=frame;
	Create:=OWindow.Create('Tabletop',WS_CHILD or WS_VISIBLE or WS_CLIPCHILDREN,0,0,w,h,frame,191,hInstance,NIL);
	hwndAnimate:=self.handle;
	player_prompt.Construct(@self);
end;

procedure Game.Initialize(tabletop:PTabletop);
begin
	MyTabletop:=tabletop;
	tabletop^.MyGame:=@self;
	tabletop^.AddProp(new(CardSizingMonitorP, Construct));
end;

constructor Game.Construct(tabletop:PTabletop);
begin
	inherited Construct;
	Initialize(tabletop);
end;

constructor Game.Construct(n:gameIndex;tabletop:PTabletop);
begin
	inherited Construct(n);
	Initialize(tabletop);
end;

function Game.PileSpacing:integer;
begin
	PileSpacing:=5;
end;

function Game.PileRows:word;
begin
	PileRows:=4;
end;

function Game.PileColumns:word; 
begin
	PileColumns:=6;
end;

function CardCenterToAnchor(center_point:xypair):xypair;
begin
	CardCenterToAnchor:=MakeXYPair(xypairWrapper(center_point).x-(CurrentWidth div 2), xypairWrapper(center_point).y-(CurrentHeight div 2));
end;

function CardAnchorToCenter(anchor_point:xypair):xypair;
begin
	CardAnchorToCenter:=MakeXYPair(xypairWrapper(anchor_point).x+(CurrentWidth div 2), xypairWrapper(anchor_point).y+(CurrentHeight div 2));
end;

constructor OCardpileProp.Construct;
begin
	Construct(52);
end;

constructor OCardpileProp.Construct(n:number);
begin
	inherited Construct(@my_pile,0,0,0);
	my_pile.Construct(n);
end;

constructor OCardpileProp.Construct(n:number; where:relativeposition);
begin
	Construct(n);
	self.relative_position:=where;
end;

destructor OCardpileProp.Destruct;
begin //Writeln('OCardpileProp.Destruct');
	my_pile.Destruct;
	inherited Destruct;
end;

function OCardpileProp.CardCount:quantity;
begin
	CardCount:=Size;
end;

procedure OCardpileProp.DealCardTo(n:number;target:OCardpileProp_ptr);
begin
	CardAtTo(n,target);
end;

function OCardpileProp.MoveCardTo(n:number;center_point:xypair;table:PTabletop;animate:boolean):OCardpileProp_ptr;
var
	pile:OCardpileProp_ptr;
begin
	pile:=OCardpileProp_ptr(table^.AddProp(New(OCardpileProp_ptr,Construct(52)),CardCenterToAnchor(center_point)));
	if animate then CardAtTo(n,pile);
	MoveCardTo:=pile;
end;

function OCardpileProp.DealCardTo(n:number;center_point:xypair;table:PTabletop):OCardpileProp_ptr;
begin
	DealCardTo:=MoveCardTo(n,center_point,table,TRUE);
end;

procedure OCardpileProp.SlideTo(center_x,center_y:integer);
begin
	SlideTo(xypair(MakeXYPair(center_x,center_y)));
end;

procedure OCardpileProp.SlideTo(center_point:xypair);
var
	pile:OCardpileProp_ptr;
begin
	// hack until I can implement a real "Slide" function
	with PTabletop(MyTabletop)^ do begin
		pile:=OCardpileProp_ptr(AddProp(New(OCardpileProp_ptr,Construct(52)),CardCenterToAnchor(center_point)));
		CardAtTo(1,pile);
		Hide;
		Hotlist.Free(@self);
	end;
	//MoveTo(xypairWrapper(center_point).x,xypairWrapper(center_point).y);
end;

procedure OCardpileProp.Fan(dx,dy:integer;n:word);
var 
	i:word;
begin
	for i:=1 to n do begin
		self.SetCardDx(CardDx+dx);
		self.SetCardDy(CardDy+dy);
		self.Refresh;
	end;
end;

procedure OCardpileProp.FanLeft;
begin
	self.Fan(-1,0,PipHSpace);
end;

procedure OCardpileProp.FanRight;
begin
	self.Fan(1,0,PipHSpace);
end;

procedure OCardpileProp.Collapse;
begin
	self.Hide;
	self.SetCardDx(0);
	self.SetCardDy(0);
	self.Show;
end;

procedure Hotspot.PostConstruct(w,h:integer);
begin //writeln('Hotspot.PostConstruct(',w,',',h,') | ', self.ToString);
	myTabletop:=NIL;
	Anchor.X:=0;
	Anchor.Y:=0;
	hrFlags:=0;
	relative_position:=CENTER_CENTER;
	FillChar(propwarts, SizeOf(propwarts), #0);
	SetRect(current_span, 0, 0, w, h);
end;

constructor Hotspot.Construct;
begin
	inherited Construct;
	PostConstruct(0, 0);
end;

constructor Hotspot.Construct(w,h:integer);
begin //writeln('Hotspot.Construct(',w,',',h,')');
	inherited Construct;
	PostConstruct(w, h);
end;

constructor Hotspot.Construct(w,h:integer;properties:flags);
begin
	inherited Construct(FALSE);
	PostConstruct(w,h);
end;

function Hotspot.OnCapturedRelease(dx,dy:integer):boolean; 
begin
	OnCapturedRelease:=FALSE;
end;

function Hotspot.GetOwnerWnd:HWND;
begin
	GetOwnerWnd:= myTabletop^.handle;
end;

function Hotspot.Width:word;
begin
	Width:=Word(GetRectWd(current_span));
end;

function Hotspot.GetWidth:word;
begin
	GetWidth:=Width;
end;

function Hotspot.Height:word;
begin 
	Height:=Word(GetRectHt(current_span));
end;

function Hotspot.GetHeight:word;
begin
	GetHeight:=Height;
end;

function Hotspot.ToString:string;
begin
	ToString:=
		'Anchor:(X:'+NumberToString(self.Anchor.x)+',Y:'+NumberToString(self.Anchor.y)+')'+
		';Span:('+DumpToString(self.current_span)+')';
end;

procedure Hotspot.Refresh;
var
	union,r,intersection:TRect;
begin //Writeln('Hotspot.Refresh|', self.ToString);
	r:=current_span;
	GetSpanRect(current_span);
	UnionRect(union, r, current_span);
	if myTabletop<>NIL then begin
		union:=current_span;
		if XpIntersectRect(intersection, r, current_span) 
			then UnionRect(union, r, current_span) 
			else myTabletop^.RefreshRect(r);
		myTabletop^.RefreshRect(union);
	end;
end;

procedure Hotspot.Redraw(dc:HDC; x,y:integer); 
begin 
end;

function Hotspot.PointIn(x:integer; y:integer):boolean;
var
	Pt:TPoint;
begin
	Pt.X:= x;
	Pt.Y:= y;
	GetSpanRect(current_span);
	PointIn:=PtInRect(current_span, Pt);
end;

function Hotspot.HitTest(dx,dy:integer):boolean;
begin
	HitTest:=IsEnabled and PointIn(dx,dy);
end;

function OTabletop.OnLButtonDblClick(keys:UINT;x,y:integer):LONG;
begin //writeln('OTabletop.OnLButtonDblClick(keys:UINT;x,y:integer)');
	OnLButtonDblClick:=Q(OnDoubleTapped(x,y), 0, 1);
end;

function OTabletop.OnLButtonDown(keys:uint;x,y:integer):LONG;
var
	P:PHotspot;
	function Matches(Item:Pointer):boolean; begin Matches:=PHotspot(Item)^.HitTest(x,y); end;
begin //writeln('OTabletop.OnLButtonDown(keys=',keys,',',x,',',y,')');
	P:=PHotspot(HotList.LastThat(@Matches));
	if (P<>nil) and P^.IsEnabled and P^.OnPressed(x-P^.Left,y-P^.Top) then begin
		OnLButtonDown:=WM_MESSAGE_PROCESSED;
		Exit;
	end;
	OnLButtonDown:=inherited OnLButtonDown(keys,x,y);
end;

function OTabletop.OnRButtonDown(keys:uint;x,y:integer):LONG;
var
	P:Pointer;
	function Matches(Item:Pointer):boolean;
	begin
		Matches:=PHotspot(Item)^.HitTest(x,y);
	end;
begin
	P:=HotList.LastThat(@Matches);
	if (P<>nil) and (PHotspot(P)^.isEnabled) then begin
		with PHotspot(P)^ do RMousePress(x-Left,y-Top);
	end;
	OnRButtonDown:=0;
end;

var
	grabbedPt:TPoint;

procedure OTabletop.SetCapturedReleaseTarget(target:PHotspot);
begin
	SetCapture(handle);
	self.CapturedReleaseTarget:=target;
end;

procedure OTabletop.ResetCapturedReleaseTarget;
begin
	self.CapturedReleaseTarget:=NIL;
	ReleaseCapture;
end;

function OTabletop.OnLButtonUp(keys:uint;x,y:integer):LONG;
var
	P:Pointer;
	function Matches(Item:Pointer):boolean; begin Matches:=PHotspot(Item)^.HitTest(x,y); end;
begin //writeln('OTabletop.OnLButtonUp(',keys,',',x,',',y,')');
	OnLButtonUp:=0;
	if (CapturedReleaseTarget<>NIL) then with CapturedReleaseTarget^ do begin
		OnCapturedRelease(x-Left,y-Top);
		Exit;
	end;
	P:=HotList.LastThat(@Matches);
	if (P=NIL) and IsDragging then begin
		DragEnd;
		grabbed_from^.DropOnTop(Integer(x) - DragOrg.x, Integer(y) - dragOrg.y);
		IsDragging:=FALSE;
	end;
	if (P<>NIL) then with PHotspot(P)^ do OnReleased(x-Left,y-Top);
end;

procedure Hotspot.OnEnabled; begin { place holder } end;

procedure Hotspot.Enable;
begin
	hrFlags:= hrFlags or HR_ENABLED;
	OnEnabled;
end;

procedure Hotspot.Disable;
begin
	hrFlags:= hrFlags and (not HR_ENABLED);
end;

function Hotspot.enabled(const p_state:boolean):boolean;
begin
	enabled:= ((hrFlags and HR_ENABLED) <> 0);
	if p_state then
		enable
	else
		disable;
end;

procedure Hotspot.RMousePress(dx,dy:integer); begin { place holder } end;

function Hotspot.OnPressed(dx,dy:integer):boolean; 
begin 
	OnPressed:=FALSE; 
end;

function Hotspot.OnReleased(dx,dy:integer):boolean; 
begin 
	OnReleased:=FALSE; 
end;

function Hotspot.IsEnabled:boolean;
begin
	IsEnabled:=((hrFlags and HR_ENABLED)<>0);
end;

function Hotspot.OverRectangle(const rSrcRect:TRect):boolean;
var
	rc:TRect;
begin
	OverRectangle:= XpIntersectRect(rc, rSrcRect, current_span);
end;

procedure OTabletop.PostConstruct;
begin //writeln('OTabletop.PostConstruct');
	HotList.Init(10,10);
	tabletop_image:=NULL_HANDLE;
end;

procedure OTabletop.Render(target_DC:HDC; const rSrcRect:TRect);
{ Note: DC size (w,h) must match size of rect }
var
	i:integer;
	temp_dc:HDC;
	bitmap:HBITMAP;
	{$ifdef SHOW_HOTSPOTS}
	procedure Dispatch(Item:Pointer);
	var
		buf:stringBuffer;
	begin
		with Hotspot(Item^) do if Width>0 then begin
			FillRect(target_DC, rSrcRect, GetStockObject(Q(IsVisible,WHITE_BRUSH,LTGRAY_BRUSH)));
			StrPCopy(buf, NumberToString(current_span.left)+','+NumberToString(current_span.top));
			TextOut(target_DC, rSrcRect.left+1, rSrcRect.top+1, buf, strlen(buf));
		end;
	end;
	{$endif}
begin //writeln('OTabletop.Render(', target_DC, ',', DumpToString(rSrcRect),')');
	System.Assert(target_DC<>NULL_HANDLE);
	System.Assert(tabletop_image<>NULL_BITMAP);
	temp_dc:=CreateCompatibleDC(target_DC);
	bitmap:=SelectObject(temp_dc, tabletop_image);
	BitBlt(target_DC, 0, 0, GetRectWd(rSrcRect), GetRectHt(rSrcRect), temp_dc, rSrcRect.left, rSrcRect.top, SRCCOPY);
	{$ifdef SHOW_HOTSPOTS} self.HotList.ForEach(@Dispatch); {$endif}
	SelectObject(temp_dc, bitmap);
	DeleteDC(temp_dc);
	with hotList do for i:=0 to count-1 do with PHotspot(At(i))^ do if (IsVisible and OverRectangle(rSrcRect)) then Redraw(target_DC, anchor.x - rSrcRect.left, anchor.y - rSrcRect.top);
end;

procedure Hotspot.SetStickyPos(where:relativeposition);
begin
	case where of
		BOTTOM_LEFT:begin
			Anchor.x:=myTabletop^.Margin;
			Anchor.y:=myTabletop^.Bottom-Height;
		end;
		BOTTOM_CENTER:begin
			Anchor.x:=Center(Width,0,myTabletop^.ClientAreaWd);
			Anchor.y:=myTabletop^.Bottom-Height;
		end;
	end;
	GetSpanRect(current_span);
end;

destructor Hotspot.Destruct;

begin //writeln('Hotspot.Destruct()');
end;

function Hotspot.Top:integer;
begin
	Top:=current_span.top;
end;

function Hotspot.Bottom:integer;
begin
	Bottom:=current_span.bottom;
end;

function Hotspot.Left:integer;
begin
	Left:=current_span.Left;
end;

function Hotspot.Right:integer;
begin
	Right:=current_span.Right;
end;

procedure Hotspot.SetPosition(x,y:integer);
begin //writeln('Hotspot.SetPosition(', x, ',', y,')');
	OffsetRect(current_span, x-Anchor.x, y-Anchor.y);
	Anchor.x:=x;
	Anchor.y:=y;
	if MyTabletop<>NIL then SetRelativeOffset(myTabletop^.Width, myTabletop^.Height);
end;

procedure Hotspot.SetPosition(point:xypair);
begin //writeln('Hotspot.SetPosition({',point.x,',',point.y,'})');
	SetPosition(xypairWrapper(point).x, xypairWrapper(point).y);
end;

function Hotspot.ObjectClass:HotspotClass;
begin
	ObjectClass:=HCLASS.BASE;
end;

function GenPileOfCards.ObjectClass:HotspotClass;
begin
	{$ifdef DEBUG}
	if inherited ObjectClass <> HCLASS.BASE then Halt; { sanity check }
	{$endif}
	ObjectClass:= HCLASS.GENPILEOFCARDS;
end;

function OTabletop.CreateTabletopBitmap(w,h:integer):HBITMAP;
var
	dc,temp_dc:HDC;
	bitmap,tabletop_bitmap:HBITMAP;
begin //writeln('OTabletop.CreateTabletopBitmap(',w,',',h,')');
	dc:=GetDC(handle);
	tabletop_bitmap:=CreateCompatibleBitmap(dc, w, h);
	temp_dc:=CreateCompatibleDC(dc);
	ReleaseDC(handle,dc);
	bitmap:=SelectObject(temp_dc, tabletop_bitmap);
	RenderTabletopSurface(temp_dc, 0, 0);
	SelectObject(temp_dc, bitmap);
	DeleteDC(temp_dc);
	CreateTabletopBitmap:=tabletop_bitmap;
end;

function OTabletop.OnSize(resizeType:UINT;newWidth,newHeight:integer):LONG;
	procedure Invoke(Item:Pointer);
	begin
		with PHotspot(Item)^ do SetPosition(GetAnchorPoint(Word(newWidth),Word(newHeight)));
	end;
begin //writeln('OTabletop.OnSize(resizeType,', newWidth, ',', newHeight,')');
	if (resizeType=SIZE_RESTORED) or (resizeType=SIZE_MAXIMIZED) then begin
//		System.Assert(tabletop_image<>NULL_BITMAP);
		if (GetBitmapWd(tabletop_image)<>newWidth) or (GetBitmapHt(tabletop_image)<>newheight) then begin
			DeleteObject(tabletop_image);
			tabletop_image:=CreateTabletopBitmap(newWidth, newHeight);
		end;
		if player_prompt.handle<>NULL_HANDLE then player_prompt.Reposition();
		HotList.ForEach(@Invoke);
	end;
	OnSize:=0;
end;

function GenPileOfCards.CardDy:integer;
begin
	CardDy:= my_card_dy;
end;

function GenPileOfCards.CardDx:integer;
begin
	CardDx:= my_card_dx;
end;

procedure GenPileOfCards.SetCardDx(aCardDx:integer);
begin
	my_card_dx:= aCardDx;
end;

procedure GenPileOfCards.SetCardDy(aCardDy:integer);
begin
	my_card_dy:= aCardDy;
end;

procedure GenPileOfCards.SetDesc(const pText:pchar);
begin
	if Desc <> nil then begin
		StrDispose(Desc);
	end;
	Desc:= StrNew(pText);
end;

procedure GenPileOfCards.AppendDesc(const pText:pchar);
var
	pNew:pchar;
	function EndOfSentence(pText:pchar):boolean;
	begin
		EndOfSentence:= StrLastChar(pText) = '.'
	end;
begin
	if pText <> nil then begin
		if Desc = nil
			then
				SetDesc(pText)
			else begin
				pNew:= StrAlloc(StrLen(Desc) + StrLen(pText) + Q(EndOfSentence(Desc), 1, 0));
				StrCopy(pNew, Desc);
				if EndOfSentence(Desc) then StrCat(pNew, ' ');
				StrCat(pNew, pText);
				Desc:= pNew;
			end
	end;
end;

procedure Hotspot.GetSpanRect(var rRect:TRect);
begin
	rRect.left:=Anchor.x;
	rRect.top:=Anchor.y;
	rRect.right:=rRect.left+GetWidth;
	rRect.bottom:=rRect.top+GetHeight;
end;

procedure Hotspot.Selected;
begin
	{ placeholder }
end;

function IsDerivedFromGenPileOfCards(aHotSpotPtr:PHotspot):boolean;
begin
	IsDerivedFromGenPileOfCards:= aHotSpotPtr^.ObjectClass = HCLASS.GENPILEOFCARDS;
end;

procedure GenPileOfCards.TryTopcardToDblClkTargets;
var
	aPtr:Pointer;

	function Matches(a_pItem:Pointer):boolean;
	var
		aHotSpotPtr:PHotspot;
		pPile:GenPileOfCardsP;
	begin
		aHotSpotPtr:= PHotspot(a_pItem);
		if IsDerivedFromGenPileOfCards(aHotSpotPtr) then begin
			pPile:= GenPileOfCardsP(aHotSpotPtr);
			if pPile^.IsDblClkTarget and pPile^.Accepts(Topcard) then begin
				Matches:= true;
			end
			else begin
				Matches:= false;
			end;
		end
		else begin
			Matches:= false;
		end;
	end;

begin
	aPtr:= PTabletop(myTabletop)^.HotList.LastThat(@Matches);
	if Assigned(aPtr) then TopcardTo(GenPileOfCardsP(aPtr));
end;

function GenPileOfCards.OnTopcardTapped:boolean;
begin
	OnTopcardTapped:=FALSE;
end;

procedure GenPileOfCards.TopSelected;
begin
	if TopFaceup then TryTopcardToDblClkTargets;
end;

function GenPileOfCards.OnReleased(dx,dy:integer):boolean;
var
	hit:integer;
begin //writeln('GenPileOfCards.OnReleased(',dx,',',dy,')');
	if isDragging then begin
		dragEnd;
		if (@self<>grabbed_from) and Accepts(grabbedCard and (not FACEUP_BIT))
			then dropOntop(Left + dx - dragOrg.x, Top + dy - dragOrg.y)
			else grabbed_from^.dropOntop(Left + dx - dragOrg.x, Top + dy - dragOrg.y);
		isDragging:= false;
	end
	else if IsEmpty
		then Selected
		else begin
			hit:=PointHitsCard(dx,dy);
			if (hit>0) and CanSelectCardAt(hit) then begin
				if hit=Size then begin
					TopSelected;
					if not OnTopcardTapped then OnCardAtTapped(hit);
				end
				else OnCardAtTapped(hit);
			end
		end;
	OnReleased:=FALSE;
end;

procedure OTabletop.RefreshRect(const rRect:TRect);
begin //writeln('OTabletop.RefreshRect(',DumpToString(rRect),')');
	InvalidateRect(Handle, rRect, TRUE);
	UpdateWindow;
end;

procedure Hotspot.RefreshRect(const rPrevSpan:TRect);
var
	union:TRect;
begin //writeln('Hotspot.RefreshRect(',DumpToString(rPrevSpan),')');
	GetSpanRect(current_span);
	UnionRect(union, rPrevSpan, current_span);
	myTabletop^.RefreshRect(union);
end;

function GenPileOfCards.CardIsPlaceHolder(aIndex:integer):boolean;
begin
	CardIsPlaceHolder:=cards.CardIsPlaceHolder(CardAt(aIndex));
end;

function GenPileOfCards.CardIsCovered(aIndex:integer):boolean;
	function CardsAboveIt:boolean;
	var
		n:integer;
	begin
		CardsAboveIt:= false;
		for n:= aindex + 1 to Size do
			if not CardIsPlaceHolder(n) and (GetCardX(aindex) = GetCardX(n)) and (GetCardY(aIndex) = GetCardY(n)) then begin
				CardsAboveIt:= true;
				break;
			end;
	end;
begin
	CardIsCovered:= (aIndex < Size) and CardsAboveIt;
end;

function OTabletop.ClientAreaWd:integer;
begin
	ClientAreaWd:=Integer(GetClientWd(handle));
end;

function OTabletop.ClientAreaHt:integer;
begin
	ClientAreaHt:=Integer(GetClientHt(handle));
end;

procedure GenPileOfCards.OnCardAdded; 
begin 
	PlaceHolder; 
end;

procedure Hotspot.SnapTo(new_position:xypair);
begin
	Hide;
	SetPosition(new_position);
	Show;
end;

function Hotspot.GetAnchorPoint(table_width,table_height:word):xypair;
begin //writeln('Hotspot.GetAnchorPoint(',table_width,',',table_height,') [Anchor:(',Anchor.x,',',Anchor.y,')]');
	SetStickyPos(relative_position);
	GetAnchorPoint:=MakeXYPair(Anchor.x,Anchor.y);
end;

function GenPileOfCards.OnCardAtTapped(n:number):boolean;
begin //writeln('GenPileOfCards.OnCardAtTapped(',n,')');
	OnCardAtTapped:=FALSE;
end;

function MakeTPoint(x,y:integer):TPoint;

var
	point:TPoint;
	
begin
	point.x:=x;
	point.y:=y;
	MakeTPoint:=point
end;

function GenPileOfCards.PointHitsCard(dx,dy:integer):integer;

var
	card:word;
	
	function HitsCard(i:number):boolean;
	
	begin
		HitsCard:=PtInRect(GetCardAtRect(i),MakeTPoint(dx,dy));
	end;
	
begin
	PointHitsCard:=0;
	if Size>0 then for card:=Size downto 1 do if HitsCard(card) then begin
		PointHitsCard:=Card;
		break;
	end;
end;

function ConvertPauseToMillSeconds(pause:pauseFactor):quantity;
begin
	ConvertPauseToMillSeconds:=(BASEDELAY*pause);
end;

function GenPileOfCards.GetCardAtRect(n:number):TRect;
var
	r:TRect;
begin
	r.left:=CardOffsetX(n);
	r.top:=CardOffsetY(n);
	r.right:=r.left+CardWd;
	r.bottom:=r.top+CardHt;
	GetCardAtRect:=r;
end;

function OTabletop.AddProp(prop:PHotspot; anchor_point:xypair):PHotspot;
var
	loc:relativeposition;
begin //writeln('OTabletop.AddProp(prop=(',prop^.ToString,'),',xypairwrapper(anchor_point).x,',',xypairwrapper(anchor_point).y,')');
	prop^.myTabletop:=@self;
	Hotlist.Insert(prop);
	prop^.SetPosition(anchor_point);
	prop^.SetRelativeOffset(Width, Height);
	prop^.Refresh;
	with prop^ do for loc:=Low(relativeposition) to High(relativeposition) do if (propwarts[loc]<>NIL) then AddProp(propwarts[loc]);
	AddProp:=prop;
end;

function OTabletop.AddProp(prop:PHotspot):PHotspot;
begin
	prop^.myTabletop:=@self;
	AddProp:=AddProp(prop, prop^.GetAnchorPoint(Width, Height));
end;

function OTabletop.AddProp(prop:PHotspot; where:relativeposition):PHotspot;
begin
	prop^.relative_position:=where;
	AddProp(prop);
end;

procedure GenPileOfCards.TransferTo(target:GenPileOfCardsP);
begin
	target^.OnDragging;
	GrabCardsAt(1);
	target^.GetGrabbed;
	Delay(DealDelay*2);
	PTabletop(myTabletop)^.HotList.Free(@self);
end;

function Hotspot.OffsetFromCenter:xypair;
begin
	OffsetFromCenter:=self.offset_from_center;
end;

procedure Hotspot.SetRelativeOffset(table_w,table_h:number);
begin
	xypairWrapper(self.offset_from_center).x:=Anchor.x-(table_w div 2);
	xypairWrapper(self.offset_from_center).y:=Anchor.y-(table_h div 2);
end;

function TCollectionOfHotspots.GetItem(var S:TStream):pointer;
begin
	{Unimplemented;}
	GetItem:=nil;
end;

procedure TCollectionOfHotspots.PutItem(var S:TStream; Item:Pointer);
begin
	{Unimplemented;}
end;

procedure TCollectionOfHotspots.FreeItem(Item:Pointer);
begin
	if Item<>nil then PHotspot(Item)^.Destruct;
end;

function OTabletop.Margin:integer;
begin
	Margin:=MIN_EDGE_MARGIN;
end;

function OTabletop.Width:word;
begin
	Width:=GetClientWd(handle);
end;

function OTabletop.Height:word;
begin
	Height:=GetClientHt(handle);
end;

function OTabletop.Bottom:integer;
begin
	Bottom:=Integer(Height)-Margin;
end;

function OTabletop.Top:integer;
begin
	Top:=Margin;
end;

procedure TCollectionOfHotspots.DiscardLast;
begin
	AtFree(Count-1);
end;

function TCollectionOfHotspots.Last:PHotspot;
begin
	Last:=PHotspot(At(Count-1));
end;

var
	x_work_dc_bitmap:HBITMAP;

function get_draw_buffer(dc:HDC;X,Y:integer):HDC;
{	Copy the bitmap image from dc that will hold the image of a playing card
	with top left corner at (X,Y) into the working buffer used to build cards in.
	Returns the DC for the in-memory bitmap to draw into.
	Caller must call a matching release_draw_buffer. }
var
	WorkDC:HDC;
	SysDC:HDC;
begin
	SysDC:=GetDC(GetDesktopWindow);
	WorkDC:=CreateCompatibleDC(SysDC);
	x_work_dc_bitmap:=SelectObject(WorkDC,x_work_bitmap);
	ReleaseDC(GetDesktopWindow,SysDC);
	BitBlt(WorkDC,0,0,CardImageWd,CardImageHt,dc,X,Y,SRCCOPY);
	DrawCardShape(WorkDC,theCardOutlinePen,theCardBgBrush,CardImageWd,CardImageHt);
	get_draw_buffer:=WorkDC;
end;

procedure release_draw_buffer(a_dc:HDC);
begin
	SelectObject(a_dc,x_work_dc_bitmap);
	DeleteDC(a_dc);
end;

procedure DrawCardBack(dc:HDC;x,y:integer);
var
	tempDC:HDC;
begin
	tempDC:=get_draw_buffer(dc,x,y);
	PutBitmap(tempDC,the_instance.GetMaskBitmap(),0,0,SRCAND);
	PutBitmap(tempDC,the_instance.GetBackBitmap(),0,0,SRCPAINT);
	BitBlt(dc,X,Y,CardImageWd,CardImageHt,tempDC,0,0,SRCCOPY);
	release_draw_buffer(tempDC);
end;

procedure draw_card_face(DC:HDC; aCard:TCard; x:integer; y:integer);
begin
	if (aCard = NULL_CARD) then exit;
	if cardIsFacedown(aCard) then
		DrawCardBack(DC,x,y)
	else
		DisplayCard(DC, aCard, x, y);
end;

function CreateCardMask:drawing.bitmap;
begin
	CreateCardMask:=CreateMaskBitmapAt(ConvertWidthToIndex(CardGraphicsManager.Instance^.CurrentWidth));
end;

procedure GenPileOfCards.OnDragging;
begin //writeln('GenPileOfCards.OnDragging', '|', self.ToString);
end;

procedure GenPileOfCards.OnDropped;
begin //writeln('GenPileOfCards.OnDropped', '|', self.ToString);
end;

procedure GenPileOfCards.GrabCardAt(n:number);
var
	temp_dc:hDC;
	mDCBitmap:hBitmap;
	rcRender:TRect;
	DragDC:hDC;
begin //writeln('GenPileOfCards.GrabCardAt(',n,')');
	OnDragging;
	GrabbedCard:=CardAt(n);
	grabbed_from:=@Self;
	theGrabMask:=CreateCardMask;

	{ render the image of the card off-screen }
	DragDC:= GetDC(myTabletop^.handle);
	temp_dc:=CreateCompatibleDC(DragDC);
	theGrabImage:=CreateCompatibleBitmap(DragDC,CardWd,CardHt);
	mDCBitmap:=SelectObject(temp_dc,theGrabImage);
	draw_card_face(temp_dc,grabbedCard,0,0);
	SelectObject(temp_dc,mDCBitmap);
	DeleteDC(temp_dc);
	ReleaseDC(myTabletop^.handle, DragDC);

	ThePile^.ref(n)^:= NULL_CARD;

	rcRender.left:=anchor.x+GetCardX(n);
	rcRender.top:=anchor.y+GetCardY(n);
	rcRender.right:=rcRender.left+CardWd;
	rcRender.bottom:=rcRender.top+CardHt;
	myTabletop^.RefreshRect(rcRender);

	grabbedPt.x:= rcRender.left;
	grabbedPt.y:= rcRender.top;

	DragDC:=GetDC(myTabletop^.handle);
	temp_dc:= CreateCompatibleDC(DragDC);
	theGrabRestore:=CreateCompatibleBitmap(DragDC, GetRectWd(rcRender), GetRectHt(rcRender));
	mDCBitmap:=SelectObject(temp_dc,theGrabRestore);
	BitBlt(temp_dc, 0, 0, GetRectWd(rcRender), GetRectHt(rcRender), DragDC, rcRender.left, rcRender.top, SRCCOPY);
	SelectObject(temp_dc, mDCBitmap);
	DeleteDC(temp_dc);
	ReleaseDC(myTabletop^.handle,DragDC);

	SetCapture(myTabletop^.handle);
end;

procedure GenPileOfCards.GrabCardsAt(n:number);
var
	mDC,maskDC:HDC;
	ht:integer;
	mDCBitmap,maskDCBitmap:hBitmap;
	i:integer;
	dragDC:HDC;
	r:TRect;
begin
	OnDragging;
	GrabbedCard:=CardAt(n);
	grabbed_from:=@Self;

	ht:=Bottom-(anchor.y + getCardY(n));

	theGrabbedCards:=new(PPileOfCards,Construct(52));
	for i:=n to size do theGrabbedCards^.Add(get(i));

	DragDC:=GetDC(myTabletop^.handle);

	{ create a bitmap/mask of the image to be dragged }
	mDC:=CreateCompatibleDC(DragDC);
	maskDC:=CreateCompatibleDC(DragDC);
	theGrabMask:=CreateCompatibleBitmap(DragDC,CardWd,Ht);
	theGrabImage:=CreateCompatibleBitmap(DragDC,CardWd,Ht);
	mDCBitmap:=SelectObject(mDC,theGrabImage);
	maskDCBitmap:=SelectObject(maskDC,theGrabMask);
	SetRect(r,0,0,CardWd,Ht);
	FillRect(maskDC,r,GetStockObject(WHITE_BRUSH));
	for i:=1 to theGrabbedCards^.size do begin
		PutBitmap(maskDC,the_instance.GetMaskBitmap,0,getCardY(n+i-1)-getCardY(n),SRCAND);
		draw_card_face(mdc,theGrabbedCards^.get(i),0,getCardY(n+i-1)-getCardY(n));
	end;
	SelectObject(maskDC, maskDCBitmap);
	SelectObject(mDC, mDCBitmap);
	DeleteDC(maskDC);
	DeleteDC(mDC);
	
	{ discard the cards we just grabbed }
	while (size >= n) do ThePile^.discardtop;	

	// create the bitmap under the unit we just grabbed
	grabbedPt.x:= anchor.x + GetCardX(n);
	grabbedPt.y:= anchor.y + GetCardY(n);
	mDC:=CreateCompatibleDC(DragDC);
	theGrabRestore:=CreateCompatibleBitmap(DragDC, CardWd, ht);
	mDCBitmap:=SelectObject(mDC,theGrabRestore);
	OffsetRect(r,grabbedPt.x,grabbedPt.y);
	myTabletop^.Render(mDC,r);
	SelectObject(mDC,mDCBitmap);
	DeleteDC(mDC);

	ReleaseDC(myTabletop^.handle, DragDC);

	UnitDragging:=True; { let the world know we are dragging a unit }

	SetCapture(myTabletop^.handle);
end;

function GenPileOfCards.Size:integer;
begin
	if (ThePile=nil) then
		Size:=0
	else
		Size:=ThePile^.Size;
end;

function GenPileOfCards.GetHeight:word;
var
	i,t,b:integer;
begin
	t:=Anchor.Y;
	b:=t+CardHt;
	for i:=1 to Size do begin
		t:=Min(t, Anchor.Y+GetCardY(i));
		b:=Max(b, Anchor.Y+GetCardY(i)+CardHt);
	end;
	GetHeight:=b-t;
end;

function GenPileOfCards.GetWidth:word;
var
	i,l,r:integer;
begin
	l:=Anchor.X;
	r:=l+CardWd;
	for i:=1 to Size do begin
		l:=Min(left, Anchor.X+GetCardX(i));
		r:=Max(right, Anchor.X+GetCardX(i)+CardWd);
	end;
	GetWidth:=r-l;
end;

procedure GenPileOfCards.GetSpanRect(var rRect:TRect);
var
	i:integer;
begin //writeln('GenPileOfCards.GetSpanRect(var rRect:TRect)');
	SetRect(rRect, Anchor.X, Anchor.Y, Anchor.X+CardWd, Anchor.Y+CardHt);
	for i:=1 to Size do begin
		rRect.left:=Min(rRect.left,Anchor.X+GetCardX(i));
		rRect.top:=Min(rRect.top,Anchor.Y+GetCardY(i));
	end;
	rRect.right:=rRect.left+GetWidth;
	rRect.bottom:=rRect.top+GetHeight;
end;

function GenPileOfCards.CardOffsetX(n:number):integer;
begin
	CardOffsetX:=GetCardX(n);
end;

function GenPileOfCards.CardOffsetY(n:number):integer;
begin
	CardOffsetY:=GetCardY(n);
end;

function GenPileOfCards.CardOffsetRect(n:number):TRect;
begin
	CardOffsetRect:=CreateRect(Anchor.X+CardOffsetX(n),Anchor.Y+CardOffsetY(n),CurrentWidth,CurrentHeight);
end;

procedure GenPileOfCards.RefreshCard(n:number);
begin
 	myTabletop^.RefreshRect(CardOffsetRect(n));
end;

procedure GenPileOfCards.OnCardFlipped(n:number);
begin
end;

procedure GenPileOfCards.FlipCard(n:number; pause_after:pauseFactor);
begin
	ThePile^.FlipCard(n);
	RefreshCard(n);
	SndCardFlick;
	Delay(ConvertPauseToMillSeconds(pause_after));
	OnCardFlipped(n);
end;

const
	DEFAULT_DEAL_PAUSE_FACTOR=0;
	DEFAULT_FLIP_PAUSE_FACTOR=6;
	
procedure GenPileOfCards.FlipCard(n:number);
begin
	FlipCard(n,DEFAULT_FLIP_PAUSE_FACTOR);
end;

procedure GenPileOfCards.OnTopcardFlipped;
begin //writeln('GenPileOfCards.OnTopcardFlipped');
end;

procedure GenPileOfCards.FlipTopcard(pause_after:pauseFactor);
begin //writeln('GenPileOfCards.FlipTopcard(',pause_after,')');
	FlipCard(Size,pause_after);
	OnTopcardFlipped;
end;

procedure GenPileOfCards.FlipTopcard;
begin
	FlipTopcard(6);
end;

function GenPileOfCards.IsFacedown(nth:integer):boolean;
begin
	IsFaceDown:=CardIsFacedown(nth);
end;

function GenPileOfCards.CardIsFacedown(n:number):boolean;
begin
	CardIsFacedown:=cards.CardIsFacedown(CardAt(n));
end;

function GenPileOfCards.TopcardIsFacedown:boolean;
begin
	TopcardIsFacedown:=TopFacedown;
end;

function GenPileOfCards.IsEmpty:boolean;
begin
	IsEmpty:=ThePile^.IsEmpty;
end;

procedure GenPileOfCards.AddCardAt(aIndex:integer; aCard:TCard);
var
	aSpanRect:TRect;
begin
	GetSpanRect(aSpanRect);
	thePile^.ref(aIndex)^:=aCard;
	RefreshRect(aSpanRect);
end;

procedure GenPileOfCards.AddCard(aCard:TCard);
begin //writeln('GenPileOfCards.AddCard(',aCard,')');
	thePile^.add(aCard);
	Refresh;
	OnCardAdded;
end;

function GenPileOfCards.Get(nth:integer):TCard;
begin //writeln('GenPileOfCards.Get(', nth, ') Size=', Size);
	Get:=CardAt(nth);
end;

procedure GenPileOfCards.Shuffle;
begin
	ThePile^.Shuffle;
end;

procedure GenPileOfCards.MoveTo(NewX, NewY:integer);
begin
	Hide;
	MoveAbs(NewX, NewY);
	Show;
end;

procedure GenPileOfCards.DiscardTop;
var
	aSpanRect:TRect;
begin //writeln('!GenPileOfCards.DiscardTop() [size=', Size, ']');
	GetSpanRect(aSpanRect);
	ThePile^.DiscardTop;
	RefreshRect(aSpanRect);
	Delay(DealDelay);
end;

procedure GenPileOfCards.Discard;
var
	aSpanRect:TRect;
begin //writeln('!GenPileOfCards.Discard');
	GetSpanRect(aSpanRect);
	ThePile^.Empty;
	RefreshRect(aSpanRect);
	Delay(BASEDELAY);
end;

procedure OCardpileProp.DealTo(target:OCardpileProp_ptr);
begin
	DealTo(target,FALSE);
end;

procedure OCardpileProp.DealTo(target:OCardpileProp_ptr; flip_it:boolean);
var
	saved:integer;
begin
	saved:=AnimateSteps;
	AnimateSteps:=min(AnimateSteps,Max(1,FDSTEP div 3));
	TopcardTo(target, flip_it, DEFAULT_DEAL_PAUSE_FACTOR);
	AnimateSteps:=saved;
end;

function OCardpileProp.DealTo(point:xypair):PCardProp;
begin
	DealTo:=DealTo(point,FALSE);
end;

function OCardpileProp.DealTo(x,y:integer):PCardProp;
begin
	DealTo:=NIL;
end;

function OCardpileProp.DealTo(point:xypair;flip_it:boolean):PCardProp;
var
	pile:^OCardProp;
begin
	pile:=New(PCardProp,Construct(Topcard));
	pile^.ThePile^.Empty; // hack
	MyTabletop^.AddProp(pile, CardCenterToAnchor(point));
	DealTo(pile, flip_it);
	DealTo:=pile;
end;

procedure GenPileOfCards.TopcardTo(target:GenPileOfCardsP);
begin
	CardAtTo(Size,target);
end;

procedure GenPileOfCards.TopcardTo(target:GenPileOfCardsP; flip_it:boolean; pause_after:pauseFactor);
begin
	CardAtTo(Size,target, flip_it, pause_after);
end;

procedure GenPileOfCards.CardAtTo(n:number; target:GenPileOfCardsP);
begin //writeln('GenPileOfCards.CardAtTo(', n, ',target="', target^.m_tag, '")');
	CardAtTo(n,target,FALSE,4);
end;

procedure GenPileOfCards.CardAtTo(n:number; target:GenPileOfCardsP; flip_it:boolean; pause_after:pauseFactor);
begin //writeln('GenPileOfCards.CardAtTo(',n,',target="',target^.m_tag,'"flip=',flip_it,')|', self.ToString);
	target^.OnDragging;
	GrabCardAt(n);
	target^.GetGrabbed;
	if flip_it then begin
		Delay(25);
		target^.FlipTopcard(0);
	end;
	OnCardAdded;
	Delay(ConvertPauseToMillSeconds(pause_after));
end;

function GenPileOfCards.Removetop:TCard;
begin
	Removetop:=CardAt(Size);
	DiscardTop;
end;

procedure GenPileOfCards.SnapTopTo(target:GenPileOfCardsP);
begin
	target^.AddCard(Removetop);
end;

procedure GenPileOfCards.SnapAllto(target:GenPileOfCardsP);
{ move all the cards in this pile in one shot to the target pile }
begin
	hide;
	thePile^.SnapAllTo(target^.thePile);
	show;
	target^.refresh;
end;

procedure GenPileOfCards.PostConstruct;
begin
	hasTarget:=FALSE;
	targetState:=FALSE;
	m_tag:=NIL;
	desc:=NIL;
end;

constructor GenPileOfCards.Construct(pPile:PPileOfCards;p_flags:word;aCardDx,aCardDy:integer);
begin
	inherited Construct(CardWd,CardHt);
	PostConstruct;
	my_card_dx:=aCardDx;
	my_card_dy:=aCardDy;
	ThePile:=pPile;
	m_outlined:=((p_flags and POC_OUTLINED)<>0);
end;

procedure GenPileOfCards.LMousePressCard(dx,dy,aIndex:integer);
begin //writeln('GenPileOfCards.LMousePressCard(dx, dy, aIndex:integer)');
	if CanGrabUnit(aIndex) then begin
		GrabCardsAt(aIndex);
		Drag(dx, dy, aIndex);
	end
	else if CanGrabCardAt(aIndex) then begin
		GrabCardAt(aIndex);
		Drag(dx, dy, aIndex);
	end;
end;

function GenPileOfCards.OnPressed(dx,dy:integer):boolean;
var
	i:integer;
	pt:TPoint;

	function pthitscard:boolean;
	var
		r:TRect;
	begin
		r.left:= anchor.x - Left + GetCardX(i);
		r.top:= anchor.y - Top + GetCardY(i);
		r.right:= r.left + CardWd;
		r.bottom:= r.top + CardHt;
		pthitscard:= PtInRect(r, pt);
	end;

begin
	OnPressed:=FALSE;
	pt.x:=dx;
	pt.y:=dy;
	for i:=size downto 1 do if PtHitsCard then break;
	if (i > 0) and (i <= size) then begin
		LMousePressCard(dx-(anchor.x-Left+GetCardX(i)),dy-(anchor.y-Top+GetCardY(i)),i);
		OnPressed:=TRUE;
	end;
end;

function GenPileOfCards.CanGrabCardAt(aIndex:integer):boolean;
begin //writeln('GenPileOfCards.CanGrabCardAt(',aIndex,')');
	CanGrabCardAt:=FALSE;
end;

function GenPileOfCards.CanSelectCardAt(n:number):boolean;
begin
	CanSelectCardAt:=(Word(n)=Size);
end;

function GenPileOfCards.CanGrabUnit(aIndex:integer):boolean;
begin //writeln('GenPileOfCards.CanGrabUnit');
	CanGrabUnit:=FALSE;
end;

function GenPileOfCards.FindPlaceHolder:word;
var
	n:word;
begin
{	writeln('!GenPileOfCards.FindPlaceHolder()');}
	for n:= 1 to Size do
		if Get(n) = NULL_CARD then begin
			FindPlaceHolder:= n;
			Exit;
		end;
	FindPlaceHolder:= Size + 1;
end;

procedure GenPileOfCards.DiscardPlaceHolder;
var
	aSpanRect:Trect;
begin
{	writeln('!GenPileOfCards.DiscardPlaceHolder()');}
	with grabbed_from^ do if FindPlaceHolder <= Size then begin
		GetSpanRect(aSpanRect);
		ThePile^.discard(FindPlaceHolder);
		RefreshRect(aSpanrect);
	end;
end;

procedure GenPileOfCards.GetGrabbedAt(aIndex:integer);
var
	i:integer;
	aRect,uRect,sRect:TRect;
begin //writeln('GenPileOfCards.GetGrabbedAt(',aIndex,')');
//	if grabbed_from<>@self then grabbed_from^.OnDropped; // this line causes card counter ON/OFF synch count errors
	
	FloatBitmap(grabbedPt.x,grabbedPt.y,Anchor.X + GetCardX(aIndex),Anchor.Y + GetCardY(aIndex),theGrabImage,theGrabRestore,theGrabMask);
	DeleteObject(theGrabMask);
	theGrabMask:=0;
	DeleteObject(theGrabRestore);
	theGrabRestore:=0;
	aRect.left:=0;
	aRect.top:=0;
	aRect.right:=GetBitmapWd(theGrabImage);
	aRect.bottom:=GetBitmapHt(theGrabImage);
	DeleteObject(theGrabImage);
	theGrabImage:=0;

	if (theGrabbedCards<>NIL) then begin
		for i:=1 to theGrabbedCards^.size do PPileOfCards(thePile)^.Add(theGrabbedCards^.get(i));
		UnitDragging:=FALSE;
		OffsetRect(aRect,Anchor.X,Anchor.Y);
		GetSpanRect(sRect);
		UnionRect(uRect,aRect,sRect);
		myTabletop^.RefreshRect(uRect);
		OnCardAdded;//(theGrabbedCards^.Size);
		dispose(theGrabbedCards, Destruct);
		theGrabbedCards:=NIL;
	end
	else begin
		if aIndex > Size then begin
			grabbed_from^.DiscardPlaceHolder;
			AddCard(GrabbedCard);
		end 
		else begin
			AddCardAt(aIndex, GrabbedCard);
		end;
	end;

	ReleaseCapture;
//	self.OnDropped;
end;

procedure GenPileOfCards.getGrabbed;
begin //writeln('GenPileOfCards.GetGrabbed');
	GetGrabbedAt(FindPlaceHolder);
end;

procedure GenPileOfCards.dropOntop(dx,dy:integer);
begin //writeln('GenPileOfCards.dropOntop(',dx,',',dy,')|', self.ToString);
	grabbedPt.x:= dx;
	grabbedPt.y:= dy;
	getGrabbed;
end;

function GenPileOfCards.Accepts(aCard:TCard):boolean;
begin
	Accepts:=(size<thePile^.Limit);
end;

destructor GenPileOfCards.Destruct;
begin //writeln('GenPileOfCards.Destruct');
	if m_tag <> nil then strDispose(m_tag);
	inherited Destruct;
end;

function GenPileOfCards.Topcard:TCard;
begin
	Topcard:=CardAt(Size);
end;

function GenPileOfCards.topFaceup:boolean;
begin
	topFaceUp:=(not isEmpty) and thePile^.isFaceup(size);
end;

function GenPileOfCards.topFaceDown:boolean;
begin
	topFaceDown:= (not IsEmpty) and (not thePile^.isFaceup(size));
end;

procedure GenPileOfCards.Help;
begin
	{ place holder }
end;

function GenPileOfCards.CardIsFaceup(n:number):boolean;
begin //writeln('GenPileOfCards.CardIsFaceup(',n,')');
	CardIsFaceup:=cards.CardIsFaceup(CardAt(n));
end;

function GenPileOfCards.isFaceup(aIndex:word):boolean;
begin
	isFaceup:=CardIsFaceup(aIndex);
end;

procedure GenPileOfCards.Drag(dx,dy,aIndex:integer);
var
	pt:TPoint;
	dc:HDC;
begin //writeln('GenPileOfCards.Drag(',dx,',',dy,',',aIndex,')');
	isDragging:=True;
	pt.x:= anchor.x+GetCardX(aIndex);
	pt.y:= anchor.y+GetCardY(aIndex);
	{ keep the offset from top left corner of card to the cursor }
	DragOrg.x:= dx;
	DragOrg.y:= dy;
	dc:=GetDC(myTabletop^.handle);
	PutBitmap(dc,theGrabMask,pt.x,pt.y,SRCAND);
	PutBitmap(dc,theGrabImage,pt.x,pt.y,SRCPAINT);
	ReleaseDC(myTabletop^.handle,dc);
	DragStart(pt.x,pt.y,theGrabImage,theGrabRestore,theGrabMask);
end;

procedure GenPileOfCards.Flip;
begin
	thePile^.Flip;
	Refresh;
	Delay(DealDelay*3);
end;

procedure GenPileOfCards.RMousePress(dx,dy:integer);
begin
	if not isDragging then
		help
	else
		inherited RMousePress(dx,dy);
end;

function GenPileOfCards.IsDblClkTarget:boolean;
begin
	IsDblClkTarget:=FALSE;
end;

function GenPileOfCards.GetCardX(nth:integer):integer;
begin
	GetCardX:=(nth-1)*my_card_dx;
end;

function GenPileOfCards.GetCardY(nth:integer):integer;
begin
	GetCardY:=(nth-1)*my_card_dy;
end;

procedure MakeNewTTColor(color:TColorRef);
begin
	if TabletopBrush<>0 then DeleteObject(TabletopBrush);
	TabletopBrush:=CreateSolidBrush(color);
	x_table_top_color:=color;
end;

function DealDelay:integer;
begin
	DealDelay:=BASEDELAY*2;
end;

procedure SoundCardFlip;
begin
	SndCardFlick;
end;

function GenPileOfCards.Overlaps(Target:TRect; var Intersection:TRect):boolean;
var
	aSpanRect:TRect;
begin
	GetSpanRect(aSpanRect);
	Overlaps:= XpIntersectRect(Intersection, Target, aSpanRect);
end;

procedure GenPileOfCards.Redraw(dc:HDC;x,y:integer);
var
	i:integer;
	ThePen,PaintDCPen:hPen;
	PaintDCBrush:hBrush;
	theRect:TRect;
begin //writeln('GenPileOfCards.Redraw(hdc=', dc, ',xOfs=', x, ',yOfs=', y, ')');
	if IsEmpty then begin
		GetSpanRect(theRect);
		if m_outlined then begin
			PaintDCBrush:=SelectObject(dc,GetStockObject(NULL_BRUSH));
			PaintDCPen:=SelectObject(dc, GetStockObject(Black_Pen));
			RoundRect(dc,x+2,y+2,x+CardWd-2,y+CardHt-2,CornerRound,CornerRound);
			SelectObject(dc, PaintDCPen);
			SelectObject(dc, PaintDCBrush);
		end;
		if HasTarget then with theRect do begin
			left:=(CardImageWd div 6);
			right:=left+CardImageWd-(CardImageWd div 3);
			top:=Center(GetRectWd(theRect),0,CardHt);
			bottom:=top+GetRectWd(theRect);
			if TargetState then begin
				{ green circle }
				PaintDCBrush:= SelectObject(dc, GetStockObject(Null_Brush));
				ThePen:=CreatePen(ps_Solid, CardImageWd div 12, RGB(0,255,0));
				PaintDCPen:= SelectObject(dc, ThePen);
				Ellipse(dc, x + left, y + top, x + right, y + bottom);
				SelectObject(dc, PaintDCPen);
				DeleteObject(ThePen);
				SelectObject(dc, PaintDCBrush);
			end
		end;
	end
	else for i:=1 to Size do DrawCard(dc,i,x,y);
end;

function GenPileOfCards.GetAnchorX:integer;
begin
	GetAnchorX:=Anchor.X;
end;

function GenPileOfCards.GetAnchorY:integer;
begin
	GetAnchorY:=Anchor.Y;
end;

procedure GenPileOfCards.MoveAbs(NewX, NewY:integer);
begin
	Anchor.X:= NewX;
	Anchor.Y:= NewY;
end;

function GenPileOfCards.AddFacedown:boolean;
begin
	AddFacedown:=thePile^.IsNextFacedown;
end;

function GenPileOfCards.IsCardExposed(aIndex:integer):boolean;
begin //writeln('!GenPileOfCards.IsCardExposed(', aIndex, ')');
	IsCardExposed:=not (CardIsPlaceHolder(aIndex) or CardIsCovered(aIndex));
end;

function GenPileOfCards.CardAt(n:number):TCard;
begin
	CardAt:=ThePile^.Get(Integer(n));
end;

procedure GenPileOfCards.DrawCard(DC:HDC; nth:integer; const x:integer; const y:integer);
begin //writeln('!GenPileOfCards.DrawCard(DC:HDC;', nth, ', x, y)');
	if IsCardExposed(nth) then draw_card_face(dc,CardAt(nth),x+GetCardX(nth),y+GetCardY(nth));
end;

function CardPlayDelay:integer;
begin
	CardPlayDelay:=DealDelay;
end;

function OTabletop.OnEraseBkGnd(dc:HDC):LONG;
begin //writeln('OTabletop.OnEraseBkGnd(dc:HDC)');
	OnEraseBkGnd:=1; // necessary to avoid white flash
	if (tabletop_image=NULL_BITMAP) then tabletop_image:=CreateTabletopBitmap(Width, Height);
end;

constructor ODeckprop.Construct(packs:number);
begin
	inherited Construct(packs*52);
	self.ThePile:=new(PDeck, Construct(Capacity));
	self.ThePile^.addPacks(FALSE);
end;

const
	PP_CONTROLID=172;
	PP_FONT_LEADING=4;
	
constructor PlayerPrompt.Construct(parent:PTabletop);
begin
	handle:=ApiCheck(CreateWindow('STATIC','',WS_CHILD or WS_BORDER or SS_CENTER or SS_OWNERDRAW,0,0,0,PP_FONT_LEADING+DISPLAY_FONT_HEIGHT+3,parent^.handle,PP_CONTROLID,hInstance,nil));
	self.my_parent:=parent;
end;

procedure PlayerPrompt.SelectFont(dc:HDC);
begin
	previous_font:=SelectObject(dc, display_font);
end;

procedure PlayerPrompt.RestoreFont(dc:HDC);
begin
	SelectObject(dc, previous_font);
end;

procedure PlayerPrompt.Draw(dc:HDC);
var
	myText:stringbuffer;
	r:RECT;
	fill_brush:HBRUSH;
begin
	SetRect(r,0,0,XWND(handle).ClientWidth,XWND(handle).ClientHeight);
	fill_brush:=CreateSolidBrush(RGB(234,234,174));
	FillRect(dc,r,fill_brush);
	DeleteObject(fill_brush);
	SelectFont(dc);
	SetBkMode(dc,TRANSPARENT);
	SetTextAlign(dc,TA_CENTER or TA_TOP);
	GetWindowText(handle,myText,sizeof(stringbuffer));
	TextOut(dc,XWND(handle).ClientWidth div 2, PP_FONT_LEADING-1,myText,StrLen(myText));
	RestoreFont(dc);
end;

procedure PlayerPrompt.Hide;
begin
	windows.ShowWindow(handle,SW_HIDE);
	windows.UpdateWindow(windows.GetParent(handle));
end;

procedure PlayerPrompt.SetText(aTextString:PChar);
var
	dc:hDC;
	ps:stringBuffer;
	w:longint;
begin
	StrCopy(ps,aTextString);
	StrCat(ps,'    ');
	dc:=GetDC(GetParent);
	SelectFont(dc);
	w:=GetHdcTextWidth(dc,ps);
	RestoreFont(dc);
	ReleaseDC(GetParent,dc);
	MoveWindow(handle,GetWndLeft(handle),GetWndTop(handle),w,Height,FALSE);
	SetWindowText(aTextString);
end;

function PlayerPrompt.IsVisible:boolean;
begin
	IsVisible:=IsWindowVisible(handle);
end;

function OCardpileProp.Capacity:number;
begin
	Capacity:=self.ThePile^.Limit;
end;

procedure PlayerPrompt.Reposition;
begin
	MoveWindow(self.handle,
		Center(Width,0,MyParent^.Width), 
		(MyParent^.Height * 2 div 3) - (Height div 2),
		Width, Height, FALSE);
end;

function PlayerPrompt.MyParent:PWindow;
begin
	MyParent:=my_parent;
end;

procedure PlayerPrompt.Show;
begin
	Reposition;
	ShowWindow(SW_SHOWNA);
	UpdateWindow;
end;

function OTabletop.OnDrawItem(control_id:UINT;draw_info:LPDRAWITEMSTRUCT):LONG;
begin
	OnDrawItem:=0;
	if (control_id=PP_CONTROLID) then begin
		if (player_prompt.handle<>NULL_HANDLE) then player_prompt.Draw(draw_info^.hDC);
		OnDrawItem:=1;
	end;
end;

function OTabletop.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;
begin
	case aMsg of
		WM_DRAWITEM:OnMsg:=OnDrawItem(wParam,LPDRAWITEMSTRUCT(lParam));
		WM_LBUTTONDBLCLK:OnMsg:=OnLButtonDblClick(wParam,LOWORD(lParam),HIWORD(lParam));
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

var
	hwCardFlick:THandle;
	hwpCardFlick:Pointer;

procedure SndCardFlick;
begin
	if x_SoundStatus then begin
		PlaySound(NIL,0,0); // stops any currently playing waveform
		sndPlaySound(hwpCardFlick,SND_NOSTOP or SND_NODEFAULT OR SND_MEMORY or SND_ASYNC);
	end;
end;

procedure Terminate;
begin
	FreeResource(hwCardFlick);
end;

procedure DisplayCard(dc:HDC;C:TCard; x,y:integer);
var
	tempDC:HDC;
begin
	tempDC:=get_draw_buffer(dc,x,y);
	PutBitmap(tempDC, the_instance.GetMaskBitmap(), 0, 0, SRCAND);
	PutBitmap(tempDC, the_instance.GetFaceBitmap(TCardToCard(C)), 0, 0, SRCPAINT);
	BitBlt(dc, X, Y, CardImageWd, CardImageHt, tempDC, 0, 0, SRCCOPY);
	release_draw_buffer(tempDC);
end;

procedure DrawDCNoSymbol(dc:hDC;aRect:TRect);
{ Draw the familiar red circle with a slash thru it for the NO symbol. Draw it in "aRect" of "dc". "aRect" should be square to get a round circle. }
var
	i:integer;
	ThePen,PaintDCPen:hPen;
	PaintDCBrush:hBrush;
begin
	PaintDCBrush:=SelectObject(dc,GetStockObject(Null_Brush));
	ThePen:=CreatePen(ps_Solid,GetRectWd(aRect) div 7,RGB(255,0,0));
	{ circle with NW to SE line thru it }
	PaintDCPen:=SelectObject(dc,ThePen);
	with aRect do begin
		Ellipse(dc,left,top,right,bottom);
		{ with NW to SE line thru it }
		i:=(GetRectWd(aRect) div 4);
		MoveToEx(dc,left+i,top+i,nil);
		LineTo(dc,right-i+1,bottom-i+1);
	end;
	SelectObject(dc,PaintDCPen);
	DeleteObject(ThePen);
	SelectObject(dc,PaintDCBrush);
end;

var
	back_bitmap_cache:array[supportedSize] of HBITMAP;
	mask_bitmap_cache:array[supportedSize] of HBITMAP;
	face_bitmap_cache:array[supportedSize] of array[Low(cards.pip)..High(cards.pip)] of array[Low(cards.suit)..High(cards.suit)] of HBITMAP;

function TheCardGraphicsManager.GetBackBitmap:drawing.bitmap;
var
	at:number;
begin
	at:=ConvertWidthToIndex(CurrentWidth);
	if (back_bitmap_cache[at]=NULL_HANDLE) then back_bitmap_cache[at]:=CreateBackBitmapAt(at);
	GetBackBitmap:=back_bitmap_cache[at];
end;

function TheCardGraphicsManager.GetMaskBitmap:drawing.bitmap;
var
	at:number;
begin
	at:=ConvertWidthToIndex(CurrentWidth);
	if (mask_bitmap_cache[at]=NULL_HANDLE) then mask_bitmap_cache[at]:=CreateMaskBitmapAt(at);
	GetMaskBitmap:=mask_bitmap_cache[at];
end;

function TheCardGraphicsManager.GetFaceBitmap(card:cards.card):drawing.bitmap;
var
	at:number;
	p:cards.pip;
	s:cards.suit;
begin
	at:=ConvertWidthToIndex(CurrentWidth);
	p:=GetCardPip(card);
	s:=GetCardSuit(card);
	if (face_bitmap_cache[at,p,s]=NULL_HANDLE) then face_bitmap_cache[at,p,s]:=CreateFaceBitmapAt(at,card);
	GetFaceBitmap:=face_bitmap_cache[at,p,s];
end;

constructor TPileHelpDlg.Construct(aPile:GenPileOfCardsP);
begin
	inherited Construct(hwndAnimate,509);
	Pile:=aPile;
end;

function TPileHelpDlg.OnInitDialog:boolean;
var
	ps:array[0..80] of Char;
	n:integer;
	s:string;
begin
	OnInitDialog:=inherited OnInitDialog;
	with Pile^ do begin
		StrCopy(ps,'');
		if (m_tag<>NIL) then StrCopy(ps, m_tag);
		StrCat(ps,' Pile');
		SetWindowText(ps);
		XWND(windows.GetDlgItem(Handle,102)).SetWindowText(Desc);
		if Size = 0 then begin
			StrCopy(ps,'Currently empty!')
		end
		else begin
			n:=Size;
			s:='Currently has '+NumberToString(n)+' Card';
			StrPcopy(ps,s);
			if Size>1 then StrCat(ps,'s');
			StrCat(ps,'.');
		end;
		windows.SetWindowText(windows.GetDlgItem(Handle,101),ps);
	end;
end;

constructor BaseCardGraphicsManager.Construct; begin end;
function __CardGraphicsManager.Instance:ICardGraphicsManager;
begin
	Instance:=card_graphics_manager;
end;

constructor OCardProp.Construct(card:TCard);
begin
	inherited Construct(1);
	ThePile^.Add(card);
	Enable;
end;

function OCardProp.GetCard:TCard;
begin
	GetCard:=Topcard;
end;

function OCardProp.OnTopcardTapped:boolean;
begin
	FlipTopcard;
	OnTopcardTapped:=TRUE;
end;

function OCardProp.CanGrabCardAt(aIndex:integer):boolean;
begin //writeln('OCardProp.CanGrabCardAt(',aIndex,')');
	CanGrabCardAt:=CardIsFaceup(aIndex);
end;

function Game.TotalWidth:word;
begin
	TotalWidth:=SpanOf(PileColumns,CurrentWidth,PileSpacing);
end;

function Game.TotalHeight:word;
begin
	TotalHeight:=SpanOf(PileRows,CurrentHeight,PileSpacing);
end;

function OPropwart.ToString:string;
begin
	ToString:='ON/OFF:'+NumberToString(onoff)+' '+inherited ToString;
end;

procedure OPropwart.PostConstruct(host:PHotspot; initially_on:boolean);
begin
	System.Assert(host<>NIL, 'host cannot be NIL');
	self.hostprop:=host;
	onoff:=Q(initially_on, 1, 0);
	Disable;
end;

constructor OPropwart.Construct(host:PHotspot; initially_on:boolean);
begin
	inherited Construct(WART_WIDTH, WART_HEIGHT, 0);
	PostConstruct(host, initially_on);
end;

constructor OPropwart.Construct(host:PHotspot);
begin
	inherited Construct(WART_WIDTH, WART_HEIGHT, 0);
	PostConstruct(host, TRUE);
end;

function OPropwart.IsOn:boolean;
begin
	IsOn:=(onoff>0);
end;

procedure OPropwart.On;
begin //writeln('OPropwart.On', '|', self.ToString);
	Inc(onoff);
	if onoff>0 then Show;
end;

procedure OPropwart.Off;
begin //writeln('OPropwart.Off', '|', self.ToString);
	Dec(onoff);
	if onoff=0 then Hide;
end;

function OTextprop.GetContent:string;
begin
	GetContent:='';
end;

function OPropwart.Parent:PHotspot;
begin
	Parent:=self.hostprop;
end;

function OPropwart.GetAnchorPoint(table_width,table_height:word):xypair;
	function GetX:integer;
	begin
		case self.relative_position of
			TOP_LEFT,CENTER_LEFT,BOTTOM_LEFT:GetX:=self.hostprop^.Left-GetWidth+WART_OVERLAP;
			TOP_RIGHT,CENTER_RIGHT,BOTTOM_RIGHT:GetX:=self.hostprop^.Right-WART_OVERLAP;
			else GetX:=Center(GetWidth, self.hostprop^.Left, self.hostprop^.Right);
		end;
	end;
	function GetY:integer;
	begin
		case self.relative_position of
			TOP_LEFT,TOP_CENTER,TOP_RIGHT:GetY:=self.hostprop^.Top+WART_OVERLAP;
			BOTTOM_LEFT,BOTTOM_CENTER,BOTTOM_RIGHT:GetY:=self.hostprop^.Bottom-WART_OVERLAP;
			else GetY:=Center(GetHeight, self.hostprop^.Top, self.hostprop^.Bottom);
		end;
	end;
begin //writeln('OPropwart.GetAnchorPoint(', table_width, ',', table_height, ')');//DumpStack;
	GetAnchorPoint:=MakeXYPair(GetX, GetY);
end;

procedure OTextprop.Redraw(dc:HDC; x,y:integer);
const
	AA_FACTOR=8;
var
	pen,aa_pen:HPEN;
	brush:HBRUSH;
	font:HFONT;
	aa_DC:HDC;
	aa_bmp:HBITMAP;
begin //writeln('OPropwart.Redraw(',dc, ',',x, ',', y, ')');
	if Length(GetContent)>0 then begin

		// use a GDI trick to render an image with Anti Aliasing leveraging the fact that StretchBlt will anti alias the result when you shrink an image
		
		// first create a bigger bitmap
		aa_DC:=CreateCompatibleDC(dc);
		aa_bmp:=CreateCompatibleBitmap(dc, Width*AA_FACTOR, Height*AA_FACTOR);
		SelectObject(aa_DC, aa_bmp);

		// stretch the current background into the larger bitmap
		SetStretchBltMode(aa_DC, COLORONCOLOR);
		StretchBlt(aa_DC, 0, 0, Width*AA_FACTOR, Height*AA_FACTOR, dc, x, y, Width, Height, SRCCOPY);
		
		// now render your image in the larger bitmap
		brush:=SelectObject(aa_dc, GetStockObject(LTGRAY_BRUSH));
		aa_pen:=CreatePen(PS_SOLID, AA_FACTOR, 0);
		pen:=SelectObject(aa_dc, aa_pen);
		RoundRect(aa_dc, (AA_FACTOR div 2), (AA_FACTOR div 2), Width*AA_FACTOR-(AA_FACTOR div 2), Height*AA_FACTOR-(AA_FACTOR div 2), Height*AA_FACTOR, Height*AA_FACTOR);
		SelectObject(aa_dc, pen);
		DeleteObject(aa_pen);
		SelectObject(aa_dc, brush);

		// and finally stretch it back to the target DC to affect the anti-aliasing
		SetStretchBltMode(dc, HALFTONE);
		StretchBlt(dc, x, y, Width, Height, aa_DC, 0, 0, Width*AA_FACTOR, Height*AA_FACTOR, SRCCOPY);
		
		// clean up the trash
		DeleteObject(aa_bmp);
		DeleteDC(aa_DC);
		
		// use the built-in anti-aliasing for text rendering
		font:=SelectObject(dc, display_font);
		SetTextColor(dc, RGB_BLACK);
		SetBkMode(dc, TRANSPARENT);
		SetTextAlign(dc, TA_CENTER or TA_BASELINE);
		TextOut(dc, x+(self.Width div 2)+Q(Length(self.GetContent)=1,1,0), y+self.Height-5, PChar(AnsiString(self.GetContent)), Length(self.GetContent));
		SelectObject(dc, font);
	end;
end;

procedure Hotspot.AddWart(wart:PPropwart);
begin //writeln('Hotspot.AddWart(',LongInt(wart),')');
	AddWart(wart, CENTER_CENTER);
end;

procedure Hotspot.AddWart(wart:PPropwart; where:relativeposition);
begin
	System.Assert(self.propwarts[where]=NIL, 'A tabletop prop can have only one (1) wart at each pre-defined relative_position.');
	wart^.relative_position:=where;
	self.propwarts[where]:=wart;
	if (self.myTabletop<>NIL) then self.myTabletop^.AddProp(self.propwarts[where]);
end;

procedure Hotspot.OnHiding;
var
	loc:relativeposition;
begin //writeln('Hotspot.OnHiding');
	for loc:=Low(relativeposition) to High(relativeposition) do if (self.propwarts[loc]<>NIL) then self.propwarts[loc]^.Hide;
end;

procedure Hotspot.OnShown;
var
	loc:relativeposition;
begin //writeln('Hotspot.OnShown');
	for loc:=Low(relativeposition) to High(relativeposition) do if (self.propwarts[loc]<>NIL) then with self.propwarts[loc]^ do if IsOn then Show;
end;

function Hotspot.GetWartAt(where:relativeposition):PPropwart;
begin
	GetWartAt:=self.propwarts[where];
end;

function OTextprop.GetWidth:word;
const
	w:word=0;
var
	font:HFONT;
	dc:HDC;
begin
	if (MyTabletop<>NIL) then begin
		dc:=GetDC(MyTabletop^.handle);
		font:=SelectObject(dc, display_font);
		w:=GetHdcTextWidth(dc, PChar(AnsiString(GetContent))) + GetHdcTextWidth(dc, PChar(AnsiString('--')));
		SelectObject(dc, font);
		ReleaseDC(MyTabletop^.handle, dc);
	end;
	GetWidth:=Max(WART_HEIGHT, ForceOddUp(w));
end;

function OCardCountWart.GetContent:string;
begin
	GetContent:=Q(GenPileOfCardsP(Parent)^.Size=0, '', NumberToString(GenPileOfCardsP(Parent)^.Size));
end;

constructor OSquaredpileprop.Construct;
begin
	Construct(52);
end;

constructor OSquaredpileprop.Construct(n:number);
begin
	inherited Construct(n);
//	AddWart(New(PCardCountWart, Construct(@self)), CENTER_CENTER);
end;

procedure OSquaredpileprop.OnDragging;
begin //writeln('OSquaredpileprop.OnDragging', '|', self.ToString);
//	CardCountOff;
end;

procedure OSquaredpileprop.OnDropped;
begin //writeln('OSquaredpileprop.OnDropped', '|', self.ToString);
//	CardCountOn;
end;

function OCardpileProp.CanGrabCardAt(a_index:integer):boolean;
begin //writeln('OCardpileProp.CanGrabCardAt(',a_index,')');
	CanGrabCardAt:= (a_index=size) and CardIsFaceup(a_index);
end;

function OFannedPileProp.canGrabUnit(a_index:integer):boolean;
begin
	canGrabUnit:= (a_index<size) and IsUnit[a_index];
end;

procedure OFannedPileProp.topSetUnit(State:boolean);
begin
	IsUnit[size]:= State;
end;

function OCardpileProp.AceUp:boolean;
begin
	AceUp:= (topFaceUp and (CardPip(Topcard) = TACE));
end;

function OCardpileProp.topX:integer;
begin
	topX:= anchor.x + getCardX(size);
end;

function OCardpileProp.topY:integer;
begin
	topY:= anchor.y + getCardY(size);
end;

procedure OFannedPileProp.topRemoved;
begin
end;

procedure OFannedPileProp.UnitRemoved;
begin
end;

procedure OCardpileProp.Help;
var
	aDialog:TPileHelpDlg;
begin
	aDialog.Construct(@Self);
	aDialog.Modal;
end;

procedure OFannedPileProp.dropOntop(x, y:integer);
begin
	inherited dropOntop(x, y);
	OFannedPileProp_ptr(grabbed_from)^.topRemoved;
end;

function OFannedPileProp.Covered:boolean;
var
	i:integer;
	r:TRect;
	Pile:OFannedPileProp_ptr;
	aThisSpan, aTargetSpan:TRect;
begin //writeln('OFannedPileProp.Covered');
	Covered:=False;
	GetSpanRect(aThisSpan);
	for i:=MyTabletop^.HotList.Count - 1 downto MyTabletop^.HotList.IndexOf(@Self) do if PHotSpot(MyTabletop^.HotList.At(i))^.ObjectClass=HCLASS.GENPILEOFCARDS then begin
		Pile:=OFannedPileProp_ptr(MyTabletop^.HotList.At(i));
		Pile^.GetSpanRect(aTargetSpan);
		if
			(Pile<>@Self)
			and
			(not Pile^.IsEmpty)
			and
			XpIntersectRect(r, aThisSpan, aTargetSpan)
		then begin
			Covered:=True;
			Break;
		end;
	end;
end;

function OFannedPileProp.OnPressed(x_offset,y_offset:integer):boolean;
begin //writeln('OFannedPileProp.OnPressed(', x_offset, ',', y_offset,')');
	if not Covered then inherited OnPressed(x_offset,y_offset);
	OnPressed:=FALSE;
end;

function Game.ColumnSpan(nPiles:word):integer;
begin
	ColumnSpan:=SpanOf(nPiles,CurrentWidth,PileSpacing);
end;

function Game.ColumnOffset(columnIndex:word):integer;
begin
	ColumnOffset:=SpanOffset(columnIndex,CurrentWidth,PileSpacing);
end;

function Game.RowSpan(nPiles:word):integer;
begin
	RowSpan:=SpanOf(nPiles,CurrentHeight,PileSpacing);
end;

function Game.RowOffset(rowIndex:word):integer;
begin
	RowOffset:=SpanOffset(rowIndex,CurrentHeight,PileSpacing);
end;

constructor OFannedPileProp.Construct;
begin
	inherited Construct(52);
	FillChar(IsUnit, sizeof(IsUnit), #0);
	Enable;
end;

constructor OCardCountWart.Construct(host:PHotspot);
begin
	inherited Construct(host, FALSE);
end;

procedure OSquaredpileprop.CardCountOn;
begin //writeln('OSquaredpileprop.CardCountOn', '|', self.ToString);
//	GetWartAt(CENTER_CENTER)^.On;
end;

procedure OSquaredpileprop.CardCountOff;
begin //writeln('OSquaredpileprop.CardCountOff', '|', self.ToString);
//	GetWartAt(CENTER_CENTER)^.Off;
end;

procedure DefaultWinnerSound;
var
	hWave:THandle;
	pWave:Pointer;
begin
	{ play a winning sound }
	if x_SoundStatus then begin
		hWave :=FindResource(hInstance,'Winner_1','WAVE');
		if hWave<>0 then begin
			hWave :=LoadResource(hInstance,hWave);
			pWave :=LockResource(hWave);
			SndPlaySound(pWave,Snd_NoDefault or Snd_Memory);
			FreeResource(hWave);
		end;
	end;
end;

begin //writeln('winqcktbl');
	display_font:=CreateFont(DISPLAY_FONT_HEIGHT, 0, 0, 0, FW_BOLD, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY, VARIABLE_PITCH or FF_SWISS, 'Arial');
	FillChar(back_bitmap_cache, SizeOf(back_bitmap_cache), 0);
	FillChar(mask_bitmap_cache, SizeOf(mask_bitmap_cache), 0);
	FillChar(face_bitmap_cache, SizeOf(face_bitmap_cache), 0);
	hwCardFlick:=FindResource(hInstance,'CARD_FLICK','WAVE');
	hwCardFlick:=LoadResource(hInstance,hwCardFlick);
	hwpCardFlick:=LockResource(hwCardFlick);
	the_instance.Construct(winCardFactory.Instance);
	card_graphics_manager:=@the_instance;
	SelectWidthAt(SupportedSizeCount);
	{$ifdef QUICK_PLAY} 
	AnimateSteps:=1; 
	BASEDELAY:=0; 
	{$endif}
end.
