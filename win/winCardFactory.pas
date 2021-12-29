{ (C) 2009 Wesley Steiner }

unit winCardFactory;

{$I platform}
{$R winCardFactory.res}

interface

uses
	windows,
	std,
	cards,
	drawing,
	cardFactory;
	
type
	supportedSize=1..80;
	
function Instance:ICardFactory;
function ConvertWidthToIndex(desired_width:word):number;
function CreateBackBitmapAt(at:number):drawing.bitmap;
function CreateFaceBitmapAt(at:number;card:cards.card):drawing.bitmap;
function CreateMaskBitmapAt(at:number):drawing.bitmap;
function SupportedSizeCount:quantity;
function SupportedHeightAt(at:number):word;
function SupportedWidthAt(at:number):word;

// bcards legacy
type
	TCardFormat=(RegularFormat,JumboFormat);
	TCardSize=(SmallCards,MediumCards,LargeCards);
	PCPipType=TPip;
	PCSuitType=TSuit;

{$ifdef TEST}
function CalcHtFromWd(aFormat:TCardFormat; aCardImageWd:word):word;
function CardWidthAt(aIndex:word):word;
function ConvertIndexToSize(aCardWidthIndex:number):TCardSize;
{$endif}

var
	CornerRound:integer; { factor to use to get round corners }
	Format:TCardFormat;
	x_work_bitmap:HBITMAP;
	theCardOutlinePen:HPEN;
	theCardBgBrush:HBRUSH;
	pipOfsX,pipOfsY:integer;
	PipHSpace,PipVSpace:integer;

const
	CardImageWd:word=MAX_WORD;
	CardImageHt:word=MAX_WORD;

function OptXSpace:integer;
function OptYSpace:integer;
function SuitIcon(S:pcSuitType):HBITMAP;
procedure DrawCardShape(aCardImageDC:HDC;outline_pen:HPEN;fill_brush:HBRUSH;w,h:word);
procedure FreeResources;
procedure SelectDesiredCardWidth(desired_width:word);
procedure SelectWidthAt(at:number);

implementation

uses 
	windowsx,
	sdkex,
	gdiex;

const
	// real world playing card dimensions in deca-mm units
	DMM_METRICS:array[TCardFormat] of record
		WIDTH, HEIGHT, INSIDE_W, INSIDE_H, PIP_H, SUIT_W, SUIT_H:integer; 
	end=(
		(WIDTH:629; HEIGHT:883; INSIDE_W:439; INSIDE_H:713; PIP_H:95; SUIT_W:132; SUIT_H:158),
		(WIDTH:629; HEIGHT:883; INSIDE_W:299; INSIDE_H:529; PIP_H:195; SUIT_W:88; SUIT_H:70)
	);

	IPBU=1; { upright black pip icons A..K }
	IPBR=IPBU+13; { reverse black pip icons A..K }
	IPRU=IPBR+13; { upright red pip icons A..K }
	IPRR=IPRU+13; { reverse red pip icons A..K }
	ISLU=IPRR+13; { large upright c,d,h,s }
	ISLR=ISLU+4; 	{ large reverse c,d,h,s }
	ISSU=ISLR+4; 	{ small upright suit icons }
	ISSR=ISSU+4; 	{ small reverse suit icons }
	IFCD=ISSR+4; 	{ first face card icon Jc..Ks }
	IACE=IFCD+12; { ACE of SPADES decoration }
	IJKU=IACE+1; 	{ upright JOKER pip }
	IJKR=IJKU+1; 	{ reverse JOKER pip }
	IJOK=IJKR+1; 	{ "JOKER" image }
	ICJU=IJOK+1; { Cdn JOKER pip }
	ICJR=ICJU+1; { US JOKER pip }
	N_ICONS	= ICJR;

	ITPU:array[TACE..TKING] of integer=(8301,8302,8303,8304,8305,8306,8307,8308,8309,8310,8311,8312,8313);
	ITSU:array[TCLUB..TSPADE] of integer=(8355,8356,8357,8358);

type
	TheCardFactory=object(ICardFactory_)
		constructor Construct;
		function CreateBackBitmapAt(at:number):drawing.bitmap; virtual;
		function CreateFaceBitmapAt(at:number;card:cards.card):drawing.bitmap; virtual;
		function CreateMaskBitmapAt(at:number):drawing.bitmap; virtual;
		function SupportedSizeCount:quantity; virtual; 
		function SupportedWidthAt(at:number):word; virtual;
		function SupportedHeightAt(at:number):word; virtual;
	end;

var
	the_card_factory:TheCardFactory;
	DeckIcons:array[1..N_ICONS] of HBITMAP;
	iRect:TRect; { inside rectangle centered on the card where suit icons and pictures are placed }

function SupportedSizeCount:quantity;
begin
	SupportedSizeCount:=the_card_factory.SupportedSizeCount;
end;

function SupportedHeightAt(at:number):word;
begin
	SupportedHeightAt:=the_card_factory.SupportedHeightAt(at);
end;

function SupportedWidthAt(at:number):word;
begin
	SupportedWidthAt:=the_card_factory.SupportedWidthAt(at);
end;

function CalcHtFromWd(aFormat:TCardFormat; aCardImageWd:word):word;
begin
	CalcHtFromWd:=ForceOddUp(LongMul(aCardImageWd, DMM_METRICS[Format].HEIGHT) div DMM_METRICS[Format].WIDTH);
end;

function Instance:ICardFactory;
begin
	Instance:=@the_card_factory;
end;

function CardWidthAt(aIndex:word):word;
begin
	CardWidthAt:=Instance^.SupportedWidthAt(aIndex);
end;

function ConvertWidthToIndex(desired_width:word):number;
var
	i:number;
begin
	ConvertWidthToIndex:=Low(number);
	for i:=Instance^.SupportedSizeCount downto 1 do if desired_width>=CardWidthAt(i) then begin
		ConvertWidthToIndex:=i;
		Break;
	end;
end;

constructor TheCardFactory.Construct; 
begin 
end;

function TheCardFactory.SupportedSizeCount:quantity; 
begin 
	SupportedSizeCount:=High(supportedSize);
end;

function TheCardFactory.SupportedHeightAt(at:number):word;
begin
	SupportedHeightAt:=CalcHtFromWd(RegularFormat,SupportedWidthAt(at));
end;

function TheCardFactory.SupportedWidthAt(at:number):word;
begin
	SupportedWidthAt:=53+at*2;
end;

procedure DrawCardShape(aCardImageDC:HDC;outline_pen:HPEN;fill_brush:HBRUSH;w,h:word);
var
	dcPen:HPEN;
	dcBrush:HBRUSH;
begin
	dcPen:=SelectObject(aCardImageDC,outline_pen);
	dcBrush:=SelectObject(aCardImageDC,fill_brush);
	RoundRect(aCardImageDC,0,0,w,h,CornerRound,CornerRound);
	SelectObject(aCardImageDC,dcBrush);
	SelectObject(aCardImageDC,dcPen);
end;

procedure DrawCardMask(aCardImageDC:HDC);
var
	r:TRect;
begin
	SetRect(r,0,0,CardImageWd,CardImageHt);
	FillRect(aCardImageDC,r,GetStockObject(WHITE_BRUSH));
	DrawCardShape(aCardImageDC,GetStockObject(BLACK_PEN),GetStockObject(BLACK_BRUSH),CardImageWd,CardImageHt);
end;

function TheCardFactory.CreateMaskBitmapAt(at:number):drawing.bitmap;
var
	sys_dc,mem_dc:HDC;
	mask,memDCBitmap:HBITMAP;
begin
	at:=Min(Integer(at), Integer(SupportedSizeCount));
	sys_dc:=GetDC(0);
	mask:=CreateCompatibleBitmap(sys_dc,SupportedWidthAt(at),SupportedHeightAt(at));
	mem_dc:=CreateCompatibleDC(sys_dc);
	ReleaseDC(0,sys_dc);
	memDCBitmap:=SelectObject(mem_dc,mask);
	DrawCardMask(mem_dc);
	SelectObject(mem_dc,memDCBitmap);
	DeleteDC(mem_dc);
	CreateMaskBitmapAt:=mask;
end;

function CreateMaskBitmapAt(at:number):drawing.bitmap;
begin
	CreateMaskBitmapAt:=Instance^.CreateMaskBitmapAt(at);
end;

function CreateBackBitmapAt(at:number):drawing.bitmap;
begin
	CreateBackBitmapAt:=Instance^.CreateBackBitmapAt(at);
end;

function CreateFaceBitmapAt(at:number;card:cards.card):drawing.bitmap;
begin
	CreateFaceBitmapAt:=Instance^.CreateFaceBitmapAt(at,card);
end;

function SuitIcon(S:pcSuitType):HBITMAP;
{ Return a pointer to a suit icon. }
begin
	SuitIcon:=DeckIcons[ISLU+Ord(S)];
end;

function CX(H:integer):integer;
{ Return the X offset needed in order to center a bitmap of width "H" in a card image. }
begin
	CX:=(CardImageWd shr 1)-(H shr 1);
end;

function CY(H:integer):integer;
begin
	CY:=(CardImageHt shr 1)-(H shr 1);
end;

procedure AddIcon(X,Y:integer;ADC:HDC;ABitmap:HBITMAP);
begin
	PutBitmap(aDC,aBitmap,x,y,SrcAnd);
end;

procedure AddBitmapScaled(target_DC:HDC; x,y,w,h:integer; source_bitmap:HBITMAP);
var
	temp_DC:HDC;
begin
	temp_DC:=CreateCompatibleDC(target_DC);
	SelectObject(temp_DC, source_bitmap);
	SetStretchBltMode(target_DC, HALFTONE);
	StretchBlt(target_DC, x, y, w, h, temp_DC, 0, 0, GetBitmapWd(source_bitmap), GetBitmapHt(source_bitmap), SRCCOPY);
	DeleteDC(temp_DC);
end;

var
	mini_suit_box_w_pxl:integer;
	pip_box_h_pxl:integer;
	pip_box_w_pxl:integer;
	suit_box_h_pxl:integer;
	suit_box_w_pxl:integer;

function PY(H:integer):integer; begin PY:=pipOfsY; end; { Y offset for the Pips }
function SSY(H:integer):integer; begin SSY:=PY(H)+pip_box_h_pxl+Max(2, LongDiv(LongMul(CardImageHt, 15), DMM_METRICS[Format].HEIGHT)); end; { Y offset for the small suits }

procedure DrawCardImage(aDC:HDC;C:byte);
{ Draw the card image for playing card "C" in "aDC". }
var
	i,w,h,x,y:integer;
	aBrush:HBRUSH;
	pen,aPen:HPEN;
	temp_DC:HDC;
	function LX(H:integer):integer; begin LX:=iRect.left+1; end;
	function LXM(H:integer):integer; begin LXM:=CardImageWd-H-LX(H); end;
	function UY(H:integer):integer; begin UY:=(CardImageHt div 3)-(H div 2); end;
	function UYM(H:integer):integer; begin UYM:=CardImageHt-UY(H)-H; end;
	function TY(H:integer):integer; begin TY:=iRect.top; end; { top suits }
	function TYM(H:integer):integer; begin TYM:=CardImageHt-TY(H)-H; end; { bottom suits }
	function VY(H:integer):integer; begin VY:=Integer(Center(H,TY(H)+(CardImageHt-TY(H)*2) div 4,TY(H)+((CardImageHt-TY(H)*2) div 4)*2-1)); end;
	function VYM(H:integer):integer; begin VYM:=CardImageHt-VY(H)-H; end;
	function EY(H:integer):integer; begin EY:=Integer(Center(H,TY(H),VY(H)+H-1)); end;
	function EYM(H:integer):integer; begin EYM:=CardImageHt-EY(H)-H; end;
	function PX(H:integer):integer; begin PX:=pipOfsX; end; { X offset for the Pips }
	function PXR(H:integer):integer; begin PXR:=CardImageWd-pipOfsX-H; end; { X offset for the reverse Pips }
	function PYR(H:integer):integer; begin PYR:=CardImageHt-pipOfsY-H; end; { Y offset for the reverse Pips }
	function SSX(H:integer):integer; begin SSX:=PX(H); end; { x offset for the small suits }
	function SSXR(H:integer):integer; begin SSXR:=CardImageWd-pipOfsX-H; end; { x for the reverse small suits }
	function SSYR(H:integer):integer; begin SSYR:=CardImageHt-SSY(H)-H; end; { Y offset for the small suits }
begin
	{ jumbo format cards need a frame around the suit icon area}
	if (CardPip(C) <= TTEN) and (Format=JumboFormat) then begin
		{if SuitColor(C)=RedSuit then
			aBrush:=GetStockObject(Black_Brush)
		else
			aBrush:=GetStockObject(gray_Brush);}
		FrameRect(aDC,iRect,GetStockObject(Black_Brush){aBrush});
	end;

	if CardPip(C) = TJOKER then begin
		AddIcon(
			CX(GetBitmapWd(DeckIcons[IJOK])),
			CY(GetBitmapHt(DeckIcons[IJOK])),
			aDC,DeckIcons[IJOK]);
		if CountryCode=UnitedStates then i:=IJKU else i:=ICJU;
		AddIcon(PX(GetBitmapWd(DeckIcons[i])), PY(GetBitmapHt(DeckIcons[i])), aDC,DeckIcons[i]);
		if CountryCode=UnitedStates then i:=IJKR else i:=ICJR;
		AddIcon(PXR(GetBitmapWd(DeckIcons[i])), PYR(GetBitmapHt(DeckIcons[i])), aDC,DeckIcons[i]);
		Exit;
	end;

	{ upright pip }
	case CardSuit(C) of
		TCLUB,TSPADE:i:=IPBU+ord(CardPip(C));
		TDIAMOND,THEART:i:=IPRU+ord(CardPip(C));
	end;
	AddBitmapScaled(adc, PX(pip_box_w_pxl), PY(pip_box_h_pxl), pip_box_w_pxl, pip_box_h_pxl, DeckIcons[i]);
	
	{ reverse pip }
	case CardSuit(C) of
		TCLUB,TSPADE:i:=IPBR+ord(CardPip(C));
		TDIAMOND,THEART:i:=IPRR+ord(CardPip(C));
	end;
	AddBitmapScaled(adc, PXR(pip_box_w_pxl), PYR(pip_box_h_pxl), pip_box_w_pxl, pip_box_h_pxl, DeckIcons[i]);
	
	{ small suits }
	AddBitmapScaled(adc, SSX(pip_box_w_pxl), SSY(mini_suit_box_w_pxl), pip_box_w_pxl, mini_suit_box_w_pxl, DeckIcons[ISLU+Ord(CardSuit(C))]);
	AddBitmapScaled(adc, SSXR(pip_box_w_pxl), SSYR(mini_suit_box_w_pxl), pip_box_w_pxl, mini_suit_box_w_pxl, DeckIcons[ISLR+Ord(CardSuit(C))]);
	
	{ single top and bottom }
	if CardPip(C) in [TDEUCE, TTHREE] then begin
		i:=ISLU+ord(CardSuit(C));
		AddBitmapScaled(adc, CX(suit_box_w_pxl), TY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		i:=ISLR+ord(CardSuit(C));
		AddBitmapScaled(adc, CX(suit_box_w_pxl), CardImageHt-TY(suit_box_h_pxl)-suit_box_h_pxl, suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
	end;
	
	{ double top and bottom }
	if CardPip(C) in [TFOUR..TTEN] then begin
		i:=ISLU+ord(CardSuit(C));
		AddBitmapScaled(adc, LX(suit_box_w_pxl), TY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		AddBitmapScaled(adc, LXM(suit_box_w_pxl), TY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		i:=ISLR+ord(CardSuit(C));
		AddBitmapScaled(adc, LX(suit_box_w_pxl), TYM(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		AddBitmapScaled(adc, LXM(suit_box_w_pxl), TYM(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
	end;

	{ double middle }
	if CardPip(C) in [TSIX,TSEVEN,TEIGHT] then begin
		i:=ISLU+ord(CardSuit(C));
		AddBitmapScaled(adc, LX(suit_box_w_pxl), CY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		AddBitmapScaled(adc, LXM(suit_box_w_pxl), CY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
	end;

	if CardPip(C) in [TSEVEN,TEIGHT] then begin
		i:=ISLU+ord(CardSuit(C));
		AddBitmapScaled(adc, CX(suit_box_w_pxl), UY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
	end;

	if CardPip(C) in [TEIGHT] then begin
		i:=ISLR+ord(CardSuit(C));
		AddBitmapScaled(adc, CX(suit_box_w_pxl), UYM(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
	end;

	if CardPip(C) in [TNINE,TTEN] then begin
		i:=ISLU+ord(CardSuit(C));
		AddBitmapScaled(adc, LX(suit_box_w_pxl), VY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		AddBitmapScaled(adc, LXM(suit_box_w_pxl), VY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		i:=ISLR+ord(CardSuit(C));
		AddBitmapScaled(adc, LX(suit_box_w_pxl), VYM(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
		AddBitmapScaled(adc, LXM(suit_box_w_pxl), VYM(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[i]);
	end;

	if CardPip(C) in [TTEN] then begin
		AddBitmapScaled(adc, CX(suit_box_w_pxl), EY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[ISLU+ord(CardSuit(C))]);
		AddBitmapScaled(adc, CX(suit_box_w_pxl), EYM(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[ISLR+ord(CardSuit(C))]);
	end;

	if CardPip(C) in [TACE,TTHREE,TFIVE,TNINE] then begin { suit in the center }
		if (CardPip(C)=TACE) and (CardSuit(C)=TSPADE) then begin
			w:=GetRectWd(iRect);
			h:=GetRectHt(iRect);
			AddBitmapScaled(adc, CX(w), CY(h), w, h, DeckIcons[IACE]);
		end
		else begin
			AddBitmapScaled(adc, CX(suit_box_w_pxl), CY(suit_box_h_pxl), suit_box_w_pxl, suit_box_h_pxl, DeckIcons[ISLU+ord(CardSuit(C))]);
		end;
	end;

	{ face cards }
	if CardPip(C) in [TJACK..TKING] then begin
		w:=GetRectWd(iRect)-2;
		h:=GetRectHt(iRect)-2;
		x:=CX(w);
		y:=CY(h);
		AddBitmapScaled(adc, x, y, w, h, DeckIcons[IFCD+(ord(CardPip(C))-ord(TJACK))*4+ord(CardSuit(C))]);
		
		aPen:=CreatePen(PS_SOLID, 1, BLUE);
		pen:=SelectObject(aDC,aPen);
		aBrush:=SelectObject(aDC, GetStockObject(NULL_BRUSH));
		Rectangle(aDC, x-1, y-1, x+w+1, y+h+1);
		SelectObject(aDC,aBrush);
		SelectObject(aDC,pen);
		DeleteObject(aPen);
	end;
end;

function ConvertIndexToSize(aCardWidthIndex:number):TCardSize;
begin
	if aCardWidthIndex<=3 
		then ConvertIndexToSize:=SMALLCARDS
	else if aCardWidthIndex<=7 
		then ConvertIndexToSize:=MEDIUMCARDS
	else
		ConvertIndexToSize:=LARGECARDS;
end;

procedure LoadResources(Fm:TCardFormat);
var
	i:integer;
begin //writeln('LoadResources(Fm:TCardFormat)');
	DeckIcons[IACE]:=LoadBitmap(HInstance,MakeIntResource(8300));

	{ pip icons }
	for i:=0 to 12 do begin
		DeckIcons[IPBU+Ord(TACE)+i]:=LoadBitmap(HInstance,MakeIntResource(ITPU[TPip(Ord(TACE)+i)]));
		DeckIcons[IPRU+Ord(TACE)+i]:=LoadBitmap(HInstance,MakeIntResource(8321+i));
		DeckIcons[IPBR+Ord(TACE)+i]:=MirrorBitmap(DeckIcons[IPBU+Ord(TACE)+i]);
		DeckIcons[IPRR+Ord(TACE)+i]:=MirrorBitmap(DeckIcons[IPRU+Ord(TACE)+i]);
	end;

	{ suit icons }
	for i:=0 to 3 do begin
		DeckIcons[ISLU+Ord(TCLUB)+i]:=LoadBitmap(HInstance, MakeIntResource(ITSU[TSuit(Ord(TCLUB)+i)]));
		DeckIcons[ISLR+Ord(TCLUB)+i]:=MirrorBitmap(DeckIcons[ISLU+Ord(TCLUB)+i]);
	end;

	{ mini suits }
	for i:=0 to 3 do begin
		DeckIcons[ISSU+Ord(TCLUB)+i]:=LoadBitmap(HInstance, MakeIntResource(ITSU[TSuit(Ord(TCLUB)+i)]));
		DeckIcons[ISSR+Ord(TCLUB)+i]:=MirrorBitmap(DeckIcons[ISSU+Ord(TCLUB)+i]);
	end;

	{ TJOKER pips }
	DeckIcons[IJKU]:=LoadBitmap(HInstance,MakeIntResource(8359));
	DeckIcons[ICJU]:=LoadBitmap(HInstance,MakeIntResource(8360));
	DeckIcons[IJKR]:=MirrorBitmap(DeckIcons[IJKU]);
	DeckIcons[ICJR]:=MirrorBitmap(DeckIcons[ICJU]);

	{ face card images (8378..8389) }
	for i:= 0 to 11 do DeckIcons[IFCD+i]:=LoadBitmap(HInstance, MakeIntResource(8378+i));

	{ TJOKER face }
	DeckIcons[IJOK]:=LoadBitmap(HInstance,MakeIntResource(8390));
end;

procedure FreeResources;
var
	i:integer;
begin
	for i:=N_ICONS downto 1 do DeleteObject(DeckIcons[i]);
	DeleteObject(x_work_bitmap);
end;

procedure InitializeIconLibrary(aCardWidthIndex:number);
var
	w,h:integer;
begin
	CardImageWd:=Instance^.SupportedWidthAt(aCardWidthIndex);
	CornerRound:=CardImageWd div 9;
	CardImageHt:=CalcHtFromWd(Format, CardImageWd);
	w:=ForceOddUp(LongDiv(LongMul(CardImageWd, DMM_METRICS[Format].INSIDE_W), DMM_METRICS[Format].WIDTH));
	h:=ForceOddUp(LongDiv(LongMul(CardImageHt, DMM_METRICS[Format].INSIDE_H), DMM_METRICS[Format].HEIGHT));
	with iRect do begin
		left:=Center(w, 0, CardImageWd-1);
		right:=left+w;
		top:=Center(h, 0, CardImageHt-1);
		bottom:=top+h;
	end;
end;

procedure Done;
begin
	DeleteObject(theCardOutlinePen);
	FreeResources;
end;

function CreateCardBackImage(card_wd,card_ht:word):HBITMAP;
var
	sysDC:HDC;
	memDC:HDC;
	memDCBitmap,memBitmap:HBITMAP;

	procedure DrawCardBack(aDC:HDC);
	const
		COLOR:TCOLORREF=$000000FF;
	var
		cx,cy,w,h:integer;
		border:word;
		rect:TRect;
		brush,aDCBrush:HBRUSH;
		pen,aDCPen:HPEN;
	begin
		border:=card_wd div 15;
		w:=card_wd-border*2;
		h:=card_ht-border*2;
		cx:=(card_wd shr 1)-(w shr 1);
		cy:=(card_ht shr 1)-(h shr 1);
		SetRect(rect,cx,cy,cx+w,cy+h);

		brush:=CreateSolidBrush(COLOR);
		FillRect(aDC,rect,brush);
		DeleteObject(brush);

		if w<45 
			then InflateRect(rect,-1,-1)
			else InflateRect(rect,-2,-2);
		FillRect(aDC,rect,GetStockObject(WHITE_BRUSH));

		InflateRect(rect,-1,-1);
		pen:= CreatePen(PS_SOLID,0,COLOR);
		brush:=CreateSolidBrush($008080FF);
		aDCPen:= SelectObject(aDC, pen);
		aDCBrush:= SelectObject(aDC, brush);
		Rectangle(aDC, rect.left, rect.top, rect.right, rect.bottom);
		SelectObject(aDC,aDCBrush);
		SelectObject(aDC,aDCPen);
		DeleteObject(brush);
		DeleteObject(pen);
	end;

begin
	sysDC:=GetDC(0);
	memDC:=CreateCompatibleDC(sysDC);
	memBitmap:=CreateCompatibleBitmap(sysDC,card_wd,card_ht);
	ReleaseDC(0,sysDC);
	memDCBitmap:=SelectObject(memDC,memBitmap);
	DrawCardShape(memDC,theCardOutlinePen,theCardBgBrush,card_wd,card_ht);
	DrawCardBack(memDC);
	SelectObject(memDC,memDCBitmap);
	DeleteDC(memDC);
	CreateCardBackImage:=memBitmap;
end;

procedure SelectWidthAt(at:number);
var
	sysDC:HDC;
begin
	if (CardImageWd<>SupportedWidthAt(at)) then begin
		InitializeIconLibrary(at);
		pipOfsX:=Max(2, LongDiv(LongMul(CardImageWd, 17), DMM_METRICS[Format].WIDTH));
		pipOfsY:=Max(2, LongDiv(LongMul(CardImageHt, 27), DMM_METRICS[Format].HEIGHT));
		PipHSpace:=iRect.left;
		pip_box_w_pxl:=PipHSpace-pipOfsX-Max(1, LongDiv(LongMul(CardImageWd, 23), DMM_METRICS[Format].WIDTH));
		pip_box_h_pxl:=LongDiv(LongMul(CardImageHt, DMM_METRICS[Format].PIP_H), DMM_METRICS[Format].HEIGHT);
		PipVSpace:=SSY(mini_suit_box_w_pxl);
		suit_box_w_pxl:=ForceOddUp(LongDiv(LongMul(CardImageWd, DMM_METRICS[Format].SUIT_W), DMM_METRICS[Format].WIDTH));
		suit_box_h_pxl:=ForceOddUp(LongDiv(LongMul(CardImageHt, DMM_METRICS[Format].SUIT_H), DMM_METRICS[Format].HEIGHT));
		mini_suit_box_w_pxl:=LongDiv(LongMul(pip_box_w_pxl, 12), 10);
		SysDC:=GetDC(0);
		x_work_bitmap:=CreateCompatibleBitmap(SysDC, CardImageWd, CardImageHt);
		ReleaseDC(0, SysDC);
	end;
end;

function TheCardFactory.CreateBackBitmapAt(at:number):drawing.bitmap;
begin
	at:=Min(Integer(at), Integer(SupportedSizeCount));
	CreateBackBitmapAt:=CreateCardBackImage(SupportedWidthAt(at),SupportedHeightAt(at));
end;

function OptXSpace:integer;
begin
	OptXSpace:= winCardFactory.PiphSpace;
end;

function OptYSpace:integer;
begin
	OptYSpace:= winCardFactory.PipvSpace;
end;

procedure SelectDesiredCardWidth(desired_width:word);
begin
	SelectWidthAt(ConvertWidthToIndex(desired_width));
end;

function TheCardFactory.CreateFaceBitmapAt(at:number;card:cards.Card):drawing.bitmap;
var
	sysDC:HDC;
	memDC:HDC;
	memDCBitmap,memBitmap:HBITMAP;
begin
	at:=Min(Integer(at), Integer(SupportedSizeCount));
	SelectWidthAt(at);
	sysDC:=GetDC(0);
	memDC:=CreateCompatibleDC(sysDC);
	memBitmap:=CreateCompatibleBitmap(sysDC, CardImageWd, CardImageHt);
	ReleaseDC(0,sysDC);
	memDCBitmap:=SelectObject(memDC,memBitmap);
	DrawCardShape(memDC, theCardOutlinePen, theCardBgBrush, CardImageWd, CardImageHt);
	DrawCardImage(memDC, CardToTCard(card));
	SelectObject(memDC, memDCBitmap);
	DeleteDC(memDC);
	CreateFaceBitmapAt:=memBitmap;
end;

begin
	the_card_factory.Construct;
	Format:=RegularFormat;
	theCardBgBrush:=GetStockObject(WHITE_BRUSH);
	theCardOutlinePen:=CreatePen(PS_SOLID, 1, RGB_DARK_GRAY);
	LoadResources(Format);
end.
