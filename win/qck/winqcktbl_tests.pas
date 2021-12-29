{ (C) 2011 Wesley Steiner }

{$MODE FPC}

unit winqcktbl_tests;

interface

implementation

uses
	windows,
	windowsx,
	gdiex,
	std,
	xy,
	punit,
	cards,
	cardFactory,
	qcktbl,
	winCardFactory,
	winqcktbl, winqcktbltestable;
	
type
	testable_OGame=object(Game)
		constructor Construct;
		function PileRows:word; virtual;
		function PileColumns:word; virtual;
		function PileSpacing:integer; virtual;
	end;

	CardGraphicsManagerStub=object(BaseCardGraphicsManager)
		constructor Construct;
		function BestFit(n_columns,n_rows:word;space_between:integer;total_width,total_height:word;edge_margin:integer):word; virtual;
		function CurrentWidth:word; virtual; unimplemented;
		function CurrentHeight:word; virtual; unimplemented;
		procedure SelectWidth(in_pixels:word); virtual;
	end;

	testable_OTabletop=object(OTabletop)
		constructor Construct;
		function Create(frame:HWND;w,h:number):HWND; virtual;
		constructor TestConstruct;
	end;
	
	mock_OTabletop=object(testable_OTabletop)
	end;
	
	testable_OGenPileOfCards=object(GenPileOfCards)
		constructor Construct;
	end;

	OViewStub=object(OTabletop)
		constructor Construct;
	end;

	GenPileOfCardsTester=object(testable_OGenPileOfCards)
		CardIsPlaceHolderResult:boolean;
		CardIsCoveredResult:boolean;
		function CardIsCovered(aIndex:integer):boolean; virtual;
		function CardIsPlaceHolder(aIndex:integer):boolean; virtual;
		procedure AddTestCard;
	end;

	testable_OSquaredpileprop=object(OSquaredpileprop)
	end;
	
	testable_ODeckProp=object(ODeckprop)
	end;
	
constructor CardGraphicsManagerStub.Construct; begin end;
function CardGraphicsManagerStub.BestFit(n_columns,n_rows:word;space_between:integer;total_width,total_height:word;edge_margin:integer):word; unimplemented; begin BestFit:=0; end;
function CardGraphicsManagerStub.CurrentWidth:word; unimplemented; begin CurrentWidth:=0; end;
function CardGraphicsManagerStub.CurrentHeight:word; unimplemented; begin CurrentHeight:=0; end;
procedure CardGraphicsManagerStub.SelectWidth(in_pixels:word); begin NotImplemented; end;

constructor testable_OTabletop.Construct;
begin
	inherited Construct(0,0,false);
end;

procedure OnDrawItemReturnValue;
var
	view:testable_OTabletop;
begin
	view.Construct;
	AssertAreEqual(0,view.OnMsg(WM_DRAWITEM,0,0));
	AssertAreEqual(1,view.OnMsg(WM_DRAWITEM,172,0));
end;

constructor testable_OGame.Construct; begin end;
function testable_OGame.PileRows:word; begin PileRows:=3; end;
function testable_OGame.PileColumns:word; begin PileColumns:=5; end;
function testable_OGame.PileSpacing:integer; begin PileSpacing:=10; end;

type
	FakeGame=object(testable_OGame)
		function PileRows:word; virtual;
		function PileColumns:word; virtual;
	end;

function FakeGame.PileRows:word; begin PileRows:=4; end;
function FakeGame.PileColumns:word; begin PileColumns:=3; end;

type
	FakeCardGraphicsManager1=object(CardGraphicsManagerStub)
		myBestFitResult:word;
		mySelectWidth:CallTelemetry;
		mySelectWidthArg1:word;
		function BestFit(nPileColumns,nPileRows:word;spaceBetweenPiles:integer;aTableWidth,aTableHeight:word;edgeMargin:integer):word; virtual;
		procedure SelectWidth(aWidthInPixels:word); virtual;
	end;

function FakeCardGraphicsManager1.BestFit(nPileColumns,nPileRows:word;spaceBetweenPiles:integer;aTableWidth,aTableHeight:word;edgeMargin:integer):word; begin BestFit:=myBestFitResult; end;

procedure FakeCardGraphicsManager1.SelectWidth(aWidthInPixels:word);
begin
	mySelectWidth.WasCalled:=TRUE;
	mySelectWidthArg1:=aWidthInPixels;
end;

procedure Test_UpdateCardSize;
var
	selector:FakeGame;
	manager:FakeCardGraphicsManager1;
begin
	selector.Construct;
	manager.Construct;
	CardImageWd:=40;
	manager.mySelectWidth.WasCalled:=false;

	manager.myBestFitResult:=40;
	UpdateCardSize(400,300,selector,@manager);
	punit.Assert.IsFalse(manager.mySelectWidth.WasCalled);
	manager.myBestFitResult:=41;
	UpdateCardSize(400,300,selector,@manager);
	punit.Assert.IsTrue(manager.mySelectWidth.WasCalled);
	punit.Assert.AreEqual(41,manager.mySelectWidthArg1);
end;

constructor OViewStub.Construct; begin end;

type
	FakeCardGraphicsManager=object(CardGraphicsManagerStub)
		procedure SetCardSize(dx,dy:integer);
	end;

procedure FakeCardGraphicsManager.SetCardSize(dx,dy:integer);
begin
	CardImageWd:=dx;
	CardImageHt:=dy;
end;
	procedure test_CardCenterToAnchor;
var
	fake_manager:FakeCardGraphicsManager;
	pt:xypair;
begin
	fake_manager.Construct;
	fake_manager.SetCardSize(11,15);
	pt:=CardCenterToAnchor(MakeXYPair(45,67));
	AssertAreEqual(40,xypairWrapper(pt).x);
	AssertAreEqual(60,xypairWrapper(pt).y);
end;

procedure test_CardAnchorToCenter;
var
	fake_manager:FakeCardGraphicsManager;
	pt:xypair;
begin
	fake_manager.Construct;
	fake_manager.SetCardSize(5,7);
	pt:=CardAnchorToCenter(MakeXYPair(1,2));
	AssertAreEqual(3,xypairWrapper(pt).x);
	AssertAreEqual(5,xypairWrapper(pt).y);
end;

constructor testable_OTabletop.TestConstruct;
begin
	PostConstruct;
end;

function testable_OTabletop.Create(frame:HWND;w,h:number):HWND;
begin
	Create:=NULL_HANDLE;
end;

type
	MyTabletop=object(testable_OTabletop)
		myMargin:integer;
		myWidth,myHeight:word;
		myRefreshWasCalled:boolean;
		RefreshRectArg:TRect;
		constructor Construct;
		constructor Construct(w,h:word);
		function AddTestProp(x,y:integer;w,h:number):FakeHotspotP;
		function Margin:integer; virtual;
		function Height:word; virtual;
		function Width:word; virtual;
		function ClientAreaWd:integer; virtual;
		procedure RefreshRect(const rRect:TRect); virtual;
		procedure HandleMouseClick(state:boolean);
	end;

constructor MyTabletop.Construct(w,h:word);
begin
	inherited Construct;
	myMargin:=MIN_EDGE_MARGIN;
	myWidth:=w;
	myHeight:=h;
end;

constructor MyTabletop.Construct;
begin
	Construct(800,600);
end;

procedure MyTabletop.RefreshRect(const rRect:TRect);
begin
	myRefreshWasCalled:=TRUE;
	RefreshRectArg:=rRect;
end;

function MyTabletop.Height:WORD; 
begin //writeln('MyTabletop.Height');
	Height:=myHeight; 
end;

function MyTabletop.Width:WORD; 
begin //writeln('MyTabletop.Width');
	Width:=myWidth; 
end;

function MyTabletop.Margin:integer; begin Margin:=myMargin; end;

function MyTabletop.ClientAreaWd:integer; 
begin 
	ClientAreaWd:=myWidth; 
end;

function MyTabletop.AddTestProp(x,y:integer;w,h:number):FakeHotspotP;
var
	spot:FakeHotspotP;
begin
	spot:=New(FakeHotspotP,Construct(x,y,w,h));
	HotList.Insert(spot);
	spot^.Enable;		
	AddTestProp:=spot;
end;

procedure MyTabletop.HandleMouseClick(state:boolean);
var
	spot:PHotspot;
begin
	if state then begin
		HotList.Init(10,10);
		spot:=New(PHotspot,Construct(100,100));
		HotList.Insert(spot);
		spot^.Enable;		
	end;
end;

procedure prop_HitTest;
var
	prop:FakeHotspot;
begin
	prop.Construct(10,20,5,5);
	prop.Disable;
	AssertIsFalse(prop.HitTest(0,0));
	prop.Enable;
	AssertIsTrue(prop.HitTest(10,20));
	AssertIsFalse(prop.HitTest(0,0));
end;

type
	DblClickView=object(MyTabletop)
		OnDoubleTappedValue:boolean;
		function OnDoubleTapped(x,y:integer):boolean; virtual;
	end;

function DblClickView.OnDoubleTapped(x,y:integer):boolean;
begin
	OnDoubleTapped:=OnDoubleTappedValue;
end;
	
procedure OnLButtonDblClickReturnCodes;
var
	view:DblClickView;
	prop:FakeHotspotP;
begin
	view.Construct;
	view.OnDoubleTappedValue:=FALSE;
	AssertIsTrue(view.OnLButtonDblClick(0,999,888)<>0);
	view.OnDoubleTappedValue:=TRUE;
	AssertIsTrue(view.OnLButtonDblClick(0,999,888)=0);
end;

procedure Test_OnLButtonDown_return_codes;
var
	view:MyTabletop;
	prop:FakeHotspotP;
begin
	view.Construct;
	punit.Assert.IsTrue(view.OnLButtonDown(0,999,888)<>0);
	prop:=view.AddTestProp(10,20,30,40);
	punit.Assert.IsTrue(view.OnLButtonDown(0,9,19)<>0);
	prop^.OnPressed_state:=TRUE;
	AssertAreEqual(0,view.OnLButtonDown(0,10,20));
	prop^.OnPressed_state:=FALSE;
	punit.Assert.IsTrue(view.OnLButtonDown(0,11,21)<>0);
end;

procedure test_TTableView_OnLButtonUp;
var
	tester:MyTabletop;
	aHotSpot:FakeHotspot;
begin
	tester.Construct;
	aHotspot.Construct(10,10);
	aHotspot.Disable;
	aHotspot.OnCapturedRelease_was_called:=FALSE;
	tester.HotList.Insert(@aHotspot);
	tester.OnLButtonUp(0,5,5);
	AssertIsFalse(aHotspot.OnCapturedRelease_was_called);
end;

procedure OnLButtonUp_OnReleasedTarget_fires_when_lit;
var
	view:MyTabletop;
	target_prop:FakeHotspot;
begin
	view.Construct;
	target_prop.Construct(10,10,10,10);
	view.HotList.Insert(@target_prop);
	target_prop.OnCapturedRelease_was_called:=FALSE;
	view.CapturedReleaseTarget:=@target_prop;
	view.OnLButtonUp(0,-999,-999);
	AssertIsTrue(target_prop.OnCapturedRelease_was_called);
end;

procedure OnLButtonUp_OnReleasedTarget_trumps_all_when_lit;
var
	view:MyTabletop;
	first_prop,target_prop,last_prop:FakeHotspot;
begin
	view.Construct;
	first_prop.Construct(0,0,10,10);
	target_prop.Construct(10,10,10,10);
	last_prop.Construct(20,20,10,10);
	view.HotList.Insert(@first_prop);
	view.HotList.Insert(@target_prop);
	view.HotList.Insert(@last_prop);
	view.CapturedReleaseTarget:=@target_prop;
	last_prop.OnCapturedRelease_was_called:=FALSE;
	view.OnLButtonUp(0,21,21);
	AssertIsFalse(last_prop.OnCapturedRelease_was_called);
end;

procedure TestTTableViewBottom;
var
	view:MyTabletop;
begin
	view.Construct;
	view.myHeight:=1234;
	AssertAreEqual(Integer(view.Height)-view.Margin,view.Bottom);
end;

procedure TestTTableViewTop;
var
	view:MyTabletop;
begin
	view.Construct;
	AssertAreEqual(view.Margin,view.Top);
end;

procedure TestHotspotSetStickyPos;
var
	view:MyTabletop;
	aHotspot:Hotspot;
begin
	view.Construct;
	view.myWidth:=500;
	view.myHeight:=300;
	aHotspot.Construct(100,50);
	aHotspot.myTabletop:=@view;
	aHotspot.SetStickyPos(BOTTOM_LEFT);
	AssertAreEqual(aHotspot.myTabletop^.Margin,aHotspot.Anchor.x);
	AssertAreEqual(aHotspot.myTabletop^.Bottom-50,aHotspot.Anchor.y);
	aHotspot.Construct(20,42);
	aHotspot.myTabletop:=@view;
	aHotspot.SetStickyPos(BOTTOM_CENTER);
	AssertAreEqual(Center(21,0,view.ClientAreaWd),aHotspot.Anchor.x);
	AssertAreEqual(aHotspot.myTabletop^.Bottom-42,aHotspot.Anchor.y);
end;

procedure OnSizeInvokesPropResizing;
var
	view:MyTabletop;
begin
	view.Construct;
	view.HotList.Insert(new(FakeHotspotP,Construct));
	view.HotList.Insert(new(FakeHotspotP,Construct));
	view.HotList.Insert(new(FakeHotspotP,Construct));
	GetAnchorPoint_call_count:= 0;
	view.OnSize(SIZE_RESTORED,$0123,$08FF);
	AssertAreEqual(3,GetAnchorPoint_call_count);
	AssertAreEqual($0123,GetAnchorPoint_wd_arg);
	AssertAreEqual($08FF,GetAnchorPoint_ht_arg);
end;

procedure TestSetDesc;
var
	aPile:GenPileOfCardsTester;
begin
	aPile.Construct;
	aPile.SetDesc(nil);
	punit.Assert.EqualPtr(nil, aPile.Desc);
	aPile.SetDesc('abc');
	punit.Assert.EqualText('abc', aPile.Desc);
end;

procedure TestAppendDesc;
var
	aPile:GenPileOfCardsTester;
begin
	aPile.Construct;
	aPile.AppendDesc(nil);
	punit.Assert.EqualPtr(nil, aPile.Desc);
	aPile.AppendDesc('Text');
	punit.Assert.EqualText('Text', aPile.Desc);
	aPile.AppendDesc(' sentence.');
	punit.Assert.EqualText('Text sentence.', aPile.Desc);
	aPile.AppendDesc('Next sentence.');
	punit.Assert.EqualText('Text sentence. Next sentence.', aPile.Desc);
end;

procedure TestHotspotGetSpanRect;
var
	view:MyTabletop;
	aHotspot:Hotspot;
	aSpanRect:TRect;
begin
	Randomize;
	view.Construct;
	aHotspot.Construct(100, 50);
	aHotspot.Anchor.x:=20;
	aHotspot.Anchor.y:=30;
	aHotspot.GetSpanRect(aSpanRect);
	AssertAreEqual(20, aSpanRect.left);
	AssertAreEqual(30, aSpanRect.top);
	AssertAreEqual(120, aSpanRect.right);
	AssertAreEqual(80, aSpanRect.bottom);
end;

type
	NotDerivedFromGenPileOfCards=object(HotSpot)
		constructor Construct;
	end;

constructor NotDerivedFromGenPileOfCards.Construct; begin end;

type
	DerivedFromGenPileOfCards = object(GenPileOfCards)
		mAddToSizeOf:integer;
		constructor Construct;
	end;

constructor DerivedFromGenPileOfCards.Construct; begin end;

procedure TestIsDerivedFromGenPileOfCards;
var
	aNotDerivedFromGenPileOfCards:NotDerivedFromGenPileOfCards;
	aDerivedFromGenPileOfCards:DerivedFromGenPileOfCards;
begin
	aNotDerivedFromGenPileOfCards.Construct;
	aDerivedFromGenPileOfCards.Construct;
	punit.Assert.IsFalse(IsDerivedFromGenPileOfCards(@aNotDerivedFromGenPileOfCards));
	punit.Assert.IsTrue(IsDerivedFromGenPileOfCards(@aDerivedFromGenPileOfCards));
end;

type
	TestGenPileOfCardsBase = object(GenPileOfCardsTester)
		m_aTopToWasCalled:boolean;
		m_TopTo_Target:GenPileOfCardsP;
		m_aTopFaceupResult:boolean;
		m_aTopcard:TCard;
		m_aTopFlipWasCalled:boolean;
		function IsEmpty:boolean; virtual;
		function Topcard:TCard; virtual;
		function TopFaceDown:boolean; virtual;
		function TopFaceup:boolean; virtual;
		procedure OnTopcardFlipped; virtual;
		procedure TopcardTo(aDest:GenPileOfCardsP); virtual;
	end;

function TestGenPileOfCardsBase.IsEmpty:boolean; begin IsEmpty:= false; end;
function TestGenPileOfCardsBase.TopFaceDown:boolean; begin topFaceDown:= true; end;
function TestGenPileOfCardsBase.Topcard:TCard; begin Topcard:= m_aTopcard; end;
function TestGenPileOfCardsBase.TopFaceup:boolean; begin TopFaceup:= m_aTopFaceupResult; end;

procedure TestGenPileOfCardsBase.OnTopcardFlipped; begin m_aTopFlipWasCalled:= true; end;

procedure TestGenPileOfCardsBase.TopcardTo(aDest:GenPileOfCardsP);
begin
	m_aTopToWasCalled:= true;
	m_TopTo_Target:= aDest;
end;

type
	TestGenPileOfCards2 = object(TestGenPileOfCardsBase)
		procedure TryTopcardToDblClkTargets; virtual;
	end;

procedure TestGenPileOfCards2.TryTopcardToDblClkTargets; begin GenPileOfCards.TryTopcardToDblClkTargets; end;

type
	FakeCompatibleHotSpot=object(GenPileOfCards)
		m_IsDblClkTarget_result:boolean;
		m_Accepts_result:boolean;
		constructor Init;
		function Accepts(aCard:TCard):boolean; virtual;
		function IsDblClkTarget:boolean; virtual;
	end;

function FakeCompatibleHotSpot.Accepts(aCard:TCard):boolean;
begin
	Accepts:= m_Accepts_result;
end;

function FakeCompatibleHotSpot.IsDblClkTarget:boolean;
begin
	IsDblClkTarget:= m_IsDblClkTarget_result;
end;

constructor FakeCompatibleHotSpot.Init; begin end;

procedure test_TryTopcardToDblClkTargets;
var
	tabletop:mock_OTabletop;
	aFake_IncompatibleHotSpot:FakeHotspot;
	aFakeCompatibleHotSpot:FakeCompatibleHotSpot;
	aFakeCompatibleHotSpot1:FakeCompatibleHotSpot;
	aFakeCompatibleHotSpot2:FakeCompatibleHotSpot;
	aTestPile:TestGenPileOfCards2;
begin
	tabletop.Construct;
	aTestPile.Construct;
	aTestPile.myTabletop:= @tabletop;

	{ top card should not be transfered if no HotSpots exist }
	aTestPile.m_aTopToWasCalled:= false;
	aTestPile.m_aTopcard:= MakeCard(TACE, TSPADES);
	aTestPile.TryTopcardToDblClkTargets;
	punit.Assert.IsFalse(aTestPile.m_aTopToWasCalled);

	{ top card should not be transfered if no compatible HotSpots exist }
	aFake_IncompatibleHotSpot.Construct;
	tabletop.HotList.Insert(@aFake_IncompatibleHotSpot);
	aTestPile.m_aTopToWasCalled:= false;
	aTestPile.TryTopcardToDblClkTargets;
	punit.Assert.IsFalse(aTestPile.m_aTopToWasCalled);

	{ top card should not be transfered if
			1. a compatible hotspot exists
			2. it is not a dblclk target }
	aFakeCompatibleHotSpot.Init;
	aFakeCompatibleHotSpot.m_IsDblClkTarget_result:= false;
	tabletop.HotList.Insert(@aFakeCompatibleHotSpot);
	aTestPile.m_aTopToWasCalled:= false;
	aTestPile.TryTopcardToDblClkTargets;
	punit.Assert.IsFalse(aTestPile.m_aTopToWasCalled);

	{ top card should not be transfered if
			1. a compatible hotspot exists,
			2. it is a dblclk target
			3. it does not accept the Topcard }
	aFakeCompatibleHotSpot.Init;
	aFakeCompatibleHotSpot.m_IsDblClkTarget_result:= true;
	aFakeCompatibleHotSpot.m_Accepts_result:= false;
	tabletop.HotList.Insert(@aFakeCompatibleHotSpot);
	aTestPile.m_aTopToWasCalled:= false;
	aTestPile.TryTopcardToDblClkTargets;
	punit.Assert.IsFalse(aTestPile.m_aTopToWasCalled);

	{ top card should be transfered if
			1. a compatible hotspot exists,
			2. it is a dblclk target
			3. it does accept the Topcard }
	aFakeCompatibleHotSpot.Init;
	aFakeCompatibleHotSpot.m_IsDblClkTarget_result:= true;
	aFakeCompatibleHotSpot.m_Accepts_result:= true;
	tabletop.HotList.Insert(@aFakeCompatibleHotSpot);
	aTestPile.m_aTopToWasCalled:= false;
	aTestPile.TryTopcardToDblClkTargets;
	punit.Assert.IsTrue(aTestPile.m_aTopToWasCalled);

	{ top card should be transfered to front-most target that accepts it }
	aFakeCompatibleHotSpot1.Init;
	aFakeCompatibleHotSpot1.m_IsDblClkTarget_result:= true;
	aFakeCompatibleHotSpot1.m_Accepts_result:= true;
	aFakeCompatibleHotSpot2.Init;
	aFakeCompatibleHotSpot2.m_IsDblClkTarget_result:= true;
	aFakeCompatibleHotSpot2.m_Accepts_result:= true;

	tabletop.HotList.Insert(@aFakeCompatibleHotSpot1);
	tabletop.HotList.Insert(@aFakeCompatibleHotSpot2);
	aTestPile.TryTopcardToDblClkTargets;
	punit.Assert.EqualPtr(@aFakeCompatibleHotSpot2, aTestPile.m_TopTo_Target);
end;

type
	TestGenPileOfCards3 = object(TestGenPileOfCardsBase)
		m_aTryTopcardToDblClkTargetsWasCalled:boolean;
		procedure TryTopcardToDblClkTargets; virtual;
	end;

procedure TestGenPileOfCards3.TryTopcardToDblClkTargets; begin m_aTryTopcardToDblClkTargetsWasCalled:= true; end;

procedure TestGenPileOfCardsTopSelected;
var
	aTestPile:TestGenPileOfCards3;
begin
	aTestPile.Construct;

	aTestPile.m_aTopFaceupResult:= true;
	aTestPile.m_aTryTopcardToDblClkTargetsWasCalled:= false;
	aTestPile.TopSelected;
	punit.Assert.IsTrue(aTestPile.m_aTryTopcardToDblClkTargetsWasCalled);

	aTestPile.m_aTopFaceupResult:= false;
	aTestPile.m_aTryTopcardToDblClkTargetsWasCalled:=false;
	aTestPile.m_aTopFlipWasCalled:= false;
	aTestPile.TopSelected;
	punit.Assert.IsFalse(aTestPile.m_aTryTopcardToDblClkTargetsWasCalled);
end;

type
	OnButtonReleaseTester=object(GenPileOfCardsTester)
		OnCardSelected_was_called:boolean;
		OnCardSelected_arg:number;
		Size_result:word;
		PointHitsCard_result:integer;
		OnTopcardTapped_was_called:boolean;
		OnTopcardTapped_result:boolean;
		CanSelectCard_result:boolean;
		function CanSelectCardAt(n:number):boolean; virtual;
		function IsEmpty:boolean; virtual;
		function OnCardAtTapped(n:number):boolean; virtual;
		function OnTopcardTapped:boolean; virtual;
		function PointHitsCard(dx,dy:integer):integer; virtual;
		function Size:integer; virtual;
		procedure TopSelected; virtual;
	end;

function OnButtonReleaseTester.IsEmpty:boolean; begin IsEmpty:=FALSE; end;
function OnButtonReleaseTester.CanSelectCardAt(n:number):boolean; begin CanSelectCardAt:=CanSelectCard_result; end;
function OnButtonReleaseTester.Size:integer; begin Size:=Size_result; end;
function OnButtonReleaseTester.PointHitsCard(dx,dy:integer):integer; begin PointHitsCard:=PointHitsCard_result; end;

function OnButtonReleaseTester.OnCardAtTapped(n:number):boolean;
begin
	OnCardSelected_was_called:=TRUE;
	OnCardSelected_arg:=n;
	OnCardAtTapped:=TRUE;
end;

function OnButtonReleaseTester.OnTopcardTapped:boolean;
 begin
	OnTopcardTapped_was_called:=TRUE;
	OnTopcardTapped:=OnTopcardTapped_result;
end;

procedure OnButtonReleaseTester.TopSelected; begin end;

procedure test_GenPileOfCards_OnButtonRelease;
var
	tester:OnButtonReleaseTester;
begin
	tester.Construct;
	tester.CanSelectCard_result:=TRUE;

	tester.OnCardSelected_was_called:=FALSE;
	tester.OnCardSelected_arg:=1;
	tester.Size_result:=10;
	tester.PointHitsCard_result:=3;
	tester.OnReleased(0,0);
	AssertIsTrue(tester.OnCardSelected_was_called);
	AssertAreEqual(3,tester.OnCardSelected_arg);

	tester.OnCardSelected_was_called:=FALSE;
	tester.OnCardSelected_arg:=1;
	tester.PointHitsCard_result:=10;
	tester.OnTopcardTapped_was_called:=FALSE;
	tester.OnTopcardTapped_result:=TRUE;
	tester.OnReleased(0,0);
	AssertIsTrue(tester.OnTopcardTapped_was_called);
	AssertIsFalse(tester.OnCardSelected_was_called);

	tester.CanSelectCard_result:=FALSE;
	tester.OnCardSelected_was_called:=FALSE;
	tester.PointHitsCard_result:=5;
	tester.OnReleased(0,0);
	AssertIsFalse(tester.OnCardSelected_was_called);
end;

procedure TestHotspotRefreshRect;
var
	view:MyTabletop;
	aHotspot:Hotspot;
	aPrevSpan:TRect;
begin
	view.Construct;
	aHotspot.Construct(50, 75);
	aHotspot.myTabletop:=@view;
 	view.AddProp(@aHotSpot, MakeXYPair(100, 200));
	SetRect(aPrevSpan, 100, 200, 150, 275);
	view.myRefreshWasCalled:= false;
	aHotspot.RefreshRect(aPrevSpan);
	punit.Assert.IsTrue(view.myRefreshWasCalled);
	AssertAreEqual(100, view.RefreshRectArg.left);
	AssertAreEqual(200, view.RefreshRectArg.top);
	AssertAreEqual(150, view.RefreshRectArg.right);
	AssertAreEqual(275, view.RefreshRectArg.bottom);

	SetRect(aPrevSpan, 99, 201, 149, 276);
	view.myRefreshWasCalled:= false;
	aHotspot.RefreshRect(aPrevSpan);
	AssertAreEqual(99, view.RefreshRectArg.left);
	AssertAreEqual(200, view.RefreshRectArg.top);
	AssertAreEqual(150, view.RefreshRectArg.right);
	AssertAreEqual(276, view.RefreshRectArg.bottom);
end;

procedure TestCardIsCoveredSquaredPile;
var
	aPile:GenPileOfCards;
begin
	aPile.Construct(new(PPileOfCards,Construct(3)), 0, 0, 0);
	aPile.ThePile^.Add(MakeCard(TJACK, TSPADE));
	punit.Assert.IsFalse(aPile.CardIsCovered(1));
	aPile.ThePile^.Add(MakeCard(3, TSPADE));
	punit.Assert.IsTrue(aPile.CardIsCovered(1));
end;

procedure TestCardIsCoveredWithPlaceHoldersAbove;
var
	aPile:GenPileOfCards;
begin
	aPile.Construct(new(PPileOfCards, Construct(3)), 0, 0, 0);
	aPile.ThePile^.Add(MakeCard(TJACK, TSPADE));
	aPile.ThePile^.Add(NULL_CARD);
	aPile.ThePile^.Add(NULL_CARD);
	punit.Assert.IsFalse(aPile.CardIsCovered(1));
end;

procedure TestCardIsCoveredSkewedPile;
var
	aPile:GenPileOfCards;
begin
	aPile.Construct(new(PPileOfCards, Construct(3)), 0, 1, 1);
	aPile.ThePile^.Add(MakeCard(TJACK, TSPADE));
	aPile.ThePile^.Add(MakeCard(3, TSPADE));
	punit.Assert.IsFalse(aPile.CardIsCovered(1));
end;

type
	PointHitsCardTester=object(GenPileOfCardsTester)
		constructor Construct;
	end;

constructor PointHitsCardTester.Construct;
begin
	thePile:=new(PPileOfCards,Construct(10));
	Anchor.x:=200;
	Anchor.y:=300;
	SetCardDx(10);
	SetCardDy(10);
end;

procedure GenPileOfCardsTester.AddTestCard;
begin
	ThePile^.Add(MakeCard(10,TCLUBS));
end;

procedure test_GenPileOfCards_PointHitsCard;
var
	tester:PointHitsCardTester;
begin
	tester.Construct;
	AssertAreEqual(0,tester.PointHitsCard(-123,-456));
	tester.AddTestCard;
	AssertAreEqual(1,tester.PointHitsCard(0,0));
	AssertAreEqual(0,tester.PointHitsCard(0,-1));
	AssertAreEqual(0,tester.PointHitsCard(CardGraphicsManager.Instance^.CurrentWidth,CardGraphicsManager.Instance^.CurrentHeight));
	tester.AddTestCard;
	AssertAreEqual(2,tester.PointHitsCard(15,15));
	AssertAreEqual(1,tester.PointHitsCard(5,5));
end;

procedure test_ConvertPauseToMillSeconds;

begin
	AssertAreEqual(0,ConvertPauseToMillSeconds(0));
	AssertAreEqual(BASEDELAY*12,ConvertPauseToMillSeconds(12));
end;

type
	GetCardRectAtTester=object(GenPileOfCardsTester)
		constructor Construct;
	end;

constructor GetCardRectAtTester.Construct;

begin
	thePile:=new(PPileOfCards,Construct(10));
	Anchor.x:=100;
	Anchor.y:=500;
	SetCardDx(10);
	SetCardDy(-10);
end;

procedure test_GenPileOfCards_GetCardRectAt;

var
	tester:GetCardRectAtTester;
	r:TRect;
	begin
	tester.Construct;
	r:=tester.GetCardAtRect(5);
	AssertAreEqual(40,r.left);
	AssertAreEqual(-40,r.top);
	AssertAreEqual(40+CardGraphicsManager.Instance^.CurrentWidth,r.right);
	AssertAreEqual(-40+CardGraphicsManager.Instance^.CurrentHeight,r.bottom);
end;

procedure test_Hotspot_SetRelativeOffset;
var
	prop:Hotspot;
begin
	prop.Construct(10,20);
	prop.Anchor.x:=100;
	prop.Anchor.y:=500;
	prop.SetRelativeOffset(1000,900);
	AssertAreEqual(-400,xypairWrapper(prop.offset_from_center).x);
	AssertAreEqual(+50,xypairWrapper(prop.offset_from_center).y);
end;

procedure test_GenPileOfCards_OnPressed();
var
	pile:GenPileOfCardsTester;
begin
	pile.Construct;
	AssertIsFalse(pile.OnPressed(0,0));
	pile.AddTestCard;
	AssertIsTrue(pile.OnPressed(1,1));
end;

type
	CanSelectCardTester=object(GenPileOfCardsTester)
		Size_result:integer;
		function Size:integer; virtual;
	end;
	function CanSelectCardTester.Size:integer; begin Size:=Size_result; end;

procedure test_GenPileOfCards_CanSelectCard;
var
	tester:CanSelectCardTester;
begin
	tester.Construct;
	tester.Size_result:=8;
	AssertIsTrue(tester.CanSelectCardAt(8));
	AssertIsFalse(tester.CanSelectCardAt(7));
end;

procedure TestFindPlaceHolder;

var
	aPile:GenPileOfCards;

begin
	aPile.Construct(new(PPileOfCards, Construct(3)), 0, 0, 0);

	AssertAreEqual(1, aPile.FindPlaceHolder);

	aPile.ThePile^.Add(MakeCard(TJACK, TSPADE));
	aPile.ThePile^.Add(MakeCard(TJACK, THEART));
	aPile.ThePile^.Add(MakeCard(TJACK, TCLUB));

	AssertAreEqual(4, aPile.FindPlaceHolder);

	aPile.ThePile^.ref(1)^:= NULL_CARD;
	AssertAreEqual(1, aPile.FindPlaceHolder);

	aPile.ThePile^.ref(1)^:= MakeCard(TJACK, TSPADE);
	aPile.ThePile^.ref(2)^:= NULL_CARD;
	AssertAreEqual(2, aPile.FindPlaceHolder);

	aPile.ThePile^.ref(2)^:= MakeCard(TJACK, TSPADE);
	aPile.ThePile^.ref(3)^:= NULL_CARD;
	AssertAreEqual(3, aPile.FindPlaceHolder);
end;

constructor testable_OGenPileOfCards.Construct;
begin
	inherited Construct(new(PPileOfCards,Construct(52)),0,0,0);
end;

function GenPileOfCardsTester.CardIsCovered(aIndex:integer):boolean;
begin
	CardIsCovered:= CardIsCoveredResult;
end;

function GenPileOfCardsTester.CardIsPlaceHolder(aIndex:integer):boolean;
begin
	CardIsPlaceHolder:=CardIsPlaceHolderResult;
end;

procedure TestGenPileOfCardsIsDblClkTarget;
var
	aTestGenPileOfCards:GenPileOfCardsTester;
begin
	aTestGenPileOfCards.Construct;
	punit.Assert.IsFalse(aTestGenPileOfCards.IsDblClkTarget);
end;

procedure TestHotspotObjectClass;
var
	aHotspot:Hotspot;
begin
	aHotspot.Construct(20, 30);
	AssertAreEqual(HCLASS.BASE, aHotspot.ObjectClass);
end;

procedure TestGenPileOfCardsObjectClass;
var
	aTestGenPileOfCards:GenPileOfCardsTester;
begin
	aTestGenPileOfCards.Construct;
	AssertAreEqual(HCLASS.GENPILEOFCARDS, aTestGenPileOfCards.ObjectClass);
end;

procedure TestGenPileOfCardsGetCardX;
var
	aPile:GenPileOfCardsTester;
begin
	aPile.Construct;
	aPile.my_card_dx:= 0;
	aPile.my_card_dy:= 0;

	{ should return the X offset from the anchor point of the nth card in the pile }

	{ default DX }

	AssertAreEqual(0, aPile.GetCardX(1));
	AssertAreEqual(0, aPile.GetCardX(3));

	{ + DX }

	aPile.my_card_dx:= 5;
	AssertAreEqual(0, aPile.GetCardX(1));
	AssertAreEqual(10, aPile.GetCardX(3));
end;

procedure TestGenPileOfCardsGetCardY;

	var
		aPile:GenPileOfCardsTester;

	begin
		aPile.Construct;
		aPile.my_card_dx:= 0;
		aPile.my_card_dy:= 0;

		{ should return the Y offset from the anchor point of the nth card in the pile }

		{ default Dy }

		AssertAreEqual(0, aPile.GetCardY(1));
		AssertAreEqual(0, aPile.GetCardY(3));

		{ + Dy }

		aPile.my_card_dy:= 5;
		AssertAreEqual(0, aPile.GetCardY(1));
		AssertAreEqual(10, aPile.GetCardY(3));
	end;

procedure TestIsCardExposed;
var
	aPile:GenPileOfCardsTester;
begin
	aPile.Construct;

	aPile.CardIsPlaceHolderResult:= false;
	aPile.CardIsCoveredResult:= false;
	punit.Assert.IsTrue(aPile.IsCardExposed(1));

	aPile.CardIsPlaceHolderResult:= false;
	aPile.CardIsCoveredResult:= true;
	punit.Assert.IsFalse(aPile.IsCardExposed(1));

	aPile.CardIsPlaceHolderResult:= true;
	aPile.CardIsCoveredResult:= false;
	punit.Assert.IsFalse(aPile.IsCardExposed(1));

	aPile.CardIsPlaceHolderResult:= true;
	aPile.CardIsCoveredResult:= true;
	punit.Assert.IsFalse(aPile.IsCardExposed(1));
end;

procedure test_DeckProp_constructor;
var
	deck:ODeckprop;
begin
	deck.Construct(1);
	AssertAreEqual(52,deck.CardCount);
	deck.Construct(2);
	AssertAreEqual(104,deck.CardCount);
end;

procedure Test_TheCardGraphicsManager_GetBackBitmap;
begin
	punit.Assert.AreEqual(55,GetBitmapWd(the_instance.GetBackBitmap));
end;

procedure Test_TheCardGraphicsManager_GetMaskBitmap;
begin
	punit.Assert.AreEqual(55,GetBitmapWd(the_instance.GetMaskBitmap));
end;

procedure Test_TheCardGraphicsManager_GetFaceBitmap;
var
	any_card:cards.card;
begin
	any_card:=cards.CreateCard(QUEEN,CLUB);
	punit.Assert.AreEqual(55,GetBitmapWd(the_instance.GetFaceBitmap(any_card)));
end;

const
	the_supported_sizes_data:array[1..2] of word=(10,20);

type
	MyFakeCardFactory=object(CardFactoryStub)
		SupportedSizeCount_result:quantity;
		SelectWidth_arg1:word;
		function SupportedSizeCount:quantity; virtual;
		function SupportedWidthAt(aIndex:number):word; virtual;
	end;

function MyFakeCardFactory.SupportedWidthAt(aIndex:number):word;
begin
	SupportedWidthAt:=the_supported_sizes_data[aIndex];
end;

function MyFakeCardFactory.SupportedSizeCount:quantity;
begin
	SupportedSizeCount:=SupportedSizeCount_result;
end;

type
	TestCardGraphicsManager=object(TheCardGraphicsManager)
		SelectWidth_arg1:word;
		constructor Construct(aCardFactory:ICardFactory);
		procedure SelectWidth(in_pixels:word); virtual;
	end;

constructor TestCardGraphicsManager.Construct(aCardFactory:ICardFactory);
begin
	myCardFactory:=aCardFactory;
end;
	procedure TestCardGraphicsManager.SelectWidth(in_pixels:word);
begin
end;

procedure test_one_size_fits_all;
var
	manager:TestCardGraphicsManager;
	factory:MyFakeCardFactory;
begin
	factory.Construct;
	manager.Construct(@factory);
	factory.SupportedSizeCount_result:=1;
	punit.Assert.AreEqual(10,manager.LargestThatFits(1,1,1234,0,TRUE));
	punit.Assert.AreEqual(10,manager.LargestThatFits(1000,1,1234,1,FALSE));
end;

procedure test_more_than_one_size;
var
	manager:TestCardGraphicsManager;
	factory:MyFakeCardFactory;
begin
	factory.Construct;
	manager.Construct(@factory);
	factory.SupportedSizeCount_result:=2;
	punit.Assert.AreEqual(10,manager.LargestThatFits(1,1,1,0,TRUE));
	// table width
	punit.Assert.AreEqual(10,manager.LargestThatFits(1,1,19,0,TRUE));
	punit.Assert.AreEqual(20,manager.LargestThatFits(1,1,20,0,TRUE));
	// multiple columns
	punit.Assert.AreEqual(20,manager.LargestThatFits(2,0,40,0,TRUE));
	punit.Assert.AreEqual(10,manager.LargestThatFits(2,0,39,0,TRUE));
	// pile spacing
	punit.Assert.AreEqual(10,manager.LargestThatFits(2,2,20+1+20,0,TRUE));
	punit.Assert.AreEqual(20,manager.LargestThatFits(2,2,20+2+20,0,TRUE));
	// edge margin
	punit.Assert.AreEqual(10,manager.LargestThatFits(1,0,22,2,TRUE));
end;

type
	TestManager2=object(TestCardGraphicsManager)
		myBestColumnFitResult,myBestRowFitResult:word;
		function BestColumnFit(nPiles:word;space_between:integer;total_width:word;edge_margin:integer):word; virtual;
		function BestRowFit(nPiles:word;space_between:integer;total_height:word;edge_margin:integer):word; virtual;
	end;

function TestManager2.BestColumnFit(nPiles:word;space_between:integer;total_width:word;edge_margin:integer):word; begin BestColumnFit:=myBestColumnFitResult; end;
function TestManager2.BestRowFit(nPiles:word;space_between:integer;total_height:word;edge_margin:integer):word; begin BestRowFit:=myBestRowFitResult; end;

procedure test_BestFit_lesser_of_w_h;
var
	manager:TestManager2;
	factory:MyFakeCardFactory;
begin
	factory.Construct;
	manager.Construct(@factory);
	manager.myBestColumnFitResult:=18;
	manager.myBestRowFitResult:=19;
	punit.Assert.AreEqual(18,manager.BestFit(1,1,1,1,1,0));
	manager.myBestColumnFitResult:=17;
	punit.Assert.AreEqual(17,manager.BestFit(1,1,1,1,1,0));
end;

procedure GameTotalWidth;
var
	game:testable_OGame;
begin
	game.Construct;
	CardImageWd:=40;
	AssertAreEqual(240, game.TotalWidth);
end;

procedure GameTotalHeight;
var
	game:testable_OGame;
begin
	game.Construct;
	CardImageHt:=20;
	AssertAreEqual(80, game.TotalHeight);
end;

procedure CardPropIsNotDraggableIfFacedown;
var
	prop:OCardProp;
	function AnyFacedownCard:card; begin AnyFacedownCard:=MakeCard(TACE,TSPADES); end;
begin
	prop.Construct(AnyFacedownCard);
	AssertIsFalse(prop.CanGrabCardAt(1));
end;

procedure CardPropIsDraggableIfFaceup;
var
	prop:OCardProp;
	function AnyFaceupCard:TCard; begin AnyFaceupCard:=CardFaceup(MakeCard(TACE,TSPADES)); end;
begin
	prop.Construct(AnyFaceupCard);
	AssertIsTrue(prop.CanGrabCardAt(1));
end;

procedure CardPropConstructor;
var
	prop:OCardProp;
begin
	prop.Construct(MakeCard(TACE,TSPADES));
	AssertAreEqual(MakeCard(TACE,TSPADES),prop.GetCard);
	AssertIsTrue(prop.IsEnabled);
end;

procedure DealTo_XY;
var
	view:testable_OTabletop;
	deck:testable_ODeckProp;
begin
	view.Construct;
	deck.Construct(1);
	view.AddProp(@deck);
	AssertIsNotNil(deck.DealTo(Integer(10),Integer(20)));
end;

procedure DealTo_ReturnsCardProp;
var
	view:testable_OTabletop;
	deck:testable_ODeckProp;
begin
	view.Construct;
	deck.Construct(1);
	view.AddProp(@deck);
	AssertIsNotNil(deck.DealTo(MakeXYPair(10,20)));
end;

procedure AddWart_inserts_wart_prop_in_Z_order;
var
	view:MyTabletop;
	prop:Hotspot;
	wart:OPropwart;
begin
	view.Construct;
	prop.Construct(50, 75);
	wart.Construct(@prop);
	view.AddProp(@prop, MakeXYPair(100, 200));
	prop.AddWart(@wart);
	AssertAreEqual(2, view.HotList.Count);
end;

procedure default_wart_GetContent_returns_empty_string;
var
	prop:Hotspot;
	wart:OPropwart;
begin
	prop.Construct(50, 75);
	wart.Construct(@prop);
	AssertAreEqual('', wart.GetContent);
end;

procedure wart_anchor_point_defaults_to_CENTER_CENTER;
var
	view:MyTabletop;
	prop:Hotspot;
	wart:OPropwart;
begin
	view.Construct;
	prop.Construct(50, 80);
	wart.Construct(@prop);
	view.AddProp(@prop, MakeXYPair(100, 200));
	prop.AddWart(@wart);
	AssertAreEqual(126-WART_WIDTH+(WART_WIDTH div 2), wart.Left);
	AssertAreEqual(241-WART_HEIGHT+(WART_HEIGHT div 2), wart.Top);
end;

procedure wart_anchor_point_default_BOTTOM_LEFT;
var
	view:MyTabletop;
	prop:Hotspot;
	wart:OPropwart;
begin
	view.Construct;
	prop.Construct(50, 80);
	wart.Construct(@prop);
	view.AddProp(@prop, MakeXYPair(100, 200));
	prop.AddWart(@wart, BOTTOM_LEFT);
	AssertAreEqual(100-WART_WIDTH+WART_OVERLAP, wart.Left);
	AssertAreEqual(280-WART_OVERLAP, wart.Top);
end;

procedure PropInitialization;
var
	prop:Hotspot;
	function AllWartsAreNil:boolean;
	var
		x:relativeposition;
	begin
		AllWartsAreNil:=FALSE;
		for x:=Low(relativeposition) to High(relativeposition) do if prop.GetWartAt(x)<>NIL then exit;
		AllWartsAreNil:=TRUE;
	end;
begin
	prop.Construct(50, 75, 0);
	AssertIsFalse(prop.IsVisible);
	AssertIsTrue(AllWartsAreNil);
end;

procedure default_GetWidth_returns_current_width;
var
	prop:Hotspot;
begin
	prop.Construct(50, 75, 0);
	AssertAreEqual(50, prop.GetWidth);
end;

procedure default_GetHeight_returns_current_height;
var
	prop:Hotspot;
begin
	prop.Construct(50, 75, 0);
	AssertAreEqual(75, prop.GetHeight);
end;

type
	myprop=object(HotSpot)
		function GetHeight:word; virtual;
		function GetWidth:word; virtual;
	end;

function myprop.GetWidth:word;
begin
	GetWidth:=inherited GetWidth*2;
end;

function myprop.GetHeight:word;
begin
	GetHeight:=inherited GetHeight*2;
end;
	
procedure prop_GetWidth_is_overridable;
var
	prop:myprop;
	base_prop:^Hotspot;
begin
	prop.Construct(50, 75, 0);
	base_prop:=@prop;
	AssertAreEqual(100, base_prop^.GetWidth);
end;

procedure prop_GetHeight_is_overridable;
var
	prop:myprop;
	base_prop:^Hotspot;
begin
	prop.Construct(50, 75, 0);
	base_prop:=@prop;
	AssertAreEqual(150, base_prop^.GetHeight);
end;

procedure CreateTabletopBitmap;
var
	tabletop:testable_OTabletop;
	hbm:HBITMAP;
begin
	tabletop.Construct;
	hbm:=tabletop.CreateTabletopBitmap(100, 200);
	AssertAreEqual(100, GetBitmapWd(hbm));
	AssertAreEqual(200, GetBitmapHt(hbm));
end;

procedure propwart_construction;
var
	host:Hotspot;
	wart:OPropwart;
begin
	host.Construct(50, 75);
	wart.Construct(@host);
	AssertIsTrue(wart.IsOn);
	wart.Construct(@host, FALSE);
	AssertIsFalse(wart.IsOn);
end;

procedure propwart_off_trumps_host_visibility;
var
	host:Hotspot;
	wart:OPropwart;
begin
	host.Construct(50, 75);
	wart.Construct(@host, FALSE);
	host.AddWart(@wart);
	host.OnShown;
	AssertIsFalse(wart.IsVisible);
end;

procedure PropWartAccess;
var
	view:MyTabletop;
	prop:Hotspot;
	wart:OPropwart;
begin
	view.Construct;
	view.myWidth:=800;
	view.myHeight:=600;
	prop.Construct(50, 75);
	wart.Construct(@prop);
	prop.Anchor.x:=0;
	prop.Anchor.y:=0;
	view.AddProp(@prop);
	prop.AddWart(@wart);
	AssertAreEqual(LongInt(@wart), LongInt(prop.GetWartAt(CENTER_CENTER)));
end;

type
	mock_OCardpileProp=object(OCardpileProp)
	end;
	
procedure cardcount_wart_displays_cards_remaining;
var
	pile:mock_OCardpileProp;
	wart:OCardCountWart;
begin
	pile.Construct(50);
	pile.AddCard(MakeCard(TACE, TSPADES));
	pile.AddCard(MakeCard(TACE, TSPADES));
	pile.AddCard(MakeCard(TACE, TSPADES));
	wart.Construct(@pile);
	pile.AddWart(@wart);
	AssertAreEqual('3', wart.GetContent);
end;

procedure cardcount_wart_is_off_by_default;
var
	pile:mock_OCardpileProp;
	wart:OCardCountWart;
begin
	pile.Construct(50);
	wart.Construct(@pile);
	AssertIsFalse(wart.IsOn);
end;

procedure cardcount_wart_empty_when_no_cards;
var
	pile:mock_OCardpileProp;
	wart:OCardCountWart;
begin
	pile.Construct(50);
	wart.Construct(@pile);
	pile.AddWart(@wart);
	AssertAreEqual('', wart.GetContent);
end;

procedure squaredpiles_have_hidden_counter_wart_by_default;
var
	pile:testable_OSquaredpileprop;
begin
	pile.Construct(52);
	AssertIsFalse(pile.GetWartAt(CENTER_CENTER)^.IsVisible);
end;

procedure squaredpile_card_count_ON_OFF;
var
	pile:testable_OSquaredpileprop;
begin
	pile.Construct(52);
	pile.CardCountOn;
	AssertIsTrue(pile.GetWartAt(CENTER_CENTER)^.IsOn);
	pile.CardCountOff;
	AssertIsFalse(pile.GetWartAt(CENTER_CENTER)^.IsOn);
end;

procedure cardcount_wart_ON_OFF_is_recursive;
var
	pile:mock_OCardpileProp;
	wart:OCardCountWart;
begin
	pile.Construct(50);
	wart.Construct(@pile);
	wart.Off;
	wart.Off;
	wart.On;
	wart.On;
	AssertIsFalse(wart.IsOn);
	wart.On;
	AssertIsTrue(wart.IsOn);
end;

procedure fanned_pile_constructors;
var
	pile:OFannedPileProp;
begin
	pile.Construct;
	AssertAreEqual(52, pile.Capacity);
end;

procedure fanned_pile_width;
var
	pile:OFannedPileProp;
begin
	pile.Construct;
	AssertAreEqual(522, pile.GetWidth);
end;

procedure squaredpile_grab_topcard_hides_card_counter;
var
	pile:testable_OSquaredpileprop;
begin
	pile.Construct(52);
	pile.CardCountOn;
	pile.OnDragging;
	AssertIsFalse(pile.GetWartAt(CENTER_CENTER)^.IsOn);
end;

procedure squaredpile_drop_grab_topcard_shows_card_counter;
var
	pile:testable_OSquaredpileprop;
begin
	pile.Construct(52);
	pile.OnDropped;
	AssertIsTrue(pile.GetWartAt(CENTER_CENTER)^.IsOn);
end;

procedure OSquaredpileprop_can_grab_topcard_if_faceup;
var
	pile:testable_OSquaredpileprop;
begin
	pile.Construct(52);
	pile.AddCard(MakeCard(TACE, TSPADES));
	AssertIsFalse(pile.CanGrabCardAt(1));
	pile.AddCard(CardFaceup(MakeCard(TTHREE, TSPADES)));
	AssertIsTrue(pile.CanGrabCardAt(2));
end;

procedure OCardpileProp_default_capacity(p:pointer);
var
	pile:OCardpileProp_ptr;
begin
	pile:=OCardpileProp_ptr(p);
	AssertAreEqual(52, pile^.Capacity);
end;

begin //writeln('winqcktbl_tests');
	Suite.Add(@AddWart_inserts_wart_prop_in_Z_order);
	Suite.Add(@cardcount_wart_displays_cards_remaining);
	Suite.Add(@cardcount_wart_empty_when_no_cards);
	Suite.Add(@cardcount_wart_is_off_by_default);
	Suite.Add(@cardcount_wart_ON_OFF_is_recursive);
	Suite.Add(@CardPropConstructor);
	Suite.Add(@CardPropIsDraggableIfFaceup);
	Suite.Add(@CardPropIsNotDraggableIfFacedown);
	Suite.Add(@CreateTabletopBitmap);
//	Suite.Add(@DealTo_ReturnsCardProp);
//	Suite.Add(@DealTo_XY);
	Suite.Add(@default_GetWidth_returns_current_width);
	Suite.Add(@default_GetHeight_returns_current_height);
	Suite.Add(@default_wart_GetContent_returns_empty_string);
	Suite.Add(@fanned_pile_constructors);
//	Suite.Add(@fanned_pile_width);
	Suite.Add(@GameTotalWidth);
	Suite.Add(@GameTotalHeight);
	Suite.Add(@OnLButtonDblClickReturnCodes);
	Suite.Add(@OnLButtonUp_OnReleasedTarget_fires_when_lit);
	Suite.Add(@OnLButtonUp_OnReleasedTarget_trumps_all_when_lit);
	Suite.Add(@OSquaredpileprop_can_grab_topcard_if_faceup);
	Suite.Add(@prop_HitTest);
	Suite.Add(@prop_GetHeight_is_overridable);
	Suite.Add(@prop_GetWidth_is_overridable);
	Suite.Add(@PropInitialization);
	Suite.Add(@PropWartAccess);
	Suite.Add(@propwart_construction);
	Suite.Add(@propwart_off_trumps_host_visibility);
//	Suite.Add(@squaredpiles_have_hidden_counter_wart_by_default);
//	Suite.Add(@squaredpile_card_count_ON_OFF);
//	Suite.Add(@squaredpile_grab_topcard_hides_card_counter);
//	Suite.Add(@squaredpile_drop_grab_topcard_shows_card_counter);
	Suite.Add(@test_GenPileOfCards_CanSelectCard);
	Suite.Add(@test_GenPileOfCards_OnButtonRelease);
	Suite.Add(@test_GenPileOfCards_GetCardRectAt);
	Suite.Add(@test_GenPileOfCards_PointHitsCard);
	Suite.Add(@test_TTableView_OnLButtonUp);
	Suite.Add(@test_Hotspot_SetRelativeOffset);
	Suite.Add(@TestTTableViewBottom);
	Suite.Add(@TestTTableViewTop);
	Suite.Add(@OnSizeInvokesPropResizing);
	Suite.Add(@TestGenPileOfCardsObjectClass);
	Suite.Add(@TestGenPileOfCardsIsDblClkTarget);
	Suite.Add(@TestGenPileOfCardsGetCardX);
	Suite.Add(@TestGenPileOfCardsGetCardY);
	Suite.Add(@test_TryTopcardToDblClkTargets);
	Suite.Add(@TestGenPileOfCardsTopSelected);
	Suite.Add(@TestHotspotGetSpanRect);
	Suite.Add(@TestHotspotObjectClass);
	Suite.Add(@TestHotspotRefreshRect);
	Suite.Add(@TestHotspotSetStickyPos);
	Suite.Add(@test_one_size_fits_all);
	Suite.Add(@test_more_than_one_size);
	Suite.Add(@test_BestFit_lesser_of_w_h);
	Suite.Add(@Test_UpdateCardSize);
	Suite.Add(@test_CardCenterToAnchor);
	Suite.Add(@test_CardAnchorToCenter);
	Suite.Add(@TestSetDesc);
	Suite.Add(@TestAppendDesc);
	Suite.Add(@TestIsDerivedFromGenPileOfCards);
	Suite.Add(@TestFindPlaceHolder);
	Suite.Add(@TestIsCardExposed);
	Suite.Add(@TestCardIsCoveredSquaredPile);
	Suite.Add(@TestCardIsCoveredSkewedPile);
	Suite.Add(@TestCardIsCoveredWithPlaceHoldersAbove);
	Suite.Add(@Test_OnLButtonDown_return_codes);
	Suite.Add(@test_GenPileOfCards_OnPressed);
	Suite.Add(@test_ConvertPauseToMillSeconds);
	Suite.Add(@test_DeckProp_constructor);
	Suite.Add(@Test_TheCardGraphicsManager_GetBackBitmap);
	Suite.Add(@Test_TheCardGraphicsManager_GetMaskBitmap);
	Suite.Add(@Test_TheCardGraphicsManager_GetFaceBitmap);
	Suite.Add(@OnDrawItemReturnValue);
	Suite.Add(@wart_anchor_point_default_BOTTOM_LEFT);
	Suite.Add(@wart_anchor_point_defaults_to_CENTER_CENTER);
	Suite.Run('winqcktbl_tests');
	RunTest(@OCardpileProp_default_capacity, New(OCardpileProp_ptr, Construct));
	RunTest(@OCardpileProp_default_capacity, New(OSquaredpileprop_ptr, Construct));
end.
