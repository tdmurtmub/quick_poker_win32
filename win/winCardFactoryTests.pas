{ (C) 2011 Wesley Steiner }

{$MODE FPC}

unit winCardFactoryTests;

{$I platform}

interface

implementation

uses 
	punit,
	windows,
	windowsx,
	gdiex,
	cards,
	winCardFactory;

procedure CardHeightIsGreaterThanCardWidth; 
begin
	Assert.IsTrue(CalcHtFromWd(RegularFormat,100)>100);
end;

procedure Test_Convert_WidthIndex_to_CardSize;
begin
	punit.Assert.AreEqual(Ord(SMALLCARDS),Ord(ConvertIndexToSize(3)));
	punit.Assert.AreEqual(Ord(MEDIUMCARDS),Ord(ConvertIndexToSize(7)));
	punit.Assert.AreEqual(Ord(LARGECARDS),Ord(ConvertIndexToSize(12)));
end;

procedure Test_SupportedSizeCount; 
begin
	Assert.IsTrue(winCardFactory.SupportedSizeCount>0);
end;

procedure Test_ConvertWidthToIndex;
begin
	// smaller than any
	punit.Assert.AreEqual(1,ConvertWidthToIndex(CardWidthAt(1)-1));
	punit.Assert.AreEqual(1,ConvertWidthToIndex(CardWidthAt(1)));
	// bigger than any
	punit.Assert.AreEqual(3,ConvertWidthToIndex(CardWidthAt(3)+1));
	punit.Assert.AreEqual(3,ConvertWidthToIndex(CardWidthAt(3)));
	// in between
	punit.Assert.AreEqual(2,ConvertWidthToIndex(CardWidthAt(2)+1));
end;

procedure Test_CreateBackBitmapAt;
begin
	punit.Assert.IsTrue(CreateBackBitmapAt(1)<>NULL_HANDLE);
	punit.Assert.AreEqual(55,GetBitmapWd(CreateBackBitmapAt(1)));
	punit.Assert.AreEqual(213,GetBitmapWd(CreateBackBitmapAt(SupportedSizeCount)));
	punit.Assert.AreEqual(213,GetBitmapWd(CreateBackBitmapAt(999)));
end;

procedure Test_CreateFaceBitmapAt;
var
	any_card:cards.card;
begin
	any_card:=cards.CreateCard(QUEEN,CLUB);
	punit.Assert.IsTrue(CreateFaceBitmapAt(1,any_card)<>NULL_HANDLE);
	punit.Assert.AreEqual(55,GetBitmapWd(CreateFaceBitmapAt(1,any_card)));
	punit.Assert.AreEqual(213,GetBitmapWd(CreateFaceBitmapAt(SupportedSizeCount,any_card)));
	punit.Assert.AreEqual(213,GetBitmapWd(CreateFaceBitmapAt(999,any_card)));
end;

procedure Test_CreateMaskBitmapAt;
begin
	punit.Assert.IsTrue(CreateMaskBitmapAt(1)<>NULL_HANDLE);
	punit.Assert.AreEqual(55,GetBitmapWd(CreateMaskBitmapAt(1)));
	punit.Assert.AreEqual(213,GetBitmapWd(CreateMaskBitmapAt(SupportedSizeCount)));
	punit.Assert.AreEqual(213,GetBitmapWd(CreateMaskBitmapAt(999)));
end;

begin
	Suite.Add(@CardHeightIsGreaterThanCardWidth);
	Suite.Add(@Test_SupportedSizeCount);
	Suite.Add(@Test_Convert_WidthIndex_to_CardSize);
	Suite.Add(@Test_ConvertWidthToIndex);
	Suite.Add(@Test_CreateBackBitmapAt);
	Suite.Add(@Test_CreateFaceBitmapAt);
	Suite.Add(@Test_CreateMaskBitmapAt);
	Suite.Run('winCardFactoryTests');
end.
