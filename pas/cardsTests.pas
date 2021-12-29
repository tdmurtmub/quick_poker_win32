(* (C) 2011 Wesley Steiner *)

{$MODE FPC}

{$I platform}

unit cardsTests;

interface

implementation

uses
	punit,
	cards;

const
	ANY_PIP:pip=THREE;
	ANY_SUIT:suit=SPADE;

procedure test_CreateCard;
begin
	AssertAreEqual($11,CreateCard(ACE,CLUB));
	AssertAreEqual($4D,CreateCard(KING,SPADE));
end;

procedure test_GetCardPip;
begin
	AssertAreEqual(Ord(ACE),Ord(GetCardPip(CreateCard(ACE,ANY_SUIT))));
	AssertAreEqual(Ord(KING),Ord(GetCardPip(CreateCard(KING,ANY_SUIT))));
end;

procedure test_GetCardSuit;
begin
	AssertAreEqual(Ord(SPADE),Ord(GetCardSuit(CreateCard(ANY_PIP,SPADE))));
	AssertAreEqual(Ord(CLUB),Ord(GetCardSuit(CreateCard(ANY_PIP,CLUB))));
end;

procedure test_TCardToCard;
begin
	AssertAreEqual(CreateCard(KING,SPADE),TCardToCard(MakeCard(TKING,TSPADE)));
	AssertAreEqual(CreateCard(ACE,CLUB),TCardToCard(MakeCard(TACE,TCLUBS)));
	AssertAreEqual(NOCARD,TCardToCard(NULL_CARD));
end;

procedure test_TPipToPip;
begin
	Assert.IsTrue(TPipToPip(TACE)=cards.ACE);
	Assert.IsTrue(TPipToPip(TKING)=cards.KING);
end;

procedure 	test_TSuitToSuit;
begin
	AssertAreEqual(Ord(CLUB),Ord(TSuitToSuit(TCLUB)));
	AssertAreEqual(Ord(SPADE),Ord(TSuitToSuit(TSPADE)));
end;

procedure Test_Pip; 
begin
	Assert.Equal(TACE, CardPip(MakeCard(TACE, TCLUB)));
	Assert.Equal(TACE, CardPip(MakeCard(TACE, TSPADE)));
	Assert.Equal(TKING, CardPip(MakeCard(TKING, TSPADE)));
end;

procedure Test_Suit; 
begin
	Assert.Equal(TCLUBS,CardSuit(MakeCard(TJACK,TCLUB)));
	Assert.Equal(TSPADE,CardSuit(MakeCard(TJACK,TSPADE)));
end;

procedure Test_CardsGet; 
var
	aPile:PileOfCards;
begin
	aPile.Construct(10);
	aPile.Add(MakeCard(TACE, TSPADES));
	Assert.Equal(MakeCard(TACE, TSPADES), CardsGet(aPile, 1));
	aPile.Add(MakeCard(TKING, TDIAMONDS));
	Assert.Equal(MakeCard(TKING, TDIAMONDS), CardsGet(aPile, 2));
end;

procedure TestIsEmpty; 
var
	aPile:PileOfCards;
begin
	aPile.Construct(5);
	Assert.IsTrue(aPile.IsEmpty);
	aPile.Add(NULL_CARD);
	Assert.IsTrue(aPile.IsEmpty);
	aPile.Add(MakeCard(TJACK,TSPADES));
	Assert.IsFalse(aPile.IsEmpty);
end;

procedure test_CardIsPlaceHolder; 
begin
	Assert.IsTrue(CardIsPlaceHolder(NULL_CARD));
	Assert.IsFalse(CardIsPlaceHolder(MakeCard(TJACK,TSPADE)));
end;

procedure Test_CardIsFaceup;
var
	aCard:TCard;
begin
	aCard:=MakeCard(TACE,TSPADES);
	punit.Assert.IsFalse(CardIsFaceup(aCard));
	aCard:=aCard or FACEUP_BIT;
	punit.Assert.IsTrue(CardIsFaceup(aCard));
end;

procedure Test_CardFaceup;
var
	aCard:TCard;
begin
	aCard:=CardFaceup(MakeCard(TJACK,TSPADES));
	punit.Assert.IsTrue(CardIsFaceup(aCard));
	punit.Assert.AreEqual(TJACK,CardPip(aCard));
end;

procedure Test_PipToTPip;
begin
	Assert.AreEqual(TACE,PipToTPip(ACE));
	Assert.AreEqual(TKING,PipToTPip(KING));
end;

procedure Test_SuitToTSuit;
begin
	Assert.AreEqual(TCLUB,SuitToTSuit(CLUB));
	Assert.AreEqual(TSPADE,SuitToTSuit(SPADE));
end;

procedure test_CardToTCard;
begin
	Assert.AreEqual(MakeCard(TACE,TCLUB),CardToTCard(CreateCard(ACE,CLUB)));
	Assert.AreEqual(MakeCard(TKING,TSPADES),CardToTCard(CreateCard(KING,SPADE)));
	Assert.AreEqual(NULL_CARD,CardToTCard(NOCARD));
end;

begin
	Suite.Add(@Test_CardsGet);
	Suite.Add(@TestIsEmpty);
	Suite.Add(@test_CardIsPlaceHolder);
	Suite.Add(@Test_CardIsFaceup);
	Suite.Add(@Test_CardFaceup);
	Suite.Add(@test_CreateCard);
	Suite.Add(@test_GetCardPip);
	Suite.Add(@test_GetCardSuit);
	Suite.Add(@test_TPipToPip);
	Suite.Add(@test_TSuitToSuit);
	Suite.Add(@test_TCardToCard);
	Suite.Add(@Test_Pip);
	Suite.Add(@Test_Suit);
	Suite.Add(@Test_PipToTPip);
	Suite.Add(@Test_SuitToTSuit);
	Suite.Add(@Test_CardToTCard);
	Suite.Run('cardsTests');
end.
