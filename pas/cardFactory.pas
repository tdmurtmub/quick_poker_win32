{ (C) 2006 Wesley Steiner }

unit cardFactory;

{$I platform}

interface

uses
	std,drawing,cards;
	
type
	ICardFactory=^ICardFactory_;
	ICardFactory_=object
		constructor Construct;
		function CreateBackBitmapAt(at:number):drawing.bitmap; virtual; abstract;
		function CreateFaceBitmapAt(at:number;card:cards.card):drawing.bitmap; virtual; abstract;
		function CreateMaskBitmapAt(at:number):drawing.bitmap; virtual; abstract;
		function SupportedSizeCount:quantity; virtual; abstract;
		function SupportedWidthAt(at:number):word; virtual; abstract;
		function SupportedHeightAt(at:number):word; virtual; abstract;
	end;

	{$ifdef TEST}
	CardFactoryStub=object(ICardFactory_)
		constructor Construct;
		function CreateBackBitmapAt(at:number):drawing.bitmap; virtual;
		function CreateFaceBitmapAt(at:number;card:cards.card):drawing.bitmap; virtual;
		function CreateMaskBitmapAt(at:number):drawing.bitmap; virtual;
		function SupportedSizeCount:quantity; virtual;
		function SupportedWidthAt(at:number):word; virtual;
		function SupportedHeightAt(at:number):word; virtual;
	end;
	{$endif TEST}

implementation

constructor ICardFactory_.Construct; begin end;

{$ifdef TEST}

constructor CardFactoryStub.Construct; 
begin 
end;

function CardFactoryStub.SupportedHeightAt(at:number):word; 
begin 
	SupportedHeightAt:=0; 
end;

function CardFactoryStub.SupportedSizeCount:quantity; 
begin 
	SupportedSizeCount:=0; 
end;

function CardFactoryStub.SupportedWidthAt(at:number):word; 
begin 
	SupportedWidthAt:=0; 
end;

function CardFactoryStub.CreateMaskBitmapAt(at:number):drawing.bitmap; 
begin 
	CreateMaskBitmapAt:=0; 
end;

function CardFactoryStub.CreateBackBitmapAt(at:number):drawing.bitmap; 
begin 
	CreateBackBitmapAt:=0; 
end;

function CardFactoryStub.CreateFaceBitmapAt(at:number;card:cards.card):drawing.bitmap; 
begin 
	CreateFaceBitmapAt:=0; 
end;

{$endif TEST}

end.
