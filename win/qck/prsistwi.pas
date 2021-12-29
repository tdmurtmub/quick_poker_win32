{ (C) 2005-2007 Wesley Steiner }

unit prsistwi;

{$I Platform}

interface

uses
	std,
	windows,
	oapp,owindows;

type
	PPersistentWindow=^TPersistentWindow;
	TPersistentWindow=object(TWindowEx)
		myRestoredX,myRestoredY:LONG;
		constructor Init(pParentWindow:PWindow;const pStgKey:pchar);
		function OnSize(resizeType:uint;newWidth,newHeight:integer):LONG; virtual;
		function OnMove(newX,newY:integer):LONG; virtual;
	private
		myPersistor:PopupWndPersistorPtr;
	end;

implementation

uses
	{$ifdef TEST} punit, {$endif}
	strings,
	systemx,
	sdkex;

constructor TPersistentWindow.Init(pParentWindow:PWindow;const pStgKey:pchar);

begin
	inherited Construct;
	myPersistor:=new(PopupWndPersistorPtr,Construct(@self,pStgKey,TRUE,FALSE,LONG(CW_USEDEFAULT),LONG(CW_USEDEFAULT)));
	myPersistor^.RestorePos(myRestoredX,myRestoredY);
end;

function TPersistentWindow.OnSize(resizeType:uint;newWidth,newHeight:integer):LONG;

begin
	case resizeType of
		SIZE_RESTORED:begin
			myPersistor^.SaveSize;
			myPersistor^.SaveMaximized(FALSE);
		end;
		SIZE_MAXIMIZED:myPersistor^.SaveMaximized(TRUE);
	end;
	OnSize:=0;
end;

function TPersistentWindow.OnMove(newX,newY:integer):LONG;

begin
	if (not Maximized) and (not Minimized) then myPersistor^.SavePos;
	OnMove:=0;
end;

{$ifdef TEST}
begin
	Suite.Run('prsistwi');
{$endif TEST}
end.
