{ (C) 2003-2006 Wesley Steiner }

unit Screen;

interface

type
	ScreenProperties=object
		function Width:word; { in pixels }
		function Height:word;
	end;

const
	Properties:ScreenProperties=();

implementation

uses
	windows;

function ScreenProperties.Width:word;

var
	rc:TRect;

begin
	GetClientRect(GetDesktopWindow,rc);
	Width:=rc.right-rc.left;
end;

function ScreenProperties.Height:word;

var
	rc:TRect;

begin
	GetClientRect(GetDesktopWindow,rc);
	Height:=rc.bottom-rc.top;
end;

end.
