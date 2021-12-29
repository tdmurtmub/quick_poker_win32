(**********************************************************************
Personal Library {BP}
Copyright (C) 2000 by Wesley Steiner. All rights reserved.
***********************************************************************)

unit WES;

interface

{ Returns the number of days (starting at 1) since 01/01/2000. }
function DaysSinceJan2000(month:integer; day:integer; year:integer):integer;

implementation

uses
	WINDOS;

function DaysSinceJan2000;

	var
		dt:TDateTime;
		aTime:LongInt;

	begin
		{ populate a TDateTime structure for conversion to packed time }
		dt.Year:= year;
		dt.Month:= month;
		dt.Day:= day;
		dt.Hour:= 0;
		dt.Min:= 0;
		dt.Sec:= 0;

		{ pack it into a longint }
		PackTime(dt, aTime);

		{ return # of days since 01/01/2000 }
		DaysSinceJan2000:= ((aTime and $FFFF0000) shr 16) - 10289;
	end;

end.
