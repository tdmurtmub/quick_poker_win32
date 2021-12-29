{ (C) 2005-2006 Wesley Steiner }

{$MODE FPC}

unit app;

interface

type
	IPersistor=^Persistor;
	Persistor=object
		Constructor Construct;
		procedure LoadNum(aKey,aSubKey:pchar;var aNum:integer;aDefault,aMinVal,aMaxVal:integer); virtual; abstract;
		procedure SaveNum(aKey,aSubKey:pchar;aNum:integer); virtual; abstract;
		procedure SaveText(aKey,aSubKey,aText:pchar); virtual; abstract;
	end;

implementation

constructor Persistor.Construct; begin end;

end.
