{ (C) 2005-2007 Wesley Steiner }

unit ipersist;

interface

type
	IPersistor=^Persistor;
	Persistor=object
		constructor Construct;
		procedure LoadNum(const pKey,pSubKey:pchar;var rNum:integer;aDflt,aLow,aHigh:integer); virtual; abstract;
		procedure SaveNum(const pKey,pSubKey:pchar;aNum:integer); virtual; abstract;
		procedure SaveText(const pKey,pSubKey,pText:pchar); virtual; abstract;
	end;

implementation

constructor Persistor.Construct; begin end;

end.
