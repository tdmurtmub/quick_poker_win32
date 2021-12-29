{ (C) 2005-2006 Wesley Steiner }

unit persist;

interface

uses
	app;

type
	WinPersistorPtr=^WinPersistor;
	WinPersistor=object(Persistor)
		constructor Construct(const aName:pchar);
		destructor Destruct;
		procedure LoadNum(aKey,aSubKey:pchar;var aNum:integer;aDefault,aMinVal,aMaxVal:integer); virtual;
		procedure SaveNum(aKey,aSubKey:pchar;aNum:integer); virtual;
	private
		myName:pchar;
	end;

implementation

uses
	strings,
	sdkex;

constructor WinPersistor.Construct;

begin
	inherited Construct;
	myName:=StrNew(aName);
end;

destructor WinPersistor.Destruct;

begin
	StrDispose(myName);
end;

procedure WinPersistor.SaveNum(const aKey,aSubKey:pchar; aNum:integer);

begin
	WriteINIInt(myName,aKey,aSubKey,aNum);
end;

procedure WinPersistor.LoadNum(const aKey,aSubKey:pchar; var aNum:integer; aDefault, aMinVal, aMaxVal:integer);

begin
	ReadINIInt(myName,aKey,aSubKey,aDefault,aMinVal,aMaxVal,aNum);
end;

end.

