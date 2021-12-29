{ (C) 2011 Wesley Steiner }

{$MODE FPC}

{$I platform}

unit os;

interface

function HasExt(const aFileSpec:String):boolean;

implementation

function HasExt(const aFileSpec:String):boolean;
var
	sz:array[0..255] of Char;
	apath,aName,aExt:string;
begin
	StrPCopy(sz,aFileSpec);
	{$ifdef FPC}
	FSplit(aFileSpec,aPath,aName,aExt);
	HasExt:=Length(aExt)>0;
	{$else}
	HasExt:=(FileSplit(sz,nil,nil,nil) and fcExtension)<>0;
	{$endif}
end;

end.

