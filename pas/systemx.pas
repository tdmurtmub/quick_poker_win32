{ Copyright (C) 2005 by Wesley Steiner. All rights reserved. }

{$MODE FPC}

unit SystemX;

interface

function BitTest(a_aBitFlags, a_aBit:word):boolean;

implementation

{$ifdef TEST} uses punit; {$endif}

function BitTest(a_aBitFlags, a_aBit:word):boolean;

	begin
		BitTest:= (a_aBitFlags and a_aBit) <> 0;
	end;

{$ifdef TEST}

procedure Test_BitTest; 

	begin
		Assert.IsTrue(BitTest($FFFF, $0080));
		Assert.IsFalse(BitTest($0000, $0080));
	end;

{$endif TEST}

{$ifdef TEST}
begin
	Suite.Add(@Test_BitTest);
	Suite.Run('SystemX');
{$endif TEST}
end.
