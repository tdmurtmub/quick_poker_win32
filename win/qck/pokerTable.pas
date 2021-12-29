{ (C) 2009 Wesley Steiner }

{$MODE FPC}

unit pokertable;

interface

uses
	std,poker,qcktbl;
	
const
	DEFAULT_TABLELIMIT=10;

const
	the_table_limit:number=DEFAULT_TABLELIMIT;
	the_table_stakes:number={$ifdef DEBUG} 1723 {$else} 250 {$endif};

implementation

end.
