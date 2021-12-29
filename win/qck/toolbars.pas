{ (C) 2005-2007 Wesley Steiner }

{$MODE FPC}

unit toolbars;

interface

uses
	windows,
	owindows;

const
	TOOL_BAR_ICON_SIZE=9; { pixels high }
	WUTTONMETRICEDGE=7;	{ pixels around Bitmap and Text to edge of buttons }
	BMPBUTSPACE=WUTTONMETRICEDGE;
	BAR_BUTTON_HT=BMPBUTSPACE+TOOL_BAR_ICON_SIZE+BMPBUTSPACE;
	BUTTON_BAR_HT=BAR_BUTTON_HT+5;
	BB_SHIFT_PIXELS=10;
	
	BB_SHIFT		=$0001; { shift this button visually farther than normal from the previous button }
	BB_SHIFT_SHORT	=$0002; { shift this button a few pixels away from the previous button }
	BB_SHOWTEXT		=$0008; { display the text with the bitmap }

type
	TButton=object
		handle:HWND;
		myCmdId:UINT;
		constructor Init(aCmdId:UINT);
		function Disable:boolean;
		procedure SetCaption(ATitle:PChar);
		procedure Show(ShowCmd:Integer); virtual;
	end;

	PImgButton=^TImgButton;
	TImgButton=object(TButton)
		m_pressed:boolean;
		constructor Init(a_cmd:integer);
	private
		hbmSticky:HBITMAP;
	end;

type
	TIEBitmapButton=object(TImgButton)
		procedure SetButtonText(const p_text:PChar);
		procedure SetButtonCommand(a_cmd:integer);
		procedure Show; virtual;
		procedure Hide; virtual;
		procedure Enable; virtual; // virtual is DEPRECATED: use OnEnabled
		procedure Disable; virtual; // virtual is DEPRECATED: use OnDisabled
		procedure Enabled(a_state:boolean); virtual;
		procedure OnEnabled; virtual;
		procedure OnDisabled; virtual;
		function isEnabled:boolean;
		function GetCommandID:integer;
	end;

type
	PBarTextBox=^TBarTextBox;
	TBarTextBox=object(OWindow)
		constructor Init(aParent:PWindow;const pText:pchar);
		destructor Done; virtual;
		function Create(aParent:PWindow;aX,aW:integer;const pText:pchar;bbFlags:Word):HWND;
		procedure Paint(aPaintDC:hDC;var PaintInfo:TPaintStruct); virtual;
		procedure Clear;
		procedure Update(const pText:pchar);
		procedure UpdateString(const aString:string);
	private
		myText:pchar;
		myAlignment:integer;
		hasBorder:boolean;
	end;

	PBarNumber = ^TBarNumber;
	TBarNumber = object(TBarTextBox)
		prefix:string;
		constructor Init(aParent:PWindow;aX,aW:integer;aInt:integer);
		procedure UpdateValue(aValue:integer);
	private
		myAmount:real;
		function AsText:string; virtual;
	end;

	TextBoxP=^TextBox;
	TextBoxPtr=^TextBox;
	TextBox=object(TBarTextBox)
		constructor Construct;
		function Create(aParent:PWindow):HWND;
	end;

	PBarDollar=^TBarDollar;
	TBarDollar=object(TBarNumber)
		constructor Init(aParent:PWindow;aX,aW:integer;amount:real);
		procedure UpdateDollar(Dollars:Real);
	end;

	PlaybarP=^Playbar;
	PToolBar=^TToolBar; { OBSOLETE: use PlaybarP }

	PBarButton=^TBarButton;
	TBarButton=object(TIEBitmapButton)
		constructor Construct(command_id:UINT);
		function Parent:PlaybarP;
		procedure Create(playbar:PToolBar;aText:pchar;bbFlags:Word);
		procedure Disable; virtual; // virtual is DEPRECATED: use OnDisabled
		procedure Enable; virtual; // virtual is DEPRECATED: use OnEnabled
	private
		my_playbar:PlaybarP;
		my_menu:HMENU;
		my_command_id:integer;
		procedure LinkMenuItem(popup_menu:HMENU;command_id:integer); { OBSOLETE: use Constructor }
		procedure SetMenuItem(popup_menu:HMENU;command_id:integer); { OBSOLETE: use LinkMenuItem }
	end;

	TToolBar=object(OWindow) { OBSOLETE: use Playbar }
		constructor Init;
		function Create(parent:HWND):HWND;
		function OnCmd(aCmdId:UINT):LONG; virtual;
	private
		my_parent:HWND;
		NextXPos:integer;
	end;
	
	Playbar=object(TToolBar)
	end;
	
(*	PStickyBarButton = ^TStickyBarButton;
	TStickyBarButton = object(TBarButton)
		constructor Init(parent:OWindowsPtr;IDB_On, IDB_Off, idb_Sticky:integer; cmd:integer; szDesc:pchar; bbFlags:Word);
		procedure depress; virtual; { depress a sticky button }
		procedure release; virtual; { release a sticky button }
	end;
*)
implementation

uses
	{$ifdef TEST} punit, {$endif}
	strings,
	stringsx,std,sdkex,gdiex,windowsx;

constructor TButton.Init(aCmdId:UINT);
begin
	handle:=0;
	myCmdId:=aCmdId;
end;

function TButton.Disable:boolean;
begin
	Disable:=EnableWindow(handle,FALSE);
end;

procedure TButton.SetCaption(ATitle:PChar);
begin
	SetWindowText(handle,ATitle);
end;

procedure TButton.Show(ShowCmd:Integer);
begin
	ShowWindow(handle,ShowCmd);
end;

constructor TImgButton.Init(a_cmd:integer);
begin
	inherited Init(a_cmd);
	m_pressed:=False;
end;

procedure TIEBitmapButton.Enable;
begin
	EnableWindow(handle,TRUE);
	OnEnabled;
end;

function TIEBitmapButton.isEnabled:boolean;
begin
	isEnabled:=IsWindowEnabled(handle);
end;

procedure TIEBitmapButton.Disable;
begin
	inherited disable;
	if (IsWindowVisible(handle)) then begin
		InvalidateRect(handle, nil, FALSE);
		UpdateWindow(handle);
	end;
	OnDisabled;
end;

procedure TIEBitmapButton.OnEnabled; 
begin
end;

procedure TIEBitmapButton.OnDisabled;
begin
end;

procedure TIEBitmapButton.Enabled(a_state:boolean);
begin
	if a_state 
		then Enable
		else Disable;
end;

procedure TIEBitmapButton.SetButtonText(const p_text:PChar);
begin
	SetCaption(p_text);
end;

procedure TIEBitmapButton.SetButtonCommand(a_cmd:integer);

begin
	myCmdId:=a_cmd;
end;

procedure TIEBitmapButton.Show;

begin
	inherited Show(SW_SHOW);
end;

procedure TIEBitmapButton.Hide;

begin
	{with attr do style:= style and (not WS_VISIBLE);}
	inherited Show(SW_HIDE);
end;

function TIEBitmapButton.GetCommandId:integer;
begin
	GetCommandId:=myCmdId;
end;

constructor TBarTextBox.Init(aParent:PWindow;const pText:pchar);
begin //writeln('TBarTextBox.Init(aParent:PWindow,"',pText,'")');
	inherited Construct;
	myText:=StrNew(pText);
	myAlignment:=TA_CENTER;
	hasBorder:=TRUE;
end;

destructor TBarTextBox.Done;
begin //writeln('TBarTextBox.Done');
	StrDispose(myText);
end;

procedure TBarTextBox.Paint(aPaintDC:hDC;var PaintInfo:TPaintStruct);
var
	TheFont, OldFont:HFONT;
	rc:TRect;
	OldPen, aPen:HPEN;
begin
	GetClientRect(rc);
	if hasBorder then begin
		OldPen:= SelectObject(aPaintDC, GetStockObject(WHITE_PEN));
		MoveToEx(aPaintDC, rc.right - 1, rc.top,nil);
		LineTo(aPaintDC, rc.right - 1, rc.bottom);
		MoveToEx(aPaintDC, rc.left, rc.bottom - 1,nil);
		LineTo(aPaintDC, rc.right - 1, rc.bottom - 1);
		SelectObject(aPaintDC, OldPen);
		aPen:= CreatePen(PS_SOLID, 1, DarkGray);
		OldPen:= SelectObject(aPaintDC, aPen);
		MoveToEx(aPaintDC, rc.left, rc.top,nil);
		LineTo(aPaintDC, rc.right - 1, rc.top);
		MoveToEx(aPaintDC, rc.left, rc.top,nil);
		LineTo(aPaintDC, rc.left, rc.bottom - 1);
		SelectObject(aPaintDC, OldPen);
		DeleteObject(aPen);
	end;
	TheFont:= CreateFont(
		GetClientHt(handle)-6, 0,
		0,
		0,
		FW_NORMAL,
		0, 0, 0,
		ANSI_CHARSET,
		OUT_DEFAULT_PRECIS,
		CLIP_DEFAULT_PRECIS,
		PROOF_QUALITY,
		VARIABLE_PITCH or FF_SWISS,
		'Arial'
		);
	OldFont:= SelectObject(aPaintDC, TheFont);
	SetTextAlign(aPaintDC,myAlignment or TA_TOP);
	SetBkMode(aPaintDC, TRANSPARENT);
	if (myText <> nil) then TextOut(aPaintDC,Q(myAlignment=TA_LEFT,0,GetClientWd(handle) div 2), Center(DevFontHt(aPaintDC),0,GetClientHt(handle)),myText,StrLen(myText));
	SelectObject(aPaintDC, OldFont);
	DeleteObject(TheFont);
end;

procedure TBarTextBox.Update(const pText:pchar);
begin //writeln('TBarTextBox.Update("',pText,'")');
	StrDispose(myText);
	myText:=StrNew(pText);
	InvalidateRect(handle,nil,TRUE);
	UpdateWindow;
end;

constructor TBarNumber.Init(aParent:PWindow;aX,aW:integer;aInt:integer);
begin
	inherited Init(aParent,'');
	prefix:='';
	myAmount:=aInt;
end;

constructor TBarDollar.Init(aParent:PWindow;aX,aW:integer; amount:real);
begin
	inherited Init(aParent,0,0,Integer(Round(amount)))
end;

function TBarNumber.AsText:string;
var
	s:string;
begin
	str(myAmount:0:0,s);
	AsText:=prefix+s;
end;

{$ifdef TEST}

type
	TBarNumberTester=object(TBarNumber)
		constructor Construct(initialAmount:real);
	end;

constructor TBarNumberTester.Construct(initialAmount:real);
begin
	prefix:='';
	myAmount:=initialAmount;
end;

procedure Test_BarNumber_AsText; 
var
	tester:TBarNumberTester;
begin
	tester.Construct(235.1);
	punit.Assert.EqualStr('235',tester.AsText);
	tester.myAmount:=235.9;
	punit.Assert.EqualStr('236',tester.AsText);
end;

procedure Test_BarNumber_AsText_with_prefix; 
var
	tester:TBarNumberTester;
begin
	tester.Construct(666.1);
	tester.prefix:='A Prefix string ';
	punit.Assert.EqualStr('A Prefix string 666',tester.AsText);
end;

{$endif}

procedure TBarDollar.UpdateDollar(Dollars:Real);
begin
	inherited UpdateValue(Integer(Round(dollars)));
end;

procedure TBarNumber.UpdateValue(aValue:integer);
var
	sb:stringBuffer;
begin
	myAmount:=aValue;
	StrPCopy(sb,AsText);
	Update(sb);
end;

function TBarButton.Parent:PlaybarP;
begin
	Parent:=my_playbar;
end;

constructor TBarButton.Construct(command_id:UINT);
begin
	inherited Init(command_id);
end;

procedure TBarButton.Enable;
begin
	inherited Enable;
	EnableMenuItem(GetMenu(GetParent(Parent^.Handle)),GetCommandID,MF_BYCOMMAND or MF_ENABLED);
end;

procedure TBarButton.Disable;
begin
	inherited Disable;
	EnableMenuItem(GetMenu(GetParent(Parent^.Handle)),GetCommandID,MF_BYCOMMAND or MF_GRAYED);
end;

procedure TBarButton.SetMenuItem(popup_menu:HMENU;command_id:integer);
begin
	my_menu:= popup_menu;
	my_command_id:= command_id;
	if IsWindowEnabled(handle) then
		EnableMenuItem(my_menu, my_command_id, MF_BYCOMMAND or MF_ENABLED)
	else
		EnableMenuItem(my_menu, my_command_id, MF_BYCOMMAND or MF_GRAYED);
end;
(*
constructor TStickyBarButton.Init(parent:PWindowsObject;
	IDB_On,IDB_Off,idb_Sticky:integer;
	cmd:integer;szDesc:pchar;bbFlags:Word);

begin
	inherited Init(parent, idb_on, idb_off, cmd, szDesc, bbFlags);
	SetStickyBitmap(LoadBitmap(hInstance, MakeIntResource(IDB_Sticky)));
end;
*)

procedure TBarButton.LinkMenuItem(popup_menu:HMENU;command_id:integer);
begin
	SetMenuItem(popup_menu,command_id);
end;

(*
procedure TStickyBarButton.depress;

begin
	if not m_pressed then begin
		inherited Depress;
		if (my_menu <> 0) then
			CheckMenuItem(my_menu, my_command_id, MF_BYCOMMAND or MF_CHECKED);
	end;
end;

procedure TStickyBarButton.release;

begin
	if m_pressed then begin
		inherited Release;
		if (my_menu <> 0) then
			CheckMenuItem(my_menu, my_command_id, MF_BYCOMMAND or MF_UNCHECKED);
	end;
end;
*)
procedure TBarTextBox.Clear;

begin
	update('');
end;

constructor TToolBar.Init;
begin
	inherited Construct;
	NextXPos:=1;
end;

function TToolBar.Create(parent:HWND):HWND;
var
	rc:TRect;
begin
	my_parent:=parent;
	windows.GetClientRect(parent,rc);
	Create:=inherited Create('Button Bar',WS_CHILD or WS_VISIBLE,0,rc.bottom-BUTTON_BAR_HT,rc.right,BUTTON_BAR_HT,parent,190,hInstance,nil);
end;

procedure TBarButton.Create(playbar:PToolBar;aText:pchar;bbFlags:Word);
var
	aDC:HDC;
	textWd:longint;
begin
	my_playbar:=PlaybarP(playbar);
	aDC:=GetDC(GetDesktopWindow);
	my_menu:=0;
	textWd:=13+GetHdcTextWidth(aDC,aText)+13;
	ReleaseDC(GetDesktopWindow, aDC);
	if ((bbFlags and BB_SHIFT) <> 0) 
		then Inc(PToolBar(playbar)^.NextXPos, BB_SHIFT_PIXELS)
		else if ((bbFlags and BB_SHIFT_SHORT) <> 0) 
			then Inc(PToolBar(playbar)^.NextXPos, 2)
			else Inc(PToolBar(playbar)^.NextXPos, 2);
	handle:=CreateWindow('BUTTON',aText,WS_CHILD or WS_VISIBLE or BS_PUSHBUTTON,PToolBar(playbar)^.NextXPos,3,textWd,BAR_BUTTON_HT,playbar^.handle,myCmdId,hInstance,nil);
	Inc(PToolBar(playbar)^.NextXPos,textWd);
end;

function TBarTextBox.Create(aParent:PWindow;aX,aW:integer;const pText:pchar;bbFlags:Word):HWND;
var
	h:integer;
	aWnd:HWND;
begin //writeln('TBarTextBox.Create(aParent:PWindow,',aX,',',aW,',"',pText,'",bbFlags:Word)');
	if ((bbFlags and BB_SHIFT) <> 0) then Inc(PToolBar(aParent)^.NextXPos, BB_SHIFT_PIXELS);
	h:=BAR_BUTTON_HT-2;
	aWnd:=inherited Create(pText,WS_CHILD or WS_VISIBLE,PToolBar(aParent)^.NextXPos,Center(h,0,BUTTON_BAR_HT),aW,h,aParent^.handle,199,hInstance,nil);
	myText:=StrNew(pText);
	Inc(PToolBar(aParent)^.NextXPos,aW);
	Create:=aWnd;
end;

function TToolbar.OnCmd(aCmdId:UINT):LONG;
begin
	OnCmd:=windows.SendMessage(GetParent,WM_COMMAND,aCmdId,0);
end;

constructor TextBox.Construct;
begin
	inherited Init(NIL,'');
	myAlignment:=TA_LEFT; 
	hasBorder:=FALSE;
end;

function TextBox.Create(aParent:PWindow):HWND;
begin
	Create:=inherited Create(aParent,0,300,'',BB_SHIFT);
end;

procedure TBarTextBox.UpdateString(const aString:string);
var
	aBuffer:stringBuffer;
begin
	Update(StrPCopy(aBuffer,aString));
end;

{$ifdef TEST}
begin
	Suite.Add(@Test_BarNumber_AsText);
	Suite.Add(@Test_BarNumber_AsText_with_prefix);
	Suite.Run('toolbars');
{$endif}
end.
