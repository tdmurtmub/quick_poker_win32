{ Convenient WM Message Wrappers }
{ Copyright (C) 2004-2005 by Wesley Steiner. All rights reserved. }

{$I Platform}

unit wmmsgobj;

interface

uses
	windows,
	OWindows;

type
	WmMsgBase = object
		constructor Construct(var rMsg:OWindows.TMessage);
		function ResultCode:longint;
		procedure SetResultCode(a_aResult:longint);
	private
		m_rMsg:PMessage;
	end;

	WmInitMenuPopupMsg = object(WmMsgBase)
		function IsSystemMenu:boolean;
		function MenuIndex:word;
		function PopupMenu:HMENU;
	end;

	WmGetMinMaxInfoMsg = object(WmMsgBase)
		function MinMaxInfo:PMinMaxInfo;
	end;

	WmSizeMsg = object(WmMsgBase)
		function SizeType:word;
		function Width:integer;
		function Height:integer;
	end;

	WmMoveMsg = object(WmMsgBase)
		function Width:integer;
		function Height:integer;
	end;

	WMWindowPosChangedMsg = object(WmMsgBase)
		function WindowPos:PWindowPos;
	end;

implementation

{$ifdef TEST} uses PUnit; {$endif}

constructor WmMsgBase.Construct(var rMsg:OWindows.TMessage);
begin
	m_rMsg:= @rMsg;
end;

function WmInitMenuPopupMsg.IsSystemMenu:boolean;
begin
	IsSystemMenu:= not (m_rMsg^.LParamHi = 0);
end;

function WmInitMenuPopupMsg.MenuIndex:word;
begin
	MenuIndex:= m_rMsg^.LParamLo;
end;

function WmInitMenuPopupMsg.PopupMenu:HMENU;
begin
	PopupMenu:= m_rMsg^.WParam;
end;

function WmGetMinMaxInfoMsg.MinMaxInfo:PMinMaxInfo;

begin
	MinMaxInfo:= PMinMaxInfo(m_rMsg^.lParam);
end;

function WmMsgBase.ResultCode:longint;
begin
	ResultCode:= m_rMsg^.Result;
end;

procedure WmMsgBase.SetResultCode(a_aResult:longint);
begin
	m_rMsg^.Result:= a_aResult;
end;

function WmSizeMsg.Width:integer;

begin
	Width:= LoWord(m_rMsg^.LParam);
end;

function WmSizeMsg.Height:integer;

begin
	Height:= HiWord(m_rMsg^.LParam);
end;

function WmSizeMsg.SizeType:word;

begin
	SizeType:= m_rMsg^.WParam;
end;

function WMWindowPosChangedMsg.WindowPos:PWindowPos;

begin
	WindowPos:= PWindowPos(m_rMsg^.lParam);
end;

{$ifdef TEST}

procedure TestWMWindowPosChangedMsg_WindowPos; 

	var
		aMsg:OWindows.TMessage;
		aMessage:WMWindowPosChangedMsg;
		aWindowPos:TWindowPos;

	begin
		aMsg.lParam:= LongInt(@aWindowPos);
		aMessage.Construct(aMsg);
		Assert.EqualPtr(@aWindowPos, aMessage.WindowPos);
	end;

{$endif TEST}

{$ifdef TEST}

procedure TestWmInitMenuPopupMsg_PopupMenu; 

	var
		aMsg:OWindows.TMessage;
		aMessage:WmInitMenuPopupMsg;

	begin
		aMsg.WParam:= 123;
		aMessage.Construct(aMsg);
		Assert.Equal(123, aMessage.PopupMenu);
	end;

procedure TestWmInitMenuPopupMsg_MenuIndex; 

	var
		aMsg:OWindows.TMessage;
		aMessage:WmInitMenuPopupMsg;

	begin
		aMsg.LParamLo:= 100;
		aMessage.Construct(aMsg);
		Assert.Equal(100, aMessage.MenuIndex);
	end;

procedure TestWmInitMenuPopupMsg_IsSystemMenu; 
var
	aMsg:OWindows.TMessage;
	aMessage:WmInitMenuPopupMsg;
begin
	aMsg.LParamHi:= 0; { not system menu }
	aMessage.Construct(aMsg);
	Assert.IsFalse(aMessage.IsSystemMenu);

	aMsg.LParamHi:= 1; { is system menu }
	aMessage.Construct(aMsg);
	Assert.IsTrue(aMessage.IsSystemMenu);
end;

procedure TestWmGetMinMaxInfoMsg_MinMaxInfo; 

var
	aMsg:OWindows.TMessage;
	aMessage:WmGetMinMaxInfoMsg;
	aMinMaxInfo:TMinMaxInfo;
	pMinMaxInfo:^TMinMaxInfo;

begin
	{ must return lParam as a pointer to a TMinMaxInfo structure }
	pMinMaxInfo:= @aMinMaxInfo;
	aMsg.lParam:= longint(@aMinMaxInfo);
	aMessage.Construct(aMsg);
	Assert.EqualPtr(@aMinMaxInfo, aMessage.MinMaxInfo);
end;

procedure TestWmMsgBase_Result; 
var
	aMsg:OWindows.TMessage;
	aMessage:WmMsgBase;
begin
	{ must return the value stored in TMessage.Result }
	aMsg.Result:= longint(126279);
	aMessage.Construct(aMsg);
	Assert.EqualLong(126279, aMessage.ResultCode);
end;

procedure TestWmMsgBase_SetResult; 

var
	aMsg:OWindows.TMessage;
	aMessage:WmMsgBase;

begin
	{ must set TMessage.Result to the argument value }
	aMessage.Construct(aMsg);
	aMessage.SetResultCode(182309);
	Assert.EqualLong(182309, aMessage.m_rMsg^.Result);
end;

procedure TestWmSizeMsg_Width; 

var
	aMsg:OWindows.TMessage;
	aMessage:WmSizeMsg;

begin
	aMessage.Construct(aMsg);

	aMsg.WParam:=SIZE_MAXIMIZED;
	Assert.Equal(SIZE_MAXIMIZED, aMessage.SizeType);

	aMsg.LParam:= $08FF0123;
	Assert.EqualLong($0123, aMessage.Width);
	Assert.EqualLong($08FF, aMessage.Height);
end;

{$endif TEST}

function WmMoveMsg.Width:integer;

	begin
		Width:= LoWord(m_rMsg^.LParam);
	end;

function WmMoveMsg.Height:integer;

	begin
		Height:= HiWord(m_rMsg^.LParam);
	end;

{$ifdef TEST}

procedure TestWmMoveMsg_Width; 

	var
		aMsg:OWindows.TMessage;
		aMessage:WmMoveMsg;

	begin
		aMessage.Construct(aMsg);

		aMsg.LParam:= $08FF0123;
		Assert.EqualLong($0123, aMessage.Width);
		Assert.EqualLong($08FF, aMessage.Height);
	end;

{$endif TEST}

begin

	{$ifdef TEST}
	Suite.Add(TestWmMsgBase_Result);
	Suite.Add(TestWmMsgBase_SetResult);
	Suite.Add(TestWmGetMinMaxInfoMsg_MinMaxInfo);
	Suite.Add(TestWmInitMenuPopupMsg_PopupMenu);
	Suite.Add(TestWmInitMenuPopupMsg_MenuIndex);
	Suite.Add(TestWmInitMenuPopupMsg_IsSystemMenu);
	Suite.Add(TestWmSizeMsg_Width);
	Suite.Add(TestWMWindowPosChangedMsg_WindowPos);
	Suite.Add(TestWmMoveMsg_Width);
	Suite.Run('WmMsgObj');
	{$endif TEST}

end.
