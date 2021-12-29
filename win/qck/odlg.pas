{ (C) 2006 Wesley Steiner }

{$MODE FPC}

{$I platform}

unit odlg;

interface

uses
	windows,oapp,owindows;

type
	ODialogPtr=^ODialog;
	ODialog=object(OWindow)
		constructor Construct(aParent:HWND;aResId:UINT);
		destructor Destruct; virtual;
		function Create(aInstance:HINST):HWND; virtual;
		function GetDlgItem(IDDlgItem:UINT):HWND;
		function OnEndDialog(aCmdId:UINT):boolean; virtual;
		function OnInitDialog:boolean; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		function Modal:long;
	private
		myParent:HWND;
		myResId:UINT;
	end;

	OPersistentDlg=object(ODialog)
		constructor Construct(aParent:HWND;aResId:UINT;const aStorageKey:pchar;aDefaultX,aDefaultY:LONG);
		destructor Destruct; virtual;
		function OnInitDialog:boolean; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
	private
		myPersistor:PopupWndPersistor;
		myDefaultX,myDefaultY:LONG;
	end;

	StickyDialog=object(OPersistentDlg)
		constructor Construct(aParent:HWND;aResId:UINT;const aStorageKey:pchar);
	end;

	OListBox=object(OWND)
		function AddString(const text:string):int;
	end;

	OCheckBox=object(OWND)
		function GetCheck:LRESULT;
		function IsChecked:boolean;
		procedure SetCheck(check_state:LONG);
	end;

	OListCtrl=object(OWND)
		function InsertColumn(n:int;const aTitle:pchar;width_in_pixels:int;alignment:int):int;
		function AppendItem(const text:string):int;
		function SetItemData(n:int;data:LPARAM):int;
		function SetSubItemText(aItemIndex,aSubItemIndex:int;const text:string):int;
		function SortItems(compare:PFNLVCOMPARE;applicationData:LPARAM):LRESULT;
	end;

{$ifdef TEST}

	ODialogStub=object(ODialog)
		constructor Construct;
	end;

{$endif}

function XCheckBox(h:HANDLE):OCheckBox;
function XListCtrl(h:HANDLE):OListCtrl;
function XListBox(h:HANDLE):OListBox;

implementation

uses
	{$ifdef TEST} punit, {$endif}
	std,strings,
	stringsx,
	sdkex;

var
	the_dialog_stack:PointerStack;

function ODialogProc(aDialogHandle:HWND;aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;stdcall;

var
	aDialog:ODialogPtr;

begin
	ODialogProc:=1;
	aDialog:=the_dialog_stack.Peek;
	case aMsg of
		WM_INITDIALOG:aDialog^.Handle:=aDialogHandle;
		WM_COMMAND:begin
			if (long(wParam)=IDOK) or (long(wParam)=IDCANCEL) then begin
				if aDialog^.OnEndDialog(UINT(wParam)) then begin
					EndDialog(aDialogHandle,wParam);
					Exit;
				end;
			end;
		end;
		WM_NCDESTROY:the_dialog_stack.Pop;
	end;
	ODialogProc:=LRESULT(aDialog^.OnMsg(aMsg,wParam,lParam));
end;

{$ifdef TEST}

type
	TestODialogBase=object(ODialog)
		constructor Construct;
	end;

	TestODialog=object(TestODialogBase)
		OnInitDialogWasCalled:boolean;
		function OnInitDialog:boolean; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
	end;

constructor TestODialogBase.Construct;

begin
end;

procedure Test_ODialgProc_WM_COMMAND;

var
	aDialog:TestODialogBase;

begin
	aDialog.Construct;
	the_dialog_stack.Push(@aDialog);
	punit.Assert.EqualLong(1,ODialogProc(0,WM_COMMAND,IDOK,0));
	punit.Assert.EqualLong(1,ODialogProc(0,WM_COMMAND,IDCANCEL,0));
	punit.Assert.EqualLong(0,ODialogProc(0,WM_COMMAND,$12342222,0));
end;

{$endif}

function ODialog.OnEndDialog(aCmdId:UINT):boolean;

begin
	OnEndDialog:=true;
end;

function ODialog.OnInitDialog:boolean;

begin
	OnInitDialog:=TRUE;
end;

{$ifdef TEST}

procedure Test_OnInitDialog; 

var
	aDlg:ODialog;

begin
	aDlg.Construct(0,1);
	punit.Assert.IsTrue(aDlg.OnInitDialog);
end;

{$endif TEST}

function ODialog.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):long;

begin
	case aMsg of
		WM_INITDIALOG:OnMsg:=Q(OnInitDialog,1,0);
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

{$ifdef TEST}

function TestODialog.OnInitDialog:boolean;

begin
	OnInitDialogWasCalled:=true;
	OnInitDialog:=inherited OnInitDialog;
end;

function TestODialog.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):long;

begin
	OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
end;

procedure Test_OnMsg_WM_INITDIALOG; 

var
	aDlg:TestODialog;
	ret:LONG;

begin
	aDlg.Construct;

	aDlg.OnInitDialogWasCalled:=false;
	ret:=aDlg.OnMsg(WM_INITDIALOG,0,0);
	punit.Assert.Equal(1,ret);
	punit.Assert.IsTrue(aDlg.OnInitDialogWasCalled);

	aDlg.OnInitDialogWasCalled:=false;
	ret:= aDlg.OnMsg(0,0,0);
	punit.Assert.Equal(0,ret);
	punit.Assert.IsFalse(aDlg.OnInitDialogWasCalled);
end;

{$endif TEST}

constructor ODialog.Construct(aParent:HWND;aResId:UINT);

begin
	inherited Construct;
	the_dialog_stack.Push(@self);
	myParent:=aParent;
	myResId:=aResId;
end;

destructor ODialog.Destruct;

begin //Writeln('ODialog.Destruct');
end;

function ODialog.Modal:long;

begin
	Modal:=LONG(DialogBox(HINST(hInstance),MakeIntResource(myResId),HWND(myParent),@ODialogProc));
end;

function ODialog.GetDlgItem(IDDlgItem:UINT):HWND;

begin
	GetDlgItem:=windows.GetDlgItem(Handle,IDDlgItem);
end;

{$ifdef TEST}

constructor ODialogStub.Construct;

begin
end;

{$endif TEST}

function OListCtrl.InsertColumn(n:int;const aTitle:pchar;width_in_pixels:int;alignment:int):int;

var
	lv:TLVCOLUMN;

begin
	lv.mask:=LVCF_TEXT or LVCF_WIDTH or LVCF_FMT;
	lv.fmt:=alignment;
	lv.cx:=width_in_pixels;
	lv.pszText:=aTitle;
	InsertColumn:=ListView_InsertColumn(handle,n,lv);
end;

function OListCtrl.AppendItem(const text:string):int;

var
	lv:TLVITEM;

var
	abuffer:stringBuffer;

begin
	lv.mask:=LVIF_TEXT;
	lv.iItem:=MAX_LONG;
	lv.iSubItem:=0;
	lv.pszText:=StrPCopy(aBuffer,text);
	AppendItem:=ListView_InsertItem(handle,lv);
end;

function OListCtrl.SetSubItemText(aItemIndex,aSubItemIndex:int;const text:string):int;

var
	abuffer:stringBuffer;

begin
	SetSubItemText:=ListView_SetItemText(handle,aItemIndex,aSubItemIndex,StrPCopy(aBuffer,text));
end;

function OListCtrl.SortItems(compare:PFNLVCOMPARE;applicationData:LPARAM):LRESULT;

begin
	SortItems:=ListView_SortItems(handle,compare,applicationData);
end;

procedure InitializeSetItemData(var lv:LV_ITEM);

begin
	lv.mask:=LVIF_PARAM;
	lv.iSubItem:=0;
end;

{$ifdef TEST}

procedure test_InitializeSetItemData;

var
	lv:LV_ITEM;

begin
	InitializeSetItemData(lv);
	AssertAreEqual(LVIF_PARAM,lv.mask);
	AssertAreEqual(0,lv.iSubItem);
end;

{$endif}

function OListCtrl.SetItemData(n:int;data:LPARAM):int;

var
	lv:LV_ITEM;

begin
	InitializeSetItemData(lv);
	lv.iItem:=n;
	lv.lParam:=data;
	SetItemData:=ListView_SetItem(handle,lv);
end;

function OCheckBox.GetCheck:LRESULT;

begin
	GetCheck:=SendMessage(BM_GETCHECK,0,0);	
end;

procedure OCheckBox.SetCheck(check_state:LONG);

begin
	SendMessage(BM_SETCHECK,check_state,0);
end;

constructor OPersistentDlg.Construct(aParent:HWND;aResId:UINT;const aStorageKey:pchar;aDefaultX,aDefaultY:LONG);

begin
	inherited Construct(aParent,aResId);
	myDefaultX:=aDefaultX;
	myDefaultY:=aDefaultY;
	myPersistor.Construct(@self,aStorageKey,TRUE,FALSE,myDefaultX,myDefaultY);
end;

destructor OPersistentDlg.Destruct;

begin
	myPersistor.Destruct;
	inherited Destruct;
end;

function OPersistentDlg.OnInitDialog:boolean;

var
	aXPos,aYPos:LONG;

begin
	OnInitDialog:=inherited OnInitDialog;
	myPersistor.RestorePos(aXPos,aYPos);
	if (aXPos<>LONG(CW_USEDEFAULT)) and (aYPos<>LONG(CW_USEDEFAULT)) then MoveWindow(Handle,aXPos,aYPos,GetWndWd(Handle),GetWndHt(Handle),TRUE);
end;

function OPersistentDlg.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	OnMsg:= 0;
	case aMsg of
		WM_MOVE:begin
			myPersistor.SavePos;
			OnMsg:=0;
		end;
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

function OListBox.AddString(const text:string):int;

var
	abuffer:stringBuffer;

begin
	StrPCopy(abuffer,text);
	AddString:=SendMessage(LB_ADDSTRING,0,LongInt(@abuffer));
end;

constructor StickyDialog.Construct(aParent:HWND;aResId:UINT;const aStorageKey:pchar);

begin
	inherited Construct(aParent,aResId,aStorageKey,LONG(CW_USEDEFAULT),LONG(CW_USEDEFAULT));
end;

function ODialog.Create(aInstance:HINST):HWND;

begin
	Create:=ApiCheck(CreateDialog(aInstance,MakeIntResource(myResId),myParent,@ODialogProc));
end;

function IsChecked_logic(checkedState:LRESULT):boolean;

begin
	IsChecked_logic:=(checkedState=BST_CHECKED)
end;

{$ifdef TEST}

procedure Test_IsChecked_logic;

begin
	AssertIsTrue(IsChecked_logic(BST_CHECKED));
	AssertIsFalse(IsChecked_logic(BST_UNCHECKED));
	AssertIsFalse(IsChecked_logic(BST_INDETERMINATE));
end;

{$endif TEST}

function OCheckBox.IsChecked:boolean;

begin
	IsChecked:=IsChecked_logic(GetCheck);
end;

function XCheckBox(h:HWND):OCheckBox;

var
	o:OCheckBox;
	
begin
	o.handle:=h;
	XCheckBox:=o;
end;

function XListCtrl(h:HANDLE):OListCtrl;

var
	o:OListCtrl;
	
begin
	o.handle:=h;
	XListCtrl:=o;
end;

function XListBox(h:HANDLE):OListBox;

var
	o:OListBox;
	
begin
	o.handle:=h;
	XListBox:=o;
end;

begin
	the_dialog_stack.Construct;
{$ifdef TEST}
	Suite.Add(@test_OnInitDialog);
	Suite.Add(@test_OnMsg_WM_INITDIALOG);
	Suite.Add(@test_ODialgProc_WM_COMMAND);
	Suite.Add(@test_IsChecked_logic);
	Suite.Add(@test_InitializeSetItemData);
	Suite.Run('odlg');
{$endif TEST}
end.
