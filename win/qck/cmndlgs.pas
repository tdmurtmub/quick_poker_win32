{ (C) 2006 Wesley Steiner }

{$MODE FPC}

unit cmndlgs;

interface

uses
	windows,
	odlg,
	quickWin, {$ifdef TEST} quickWinTests, {$endif}
	winqcktbl;

type
	TabletopDlg=object(ODialog)
		constructor Construct(frame:quickWin.FrameP;aView:PTabletop;const imagePath:ansistring);
		function OnCmd(aCmdId:UINT):LONG; virtual;
		function OnEndDialog(aCmdId:UINT):boolean; virtual;
		function OnInitDialog:boolean; virtual;
	private
		my_frame:quickWin.FrameP;
		myView:winqcktbl.PTabletop;
		myResetColor:TColorRef;
		myResetBitmap:HBITMAP;
		myResetUseImage:boolean;
		myResetImagePath:ansistring;
		myUseImageCheckBox:OCheckBox;
		function UseImageFromControl:boolean;
		function ImageMessage(const imagePath:ansistring;imageBitmap:HBITMAP):string;
		function IsValidImage(image:HBITMAP):boolean;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
		function OnBrowse:LONG;
		function OnReset(redraw:boolean):LONG;
		function OnUseImage:LONG;
		function OnHScroll(wScrollCode,nPos:integer;hwndCtl:HWND):bool;
		procedure ResetControls(redraw:boolean);
		procedure ScrollBy(control:HWND;n:integer);
		procedure ScrollTo(control:HWND;aPos:integer);
		procedure SetColorControls(color:TColorRef;redraw:Boolean);
		procedure SetImageControls(const imagePath:ansistring;imageBitmap:HBITMAP);
	end;

implementation

uses
	{$ifdef TEST} punit, {$endif}
	std,windowsx,gdiex,owindows,
	stdwin;

const
	ID_SCROLLR=101;
	ID_SCROLLG=102;
	ID_SCROLLB=103;

	IDC_USEIMAGE=1000;
	IDC_BROWSE=1001;
	IDC_IMAGEFILENAME=1004;
	IDC_IMAGEDIRNAME=1005;
	IDC_IMAGEMESSAGE=1007;

	IDS_NOIMAGEMESSAGE=101;
	IDS_BADIMAGETITLE=102;
	IDS_BADIMAGEMESSAGE=103;
	IDS_IMAGEBROWSETITLE=104;

constructor TabletopDlg.Construct(frame:quickWin.FrameP;aView:PTabletop;const imagePath:ansistring);

begin
	inherited Construct(frame^.MyFrameWindow^.Handle,502);
	my_frame:=frame;
	myView:=aView;
	myResetColor:=frame^.Tabletop^.BgColor;
	myResetBitmap:=frame^.Tabletop^.BgImage;
	myResetUseImage:=frame^.Tabletop^.UseBgImage;
	myResetImagePath:=imagePath;
end;

procedure TabletopDlg.ScrollBy(control:HWND;n:integer);

var
	newScrollPos:integer;

begin
	newScrollPos:=GetScrollPos(control,SB_CTL)+n;
	newScrollPos:=Max(0,newScrollPos);
	newScrollPos:=Min(255,newScrollPos);
	SetScrollPos(control, SB_CTL, newScrollPos, TRUE);
	with my_frame^.Tabletop^ do begin
		SetBackground(PaletteRGB(GetScrollPos(GetDlgItem(ID_SCROLLR),SB_CTL),GetScrollPos(GetDlgItem(ID_SCROLLG),SB_CTL),GetScrollPos(GetDlgItem(ID_SCROLLB),SB_CTL)),BgImage,UseBgImage);
		RefreshRect(ClientRect);
	end;
end;

procedure TabletopDlg.ScrollTo(control:HWND; aPos:integer);
begin
	SetScrollPos(control,SB_CTL,aPos,TRUE);
	ScrollBy(control,0);
end;

function TabletopDlg.OnHScroll(wScrollCode,nPos:integer;hwndCtl:HWND):bool;
begin //writeln('TabletopDlg.OnHScroll(code=',wScrollCode,',',nPos,',',hwndCtl,')');
	case wScrollCode of
		SB_LINEUP:ScrollBy(hwndCtl,-1);
		SB_PAGEUP:ScrollBy(hwndCtl,-10);
		SB_LINEDOWN:ScrollBy(hwndCtl,1);
		SB_PAGEDOWN:ScrollBy(hwndCtl,10);
		SB_THUMBTRACK,TB_THUMBPOSITION:ScrollTo(hwndCtl,nPos)
	end;
	OnHScroll:=FALSE;
end;

function TabletopDlg.OnInitDialog:boolean;
begin //WriteLn('TabletopDlg.OnInitDialog');
	OnInitDialog:=inherited OnInitDialog;
	myUseImageCheckBox.Handle:=GetDlgItem(IDC_USEIMAGE);
	SetScrollRange(GetDlgItem(ID_SCROLLR),SB_CTL,0,255,FALSE);
	SetScrollRange(GetDlgItem(ID_SCROLLG),SB_CTL,0,255,FALSE);
	SetScrollRange(GetDlgItem(ID_SCROLLB),SB_CTL,0,255,FALSE);
	ResetControls(FALSE);
end;

procedure TabletopDlg.SetImageControls(const imagePath:ansistring;imageBitmap:HBITMAP);
begin //writeln('TabletopDlg.SetImageControls(',imagePath,'imageBitmap)');
	XWnd(GetDlgItem(IDC_IMAGEMESSAGE)).SetWindowText(PChar(AnsiString(ImageMessage(imagePath,imageBitmap))));
	XWnd(GetDlgItem(IDC_IMAGEFILENAME)).SetWindowText(PChar(AnsiString(ExtractFileFromPath(imagePath))));
	XWnd(GetDlgItem(IDC_IMAGEDIRNAME)).SetWindowText(PChar(AnsiString(ExtractDirectoryFromPath(imagePath))));
	myUseImageCheckBox.SetCheck(Q(my_frame^.Tabletop^.UseBgImage,BST_CHECKED,BST_UNCHECKED));
	myUseImageCheckBox.EnableWindow(IsValidImage(my_frame^.Tabletop^.BgImage));
end;

procedure TabletopDlg.ResetControls(redraw:boolean);

begin
	SetColorControls(myResetColor,redraw);
	SetImageControls(myResetImagePath,myResetBitmap);
end;

function TabletopDlg.IsValidImage(image:HBITMAP):boolean;

begin
	IsValidImage:=(image<>NULL_HANDLE);	
end;

{$ifdef TEST}

type
	Test_TabletopDlg=object(TabletopDlg)
		constructor Construct;
	end;

constructor Test_TabletopDlg.Construct; begin end;

procedure Test_IsValidImage;

const
	NON_NULL_HANDLE=1234;

var
	testDialog:Test_TabletopDlg;

begin
	testDialog.Construct;
	AssertIsFalse(testDialog.IsValidImage(NULL_HANDLE));
	AssertIsTrue(testDialog.IsValidImage(NON_NULL_HANDLE));
end;

{$endif}

function TabletopDlg.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	OnMsg:= 0;
	case aMsg of
		WM_HSCROLL:OnMsg:=Q(OnHScroll(LOWORD(wParam),HIWORD(wParam),HWND(lParam)),1,0);
		else OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
	end;
end;

function TabletopDlg.UseImageFromControl:boolean;

begin
	UseImageFromControl:=(myUseImageCheckBox.IsChecked=TRUE);
end;

function TabletopDlg.OnBrowse:LONG;
var
  ofn:OPENFILENAME;
  buffer:array[0..260] of char;
  bitmap:HBITMAP;
begin
	ZeroMemory(@ofn,sizeof(OPENFILENAME));
	buffer:='';
	ofn.lStructSize:=sizeof(OPENFILENAME);
	ofn.hwndOwner:=Handle;
	ofn.lpstrTitle:=PChar(AnsiString(LoadResString(IDS_IMAGEBROWSETITLE)));
	ofn.lpstrFilter:='Image (BMP)'+Chr(0)+'*.BMP'+Chr(0);
	ofn.nFilterIndex:=0;
	ofn.lpstrFile:=@buffer; 
	ofn.nMaxFile:=sizeof(buffer);
	ofn.Flags:=OFN_FILEMUSTEXIST or OFN_LONGNAMES or OFN_NONETWORKBUTTON or OFN_PATHMUSTEXIST or OFN_HIDEREADONLY; 
	if GetOpenFileName(@ofn) then begin
		bitmap:=LoadBitmapFromFile(buffer);
		if bitmap<>NULL_HANDLE then begin
			myUseImageCheckBox.SetCheck(BST_CHECKED);
			with my_frame^.Tabletop^ do begin
				SetBackground(BgColor,bitmap,UseImageFromControl);
				with quickWin.FrameWindow(my_frame^.MyFrameWindow^) do if ShouldAutosize(UseImageFromControl,bitmap) then Autosize(bitmap);
				RefreshRect(ClientRect);
			end;
			SetImageControls(buffer,bitmap);
			my_frame^.Owner^.BgImagePath:=buffer;
		end
		else MessageBox(handle,PChar(AnsiString(LoadResString(IDS_BADIMAGEMESSAGE))),PChar(AnsiString(LoadResString(IDS_BADIMAGETITLE))),MB_ICONEXCLAMATION or MB_OK);
	end;
	OnBrowse:=0;
end;

procedure TabletopDlg.SetColorControls(color:TColorRef;redraw:Boolean);
begin
	SetScrollPos(GetDlgItem(ID_SCROLLR),SB_CTL,GetRValue(color),redraw);
	SetScrollPos(GetDlgItem(ID_SCROLLG),SB_CTL,GetGValue(color),redraw);
	SetScrollPos(GetDlgItem(ID_SCROLLB),SB_CTL,GetBValue(color),redraw);
end;

function TabletopDlg.OnReset(redraw:boolean):LONG;
begin
	with my_frame^.Tabletop^ do begin
		SetBackground(myResetColor,myResetBitmap,myResetUseImage);
		RefreshRect(ClientRect);
	end;
	ResetControls(redraw);
	OnReset:=0;
end;

function TabletopDlg.OnUseImage:LONG;
begin //writeln('TabletopDlg.OnUseImage');
	with my_frame^.Tabletop^ do begin
		SetBackground(BgColor,BgImage,UseImageFromControl);
		RefreshRect(ClientRect);
	end;
	OnUseImage:=0;
end;

function TabletopDlg.OnCmd(aCmdId:UINT):LONG;

begin //writeln('TabletopDlg.OnCmd(',aCmdId,')');
	case aCmdId of
		IDC_BROWSE:OnCmd:=OnBrowse;
		IDC_USEIMAGE:OnCmd:=OnUseImage;
		else OnCmd:=inherited OnCmd(aCmdId);
	end
end;

function TabletopDlg.OnEndDialog(aCmdId:UINT):boolean;
begin
	if aCmdId=IDCANCEL then OnReset(FALSE);
	OnEndDialog:=inherited OnEndDialog(aCmdId);
end;

function TabletopDlg.ImageMessage(const imagePath:ansistring;imageBitmap:HBITMAP):string;
begin
	if IsEmptyString(imagePath)
		then ImageMessage:='('+LoadResString(IDS_NOIMAGEMESSAGE)+')'
		else if imageBitmap=NULL_HANDLE
			then ImageMessage:=''
			else ImageMessage:='('+IntToStr(GetBitmapWd(imageBitmap))+' x '+IntToStr(GetBitmapHt(imageBitmap))+' pixels)';
end;

{$ifdef TEST}

procedure Test_ImageMessage;

var
	testDialog:Test_TabletopDlg;
	bitmap:HBITMAP;
	desktopDC,tempDC:HDC;

begin
	testDialog.Construct;
	desktopDC:=GetDC(GetDesktopWindow);
	tempDC:=CreateCompatibleDC(desktopDC);
	bitmap:=CreateCompatibleBitmap(tempDC,1024,768);
	DeleteDC(tempDC);
	ReleaseDC(GetDesktopWindow,desktopDC);
	AssertAreEqual('',testDialog.ImageMessage('some_image_path',NULL_HANDLE));
	AssertAreEqual('(no image selected, press the Browse button to select an image file)',testDialog.ImageMessage('',NULL_HANDLE));
	AssertAreEqual('(1024 x 768 pixels)',testDialog.ImageMessage('some_image_path',bitmap));
	DeleteObject(bitmap);
end;

{$endif TEST}

{$ifdef TEST}
begin
	Suite.Add(@Test_IsValidImage);
	Suite.Add(@Test_ImageMessage);
	Suite.Run('cmndlgs');
{$endif TEST}
end.
