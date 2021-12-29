{ (C) 2005-2007 Wesley Steiner }

{$MODE FPC}

{$I platform}

unit qpdlgs;

interface

uses
	windows,odlg,
	pokerlib,drawplib,studplib;

const
	ANTE_EDITBOX_MIN=0;
	ANTE_EDITBOX_MAX=100;
	ANTE_EDITBOX_TEXTLEN=3;

type
	tAnteEditBoxTransferBuffer = array[0..ANTE_EDITBOX_TEXTLEN] of char;

	DrawOptionsDlg=object(ODialog)
		constructor Init;
		function OnInitDialog:boolean; virtual;
		function OnEndDialog(aCmdId:UINT):boolean; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
	private
		function DoVariationSelection(aVariation:drawPokerVar):boolean;
		function ProcessVariationSelection(aControlId:UINT):boolean;
		procedure SetControls;
		procedure SetupDrawTransferBuffer(aVariation:drawPokerVar;var settings:tDrawPokerOptions);
		procedure SetVariationButton(aVariation:drawPokerVar);
	end;

	StudOptionsDlg=object(ODialog)
		constructor Init;
		function OnInitDialog:boolean; virtual;
		function OnEndDialog(aCmdId:UINT):boolean; virtual;
		function OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG; virtual;
	private
		function DoVariationSelection(aVariation:studPokerVar):boolean;
		function ProcessVariationSelection(aControlId:UINT):boolean;
		procedure Setup(aVariation:StudPokerVar);
		procedure SetVariationButton(aVariation:studPokerVar);
	end;

var
	theDrawSettingsTransferBuffer:record
		myVariation:DrawPokerVar;
		myAnteEditboxBuffer:tAnteEditBoxTransferBuffer;
		myPasses:array[0..1] of word;
	end;

	theStudSettingsTransferBuffer:record
		myVariation:StudPokerVar;
		myAnteEditboxBuffer:tAnteEditBoxTransferBuffer;
	end;

implementation

uses
	{$ifdef TEST} punit, {$endif}
	std,
	owindows,
	quickWin;

const
	ID_CUSTOM=108;
	ID_CUSTOMBOX=300;
	ID_PIPBOX=302;
	ID_SUITBOX=303;
	ID_CLEAR=150;
	ID_JOKER=102;
	ID_WILDA=101;
	RLM_PERROUND=999;
	RLM_PERPERSON=10;
	RLM_FACTOR=10;
	ID_ANYTHING=102;
	ID_JACKPOTS=103;
	ID_ANTEBOX=110;
	ID_PASSBEFORE=116;
	ID_PASSAFTER=107;
	ID_5CARD=115;
	ID_6CARD=ID_5CARD+1;
	ID_7CARD=ID_5CARD+2;
	ID_STUDANTE=102;

	{$ifdef INACTIVE}
	PDrawLimitsDlg = ^TDrawLimitsDlg;
	TDrawLimitsDlg = object(ODialog)
		constructor Init(aParent:PWindowsObject);
	end;

	PStudLimitsDlg=^TStudLimitsDlg;
	TStudLimitsDlg=object(ODialog)
		constructor Init(aParent:PWindowsObject);
	end;
	{$endif}

{$ifdef INACTIVE}

type
	PWildDlg=^TWildDlg;
	TWildDlg=object(TDialog)
		constructor Init(aParent:PWindowsObject);
		procedure IDCustom(var Msg:TMessage); virtual id_First + id_Custom;
		{procedure WMDrawItem(var Msg:TMessage); virtual wm_First+wm_DrawItem;}
	end;

var
	WildData:record
		Choice:array[WildType] of Word;
	end;

	x_aDrawLimitsDlgTransferBuffer:record
		m_aBeforeDrawMinMax, m_aAfterDrawMinMax:array[0..2] of Word;
		m_aRaiseTotalPerRound,tpp,trf:array[0..sl_Word] of char;
	end;

	x_aStudLimitsDlgTransferBuffer:record
		bRound:array[1..StudPLib.MAX_BETTING_ROUNDS] of array[1..3] of Word;
		m_aRaiseTotalPerRound,tpp,trf:array[0..sl_Word] of Char;
	end;

	wGame:PokerGameType; { game we are processing wilds for }

constructor TDrawLimitsDlg.Init;

const
	id_RaiseTPR=117;
	id_RaiseTPP=118;
	id_RaiseBet=116;

var
	Edit:PEdit;
	radio:PCheckBox;

begin
	inherited Init(aParent,'Draw_Limits');

	radio:= new(PCheckBox, InitResource(@Self, 107));
	radio:= new(PCheckBox, InitResource(@Self, 108));
	radio:= new(PCheckBox, InitResource(@Self, 109));
	radio:= new(PCheckBox, InitResource(@Self, 104));
	radio:= new(PCheckBox, InitResource(@Self, 105));
	radio:= new(PCheckBox, InitResource(@Self, 106));

	Edit:= New(PEdit,InitResource(@Self,id_RaiseTPR,sl_Word+1));
	Edit:= New(PEdit,InitResource(@Self,id_RaiseTPP,sl_Word+1));
	Edit:= New(PEdit,InitResource(@Self,id_RaiseBet,sl_Word+1));

	FillChar(x_aDrawLimitsDlgTransferBuffer, SizeOf(x_aDrawLimitsDlgTransferBuffer), #0);
	with x_aDrawLimitsDlgTransferBuffer, theDrawPokerOptions do begin
		case beforeDrawOpen of
			1:m_aBeforeDrawMinMax[0]:= 1;
			5:m_aBeforeDrawMinMax[1]:= 1;
			10:m_aBeforeDrawMinMax[2]:= 1;
		end;
		case afterDrawOpen of
			2:m_aAfterDrawMinMax[0]:= 1;
			10:m_aAfterDrawMinMax[1]:= 1;
			20:m_aAfterDrawMinMax[2]:= 1;
		end;
		with RaiseLmts do begin
			w2s(LmtPerRnd,m_aRaiseTotalPerRound);
			w2s(LmtPerPsn,tpp);
			w2s(RaiseFactor,trf);
		end;
	end;
	TransferBuffer:= @x_aDrawLimitsDlgTransferBuffer;
end;

procedure DrawOptionsDlg.IDLimits;

	var
		i:integer;

	begin
		if Application^.ExecDialog(New(PDrawLimitsDlg, Init(@Self))) = idOK then
		with x_aDrawLimitsDlgTransferBuffer, theDrawPokerOptions do begin

			{ reset the 'before-the-draw' limits }

			i:= 0;
			while (m_aBeforeDrawMinMax[i] = 0) do inc(i);
			case i of
				0:BeforeDrawOpen:= 1;
				1:BeforeDrawOpen:= 5;
				2:BeforeDrawOpen:= 10;
			end;
			BeforeDrawOpenMax:= BeforeDrawOpen * 5;

			{ reset the 'after-the-draw' limits }

			i:= 0;
			while (m_aAfterDrawMinMax[i] = 0) do inc(i);
			case i of
				0:AfterDrawOpen:= 2;
				1:AfterDrawOpen:= 10;
				2:afterDrawOpen:= 20;
			end;
			AfterDrawOpenMax:= afterDrawOpen * 5;

			with RaiseLmts do begin
				if s2w(m_aRaiseTotalPerRound) >= 0 then LmtPerRnd:= minw(RLM_PERROUND, s2w(m_aRaiseTotalPerRound));
				if s2w(tpp) >= 0 then LmtPerPsn:= minw(RLM_PERPERSON, s2w(tpp));
				if s2w(trf) > 0 then RaiseFactor:= minw(RLM_FACTOR, s2w(trf));
			end;
		end;
	end;

constructor TStudLimitsDlg.Init;

	var
		i:integer;
		Edit:PEdit;
		check:PCheckBox;

	begin
		inherited Init(aParent,'Stud_Limits');

		for i:=1 to StudPLib.MAX_BETTING_ROUNDS do begin
			check:= new(PCheckBox, InitResource(@Self, 200 + i * 10 + 1));
			check:= new(PCheckBox, InitResource(@Self, 200 + i * 10 + 2));
			check:= new(PCheckBox, InitResource(@Self, 200 + i * 10 + 3));
		end;

		Edit:=New(PEdit,InitResource(@Self,122,sl_Word+1));
		Edit:=New(PEdit,InitResource(@Self,123,sl_Word+1));
		Edit:=New(PEdit,InitResource(@Self,124,sl_Word+1));

		FillChar(x_aStudLimitsDlgTransferBuffer, SizeOf(x_aStudLimitsDlgTransferBuffer), #0);
		with x_aStudLimitsDlgTransferBuffer,x_aStudPokerOptions do begin
			for i:=1 to StudPLib.MAX_BETTING_ROUNDS do case m_aRoundBettingLimits.MinOpen[i] of
				1:bRound[i, 1]:= 1;
				5:bRound[i, 2]:= 1;
				10:bRound[i, 3]:= 1;
			end;
			with RaiseLmts do begin
				w2s(LmtPerRnd, m_aRaiseTotalPerRound);
				w2s(LmtPerPsn, tpp);
				w2s(RaiseFactor, trf);
			end;
		end;
		TransferBuffer:= @x_aStudLimitsDlgTransferBuffer;
	end;

procedure DrawOptionsDlg.IDWildcards;

begin
	wGame:=DrawPoker;
	if Application^.ExecDialog(New(PWildDlg,Init(@Self)))=idOK then
		with theDrawPokerOptions,WildData do begin
			wType:=Low(WildType);
			while Choice[wType]=0 do wType:=Succ(wType);
		end;
end;

{$endif INACTIVE}

procedure StudOptionsDlg.Setup(aVariation:StudPokerVar);

begin
	FillChar(theStudSettingsTransferBuffer, SizeOf(theStudSettingsTransferBuffer), #0);
	with theStudSettingsTransferBuffer do begin
		myVariation:=aVariation;
		w2s(studplib.Ante(aVariation), myAnteEditboxBuffer);
	end;
	with theStudSettingsTransferBuffer do begin
		windows.SetWindowText(GetDlgItem(ID_STUDANTE),myAnteEditboxBuffer);
	end;
end;

procedure StudOptionsDlg.SetVariationButton(aVariation:studPokerVar);

var
	aVariationButton:OWnd;

begin
	aVariationButton.handle:=GetDlgItem(ID_5CARD+Ord(aVariation));
	aVariationButton.SendMessage(BM_SETCHECK,BST_CHECKED,0);
end;

function StudOptionsDlg.OnInitDialog:boolean;

var
	spv:StudPokerVar;

begin
	CenterDialog(@Self, AppWnd);
	windows.SendMessage(GetDlgItem(ID_STUDANTE),EM_LIMITTEXT,ANTE_EDITBOX_TEXTLEN+1,0);
	SetVariationButton(studplib.Variation);
	Setup(studplib.Variation);
	OnInitDialog:=inherited OnInitDialog;
end;

function StudOptionsDlg.OnEndDialog(aCmdId:UINT):boolean;

var
	spv:StudPokerVar;
	checked:long;

begin
	if aCmdId=IDOK then begin	
		FillChar(theStudSettingsTransferBuffer,SizeOf(theStudSettingsTransferBuffer),#0);
		with theStudSettingsTransferBuffer do begin
			for spv:=Low(StudPokerVar) to High(StudPokerVar) do begin
				checked:=windows.SendMessage(GetDlgItem(ID_5CARD+Ord(spv)),BM_GETCHECK,0,0);
				if checked=BST_CHECKED then myVariation:=studPokerVar(Ord(spv));
			end;
			GetWindowText(GetDlgItem(ID_STUDANTE),myAnteEditboxBuffer,ANTE_EDITBOX_TEXTLEN+1);
		end;
	end;
	OnEndDialog:=TRUE;
end;

constructor StudOptionsDlg.Init;

begin
	inherited Construct(theApp^.MainWindow^.MyFrameWindow^.handle,902);
end;

{$ifdef TEST}

{$ifdef INACTIVE}

procedure Test_TDrawLimitsDlg_Init;

var
	aDlg:TDrawLimitsDlg;
	aTextBuffer:array[0..255] of char;

begin
	{ transfer buffer order }
	w2s(123, x_aDrawLimitsDlgTransferBuffer.m_aRaiseTotalPerRound);
	aDlg.Init(nil);
	aDlg.TransferData(TF_SETDATA);
	GetWindowText(GetDlgItem(aDlg.HWindow, 117), aTextBuffer, 255);
	Assert.EqualText('123', aTextBuffer);
end;

{$endif}

{$endif}

constructor DrawOptionsDlg.Init;

begin
	inherited Construct(theApp^.MainWindow^.MyFrameWindow^.handle,901);
end;

procedure DrawOptionsDlg.SetupDrawTransferBuffer(aVariation:drawPokerVar;var settings:tDrawPokerOptions);

begin
	FillChar(theDrawSettingsTransferBuffer,SizeOf(theDrawSettingsTransferBuffer),#0);
	with theDrawSettingsTransferBuffer,settings do begin
		myVariation:=aVariation;
		w2s(AnteChips[aVariation],myAnteEditboxBuffer);
		myPasses[0]:=Word(PassOK[aVariation,0]);
		myPasses[1]:=Word(PassOK[aVariation,1]);
	end;
	SetControls;
end;

procedure DrawOptionsDlg.SetVariationButton(aVariation:drawPokerVar);

var
	aVariationButton:OWnd;

begin
	aVariationButton.handle:=GetDlgItem(ID_ANYTHING+Ord(aVariation));
	aVariationButton.SendMessage(BM_SETCHECK,BST_CHECKED,0);
end;

procedure DrawOptionsDlg.SetControls;

var
	aPassBeforeButton,aPassAfterButton:OWnd;

begin
	aPassBeforeButton.handle:=GetDlgItem(ID_PASSBEFORE);
	aPassAfterButton.handle:=GetDlgItem(ID_PASSAFTER);
	with theDrawSettingsTransferBuffer do begin
		aPassBeforeButton.SendMessage(BM_SETCHECK,Q(myPasses[0]<>0,BST_CHECKED,BST_UNCHECKED),0);
		aPassAfterButton.SendMessage(BM_SETCHECK,Q(myPasses[1]<>0,BST_CHECKED,BST_UNCHECKED),0);
		windows.SetWindowText(GetDlgItem(ID_ANTEBOX),myAnteEditboxBuffer);
	end;
end;

function DrawOptionsDlg.OnInitDialog:boolean;

var
	aVariationButton,aPassBeforeButton,aPassAfterButton:OWnd;

begin
//	WriteLn('DrawOptionsDlg.OnInitDialog');
	CenterDialog(@Self,AppWnd);
	windows.SendMessage(GetDlgItem(ID_ANTEBOX),EM_LIMITTEXT,ANTE_EDITBOX_TEXTLEN+1,0);
	SetVariationButton(drawplib.Variation);
	SetupDrawTransferBuffer(drawplib.Variation,theDrawPokerOptions);
	OnInitDialog:=inherited OnInitDialog;
end;

function DrawOptionsDlg.OnEndDialog(aCmdId:UINT):boolean;

var
	dpv:DrawPokerVar;
	checked:long;
	aPassBeforeButton,aPassAfterButton:OWnd;

begin
	if aCmdId=IDOK then begin
		aPassBeforeButton.handle:=GetDlgItem(ID_PASSBEFORE);
		aPassAfterButton.handle:=GetDlgItem(ID_PASSAFTER);
		FillChar(theDrawSettingsTransferBuffer,SizeOf(theDrawSettingsTransferBuffer),#0);
		with theDrawSettingsTransferBuffer do begin
			for dpv:=Low(DrawPokerVar) to High(DrawPokerVar) do begin
				checked:=windows.SendMessage(GetDlgItem(ID_ANYTHING+Ord(dpv)),BM_GETCHECK,0,0);
				if checked=BST_CHECKED then myVariation:=drawPokerVar(dpv);
			end;
			checked:=aPassBeforeButton.SendMessage(BM_GETCHECK,0,0);
			if checked=BST_CHECKED then myPasses[0]:=1;
			checked:=aPassAfterButton.SendMessage(BM_GETCHECK,0,0);
			if checked=BST_CHECKED then myPasses[1]:=1;
			GetWindowText(GetDlgItem(ID_ANTEBOX),myAnteEditboxBuffer,ANTE_EDITBOX_TEXTLEN+1);
		end;
	end;
	OnEndDialog:=true;
end;

{$ifdef INACTIVE}

constructor TWildDlg.Init;

	var
		w:WildType;
		Radio:PRadioButton;

	begin
		inherited Init(aParent,'Wildcards');
		for w:=Low(WildType) to High(WildType) do Radio:=New(PRadioButton,InitResource(@Self,101+Ord(w)));
		FillChar(WildData,SizeOf(WildData),#0);
		with WildData do if wGame=DrawPoker then
			Choice[theDrawPokerOptions.wType]:=1
		else
			Choice[x_aStudPokerOptions.wType]:=1;
		TransferBuffer:= @WildData;
	end;

type
	PCustomDlg=^TCustomDlg;
	TCustomDlg=object(TDialog)
		constructor Init(aParent:PWindowsObject);
		procedure WMDrawItem(var Msg:TMessage); virtual wm_First+wm_DrawItem;
		procedure IDClear(var Msg:TMessage); virtual id_First+id_Clear;
	end;

var
	TmpWilds:WildCardList;
	CustomData:record
		Joker:Word;
	end;

procedure TCustomDlg.IDClear;

	begin
		FillChar(TmpWilds,SizeOf(TmpWilds),#0);
		CustomData.Joker:=0;
		TransferData(tf_SetData);
		InvalidateRect(GetItemHandle(id_CustomBox),nil,True);
		UpdateWindow(GetItemHandle(id_CustomBox));
	end;

constructor TCustomDlg.Init;

	var
		Check:PCheckBox;

	begin
		inherited Init(aParent,'Custom');
		Check:=New(PCheckBox,InitResource(@Self,id_Joker));
		if wGame=DrawPoker then with theDrawPokerOptions do begin
			TmpWilds:=WildCards;
			if Wildcards.jWild then
				CustomData.Joker:=1
			else
				CustomData.Joker:=0;
		end
		else with x_aStudPokerOptions do begin
			TmpWilds:=WildCards;
			if Wildcards.jWild then
				CustomData.Joker:=1
			else
				CustomData.Joker:=0;
		end;
		TransferBuffer:= @CustomData;
		{tmpwilds.wlist[sixpip,diamondsuit]:=true;}
	end;

procedure TWildDlg.IDCustom;

	begin
		if Application^.ExecDialog(New(PCustomDlg,Init(@Self)))=idOK then begin
			TmpWilds.jWild:=boolean(CustomData.Joker);
			if wGame=DrawPoker then
				theDrawPokerOptions.WildCards:=TmpWilds
			else
				x_aStudPokerOptions.WildCards:=TmpWilds;
		end;
	end;

procedure TCustomDlg.WMDrawItem;

	type
		PDrawItemStruct=^TDrawItemStruct;

	var
		Row:pcSuitType;
		Col:pcPipType;
		wd,ht:integer;
		r:TRect;pt:TPoint;
		ps:array[0..10] of Char;

	procedure SetState(tp:pcPipType;ts:pcSuitType);

		{ indicate the wild state of this card in the table }

		var
			r:TRect;
			bm:hBitmap;

		begin
			with PDrawItemStruct(Msg.lParam)^,r do begin
				Left:=Ord(tp)*Wd+1;
				Top:=Ord(ts)*Ht+1;
				Right:=Left+Wd-1;
				Bottom:=Top+Ht-1;
				if TmpWilds.wList[tp,ts] then begin
					bm:=x_engine.SuitIcon(ts);
					{LoadBitmap(0,MakeIntResource(obm_Check));}
					PutBitmap(hDC,bm,
						Centered(GetBitmapWd(bm),Left,Right),
						Centered(GetBitmapHt(bm),Top,Bottom),
						SrcCopy);
				end
				else
					FillRect(hDC,r,GetStockObject(White_Brush));
			end;
		end;

	begin
		with PDrawItemStruct(Msg.lParam)^ do begin
			Wd:=(RectWd(rcItem) div 13);
			Ht:=RectHt(rcItem) div 4;
			case Msg.wParam of
				id_PipBox:begin
					for Col:=Low(pcPipType) to High(pcPipType) do
						PutBitmap(hDC,x_engine.PipIcon(Black_Suit,Col),
							Centered(GetBitmapWd(x_engine.PipIcon(Black_Suit,Col)),Ord(Col)*wd,Ord(Col)*wd+wd),
							0,
							SrcCopy);
				end;
				id_SuitBox:begin
					for Row:=Low(pcSuitType) to High(pcSuitType) do
						PutBitmap(hDC,x_engine.SuitIcon(Row),
							Centered(GetBitmapWd(x_engine.SuitIcon(Row)),0,rcItem.Right),
							Centered(GetBitmapHt(x_engine.SuitIcon(Row)),Ord(Row)*ht,Ord(Row)*ht+ht),
							SrcCopy);
				end;
				id_CustomBox:begin
					Wd:=(RectWd(rcItem) div 13);
					Ht:=RectHt(rcItem) div 4;
					if (ItemAction and oda_DrawEntire)<>0 then begin
						{ draw the whole table }
						FillRect(hDC,rcItem,GetStockObject(White_Brush));
						FrameRect(hDC,rcItem,GetStockObject(Black_Brush));
						for Row:=ClubSuit to SpadeSuit do begin
							MoveTo(hDC,0,ht*Ord(Row));
							LineTo(hDC,rcItem.Right,ht*Ord(Row));
							for Col:=AcePip to KingPip do begin
								MoveTo(hDC,wd*Ord(Col),0);
								LineTo(hDC,wd*Ord(Col),rcItem.Bottom);
								SetState(Col,Row);
							end;
						end;
					end;
					if (ItemAction and oda_Select)<>0 then if (ItemState and ods_Selected)<>0 then begin
						for Row:=Low(pcSuitType) to High(pcSuitType) do
							for Col:=Low(pcPipType) to High(pcPipType) do begin
								with r do begin
									Left:=Ord(Col)*Wd+1;
									Top:=Ord(Row)*Ht+1;
									Right:=Left+Wd-1;
									Bottom:=Top+Ht-1;
								end;
								GetCursorPos(pt);
								ScreenToClient(GetItemHandle(id_CustomBox),pt);
								if PtInRect(r,pt) then begin
									Toggle(TmpWilds.wList[Col,Row]);
									SetState(Col,Row);
								end;
							end;
					end;
				end;
			end;
		end;
		Msg.Result:=1;
	end;

procedure StudOptionsDlg.IDLimits;

	var
		i, j:integer;

	begin
		if Application^.ExecDialog(New(PStudLimitsDlg,Init(@Self)))=idOK then
		with x_aStudLimitsDlgTransferBuffer,x_aStudPokerOptions do begin

			for i:=1 to StudPLib.MAX_BETTING_ROUNDS do begin
				j:= 1;
				while (bRound[i, j] = 0) do inc(j);
				case j of
					1:m_aRoundBettingLimits.MinOpen[i]:= 1;
					2:m_aRoundBettingLimits.MinOpen[i]:= 5;
					3:m_aRoundBettingLimits.MinOpen[i]:= 10;
				end;
				m_aRoundBettingLimits.MaxOpen[i]:= m_aRoundBettingLimits.MinOpen[i] * 5;
			end;

			with RaiseLmts do begin
				if s2w(m_aRaiseTotalPerRound) >= 0 then LmtPerRnd:= minw(RLM_PERROUND, s2w(m_aRaiseTotalPerRound));
				if s2w(tpp) >= 0 then LmtPerPsn:= minw(RLM_PERPERSON, s2w(tpp));
				if s2w(trf) > 0 then RaiseFactor:= minw(RLM_FACTOR, s2w(trf));
			end;
		end;
	end;

procedure StudOptionsDlg.IDWildcards;

	begin
		wGame:=StudPoker;
		if Application^.ExecDialog(New(PWildDlg,Init(@Self)))=idOK then
			with x_aStudPokerOptions,WildData do begin
				wType:=Low(WildType);
				while Choice[wType]=0 do wType:=Succ(wType);
			end;
	end;

{$endif INACTIVE}

{$ifdef TEST}

procedure Test_Unit;

begin
	{ edit box is big enough to enter the range of values }
	Assert.Equal(ANTE_EDITBOX_TEXTLEN, Round(Ln(ANTE_EDITBOX_MAX)/2.303) + 1);
end;

{$endif}

{$ifdef INACTIVE}

function CmpHData(var a;var b):integer;
	begin
		if HDataRec(a).OData > HDataRec(b).OData then
			CmpHData:= -1
		else if HDataRec(a).OData < HDataRec(b).OData then
			CmpHData:= +1
		else
			CmpHData:= 0;
	end;

procedure DspOddstable(adc:hDC;aRect:TRect);

	{	Displays the standard poker hands sorted with the most likely
		hand at the top and the rest in descending order of odds of
		getting.

		Since the odds of getting each hand changes depending on the
		wild cards in play and the scoring Mode (HIGH or LOW) the odds
		are generated dynamically during the idle state. To generate
		accurate odds would require the evaluation of every possible hand
		from the deck which is not feasable so the odds are generated
		in increments during the idle cycle. }

	var
		i:integer;
		h:handrank;
		widest:integer;
		oddsStr:string[10];
		table:TRectangle; { hands table inside the panel }
		HData:array[1..ord(FIVE_KIND)+1] of HDataRec; { sorted hand odds }
		y:integer;
		doMoreHands:boolean;

	begin
		TGI_DC:= adc;
		{ find the widest description }
		widest:=0;
		for h:=RUNT to maxHandType do
			widest:=max(widest, tgi_stringWd(pokerHandDesc[h]));
		widest:=widest+ tgi_stringWd('99999999/1');
		{ initialize the hand table window }
		Table.init(GetRectWd(aRect),GetRectHt(aRect));
		with Table,aRect do begin
			SetX(Left);
			SetY(Top);
		end;
		tgi_setTextColor(0);
		tgi_setColor(8);
		y:=1;
		{ assign the current odds data to this temporary array and sort it }
		for h:=RUNT to maxHandType do with HData[ord(h)+1] do begin
			HType:=h;
			OData:=hndOddsTbl[h];
		end;
		sort(HData,ord(maxHandType)+1,SizeOf(HDataRec),CmpHData);
		for i:=1 to ord(maxHandType)+1 do begin
			if ((i+1) mod 2)<>0 then tgi_fillBlock(1,y,table.width-2,tgi_upcaseHt+2);
			tgi_setTextJustify(justify_left);
			tgi_putStringXY(2,y+1,pokerHandDesc[HData[i].HType]);
			tgi_setTextJustify(justify_right);
			if HData[i].OData=0 then
				oddsStr:='---'
			else
				oddsStr:=long2str(totalHndCnt div HData[i].OData);
			tgi_putStringXY(GetRectWd(aRect)-3,y+1,oddsStr+'/1');
			y:=y+tgi_upcaseHt;
		end;
	end;

const
	id_OddsTab=101;
	id_More=102;

type
	POddsDlg=^TOddsDlg;
	TOddsDlg=object(TDialog)
		procedure WMDrawItem(var Msg:TMessage); virtual WM_FIRST+wm_DrawItem;
		procedure IDMore(var Msg:TMessage); virtual id_First+id_More;
	end;

procedure TOddsDlg.IDMore;

	var
		pCursor:hCursor;
	begin
		pCursor:=SetCursor(LoadCursor(0,idc_Wait));
		IncOddsTable(InitOddsGen);
		SetCursor(pCursor);
		InvalidateRect(GetItemHandle(id_OddsTab),nil,True);
		InvalidateRect(GetItemHandle(103),nil,True);
		Updatewindow(handle);
	end;

procedure TOddsDlg.WMDrawItem;

	type
		PDrawItemStruct=^TDrawItemStruct;
	var
		pstr:array[0..40] of Char;
	begin
		with PDrawItemStruct(Msg.lParam)^ do
			case Msg.wParam of
				id_OddsTab:begin
					wvsprintf(pstr,'(from %ld hands)',TotalHndCnt);
					SetWindowText(GetItemHandle(103),pstr);
					FrameRect(hDC,rcItem,GetStockObject(Black_Brush));
					DspOddsTable(hDC, rcItem);
				end;
			end;
		Msg.Result:=1;
	end;

function AnyChange:boolean;

	{ Return true if any of the variables that effect the odds have
		changed since the last time we called this procedure. }

	begin
		AnyChange:=(
			(not MemCmp(_wilds_,cgWilds,SizeOf(WildCardList)))
			or
			(HandSize<>cgHandSize)
			);
	end;

procedure TPkrFrm.CMOdds;

	var
		pCursor:hCursor;

	begin
		if (TotalHndCnt=0) or AnyChange then begin
			SetupForOdds;
			ClearOddsTable;
			pCursor:=SetCursor(LoadCursor(0, idc_Wait));
			buildOddsTable(initOddsGen);
			SetCursor(pCursor);
		end;
		Application^.ExecDialog(New(POddsDlg, Init(thePkrFrm, 'Odds')));
	end;

function sittingFromDlr(po:PlayerId):integer;
	{ Return the distance (1.."MAX_PLAYERS") that player # "po" is sitting
		from the theDealerIndex. }
	var
		d:integer;
		pn:PlayerId;
	begin
		d:=1;
		pn:=NextClockwiseFrom(theDealerIndex);
		while pn<>po do begin
			inc(d);
			pn:=NextClockwiseFrom(pn);
		end;
		sittingFromDlr:=d;
	end;

procedure DoPokerHist(adc:hDC;aRect:TRect);

	var
		table:TRectangle; { hands table inside the panel }
		i,x,y,nLines:integer;
		cellWd,cellHt:integer;
		s:string[255];
		pn:PlayerId;
		action:string[10];
		IconHt:integer; { height of the tallest icon }
		name:array[0..40] of Char;

	begin
		TGI_DC:=adc;
		s:=rndHistory;
		{ calculate the optimal width of the interior of each cell }
		cellWd:=GetRectWd(aRect) div 8;
		cellHt:=1+tgi_upcaseHt;
		IconHt:=50; { space reserved for the icons }
		{ how many lines in the table }
		nLines:=1;
		for i:=1 to length(rndHistory) do if rndHistory[i]='$' then inc(nLines);
		table.init(GetRectWd(aRect),GetRectHt(aRect)-IconHt-DevFontHt(adc));
		TGI_SetColor(0);
		SetTextAlign(adc,ta_top or ta_Center);
		with table do begin
			setX(0);
			setY(IconHt+DevFontHt(adc));
			TGI_LineH(attr.x,attr.y,Width);
			{ display the icons with the theDealerIndex on the right and add the col/row dividers }
			tgi_setTextJustify(justify_center);
			{for i:=1 to nLines-1 do tgi_lineH(0,attr.y+i*(cellHt+1),width);}
			pn:=theDealerIndex;
			x:=0;
			repeat
				pn:=NextClockwiseFrom(pn);
				with group[pn] do begin
						StrPCopy(Name,GetNickname);
						TextOut(adc,x+(CellWd shr 1),IconHt-1,Name,StrLen(Name));
					end;
				x:=x+cellWd+1;
				tgi_lineV(x,attr.y,height);
			until pn=theDealerIndex;
		end;
		tgi_setTextColor(0);
		with table do begin
			x:=0;
			y:=attr.y+2;
			while s<>'' do begin
				case s[1] of
					'1'..'8':begin
						i:=sittingFromDlr(str2int(s[1]));
						delete(s,1,pos(':',s));
						case s[1] of
							'P':action:='Pass';
							'S':action:='Stand';
							'O':action:='Open';
							'F':action:='Fold';
							'R':action:='Raise';
							'C':action:='Call';
							'K':action:='Check';
							'B':action:='Bet';
							'D':action:=copy(s,1,pos(';',s)-1);
						end;
						StrPCopy(Name,Action);
						TextOut(adc,(i-1)*(1+cellWd)+((1+cellWd) shr 1),y,Name,StrLen(Name));
					end;
					'$':begin
						x:=0;
						y:=y+cellHt;
					end;
				end;
				delete(s,1,pos(';',s));
			end;
		end;
	end;

type
	PHistoryDlg=^THistoryDlg;
	THistoryDlg=object(TDialog)
		constructor Init(aParent:PWindowsObject);
		procedure WMDrawItem(var Msg:TMessage); virtual WM_FIRST+wm_DrawItem;
	end;

constructor THistoryDlg.Init(aParent:PWindowsObject);

begin
	inherited Init(aParent, 'History');
end;

procedure THistoryDlg.WMDrawItem;

const
	id_HistTab=101;
type
	PDrawItemStruct=^TDrawItemStruct;
begin
	with PDrawItemStruct(Msg.lParam)^ do
		case Msg.wParam of
			id_HistTab:begin
				FrameRect(hDC,rcItem,GetStockObject(Black_Brush));
				DoPokerHist(hDC,rcItem);
			end;
		end;
	Msg.Result:=1;
end;

procedure TPkrFrm.CMHistory;

begin
	Application^.ExecDialog(New(PHistoryDlg, Init));
end;

{$endif INACTIVE}

function DrawOptionsDlg.DoVariationSelection(aVariation:drawPokerVar):boolean;

begin
	SetupDrawTransferBuffer(aVariation,theDrawPokerOptions);
	DoVariationSelection:=TRUE;	
end;

function StudOptionsDlg.DoVariationSelection(aVariation:studPokerVar):boolean;

begin
	Setup(aVariation);
	DoVariationSelection:=TRUE;	
end;

function DrawOptionsDlg.ProcessVariationSelection(aControlId:UINT):boolean;

begin
	ProcessVariationSelection:=FALSE;
	case aControlId of
		ID_ANYTHING:ProcessVariationSelection:=DoVariationSelection(ANYTHINGOPENS);
		ID_JACKPOTS:ProcessVariationSelection:=DoVariationSelection(JACKPOTS);
	end;
end;

function StudOptionsDlg.ProcessVariationSelection(aControlId:UINT):boolean;

begin
	ProcessVariationSelection:=FALSE;
	case aControlId of
		ID_5CARD:ProcessVariationSelection:=DoVariationSelection(STUD5CARD);
		ID_6CARD:ProcessVariationSelection:=DoVariationSelection(STUD6CARD);
		ID_7CARD:ProcessVariationSelection:=DoVariationSelection(STUD7CARD);
	end;
end;

{$ifdef TEST}

type
	DrawOptionsDlgTest=object(DrawOptionsDlg)
		constructor Init;
	end;

constructor DrawOptionsDlgTest.Init; begin end;

procedure Test_Draw_ProcessVariationSelection;

const
	INVALID_ID=ID_ANYTHING-1;

var
	aDialog:DrawOptionsDlgTest;

begin
	aDialog.Init;
	punit.Assert.IsTrue(aDialog.ProcessVariationSelection(ID_ANYTHING));
	punit.Assert.IsTrue(aDialog.ProcessVariationSelection(ID_JACKPOTS));
	punit.Assert.IsFalse(aDialog.ProcessVariationSelection(INVALID_ID));
end;

type
	StudOptionsDlgTest=object(StudOptionsDlg)
		constructor Init;
	end;

constructor StudOptionsDlgTest.Init; begin end;

procedure Test_Stud_ProcessVariationSelection;

const
	INVALID_ID=ID_5CARD-1;

var
	aDialog:StudOptionsDlgTest;

begin
	aDialog.Init;
	punit.Assert.IsTrue(aDialog.ProcessVariationSelection(ID_5CARD));
	punit.Assert.IsTrue(aDialog.ProcessVariationSelection(ID_5CARD+1));
	punit.Assert.IsTrue(aDialog.ProcessVariationSelection(ID_5CARD+2));
	punit.Assert.IsFalse(aDialog.ProcessVariationSelection(INVALID_ID));
end;

{$endif}

function DrawOptionsDlg.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	case aMsg of
		WM_COMMAND: if ProcessVariationSelection(LOWORD(wParam)) then begin
			OnMsg:=0;
			Exit; 
		end;
	end;
	OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
end;

function StudOptionsDlg.OnMsg(aMsg:UINT;wParam:WPARAM;lParam:LPARAM):LONG;

begin
	case aMsg of
		WM_COMMAND: if ProcessVariationSelection(LOWORD(wParam)) then begin
			OnMsg:=0;
			Exit; 
		end;
	end;
	OnMsg:=inherited OnMsg(aMsg,wParam,lParam);
end;

{$ifdef TEST}
begin
	Suite.Add(@Test_Unit);
	Suite.Add(@Test_Draw_ProcessVariationSelection);
	Suite.Add(@Test_Stud_ProcessVariationSelection);
	Suite.Run('qpdlgs');
{$endif}
end.
