{
 ****************************************************************************
 *                                  SimDig                                  *
 *                      Copyright (c) 1992 by Mike Hesser                   *
 *                        mîgen die GATES mit dir sein!                     *
 ****************************************************************************
 * 04.07.92 *
}

{$M 16384,0,655360}
{$IFDEF DEVELOP}
  {$A+,B-,D+,E-,F-,G+,I+,L+,N+,O-,Q+,R+,S+,V+,X+}
{$ELSE}
  {$A+,B-,D-,E-,F-,G+,I-,L-,N-,O-,Q-,R-,S-,V-,X+}
{$ENDIF}

PROGRAM Simdig;
USES Dos,Crt,Graph;

CONST GX = 12; GY = 12; AX = 5; AY = 3; AX2 = 2*AX; AY2 = 2*AY;
      GXH = GX DIV 2; GYH = GY DIV 2; GMODE = VGAHI;
      GOX = 580; GOY = 5; RX = 47; RY = 38; STATUSY = 20; (* Abstand von unten *)
      ANPL = 128; (* Bit 7 *)
      MOUSELEFT = 1; MOUSERIGHT = 2; CHARHEIGHT = 16;
      MX = 74; MY = 6; MAXGATET = 11; MH = MAXGATET;         (* fÅr Gates *)
      GMO = (MY-1) * CHARHEIGHT; GMH = (MH+1)*CHARHEIGHT;
      MBY = 21; MAXMENP = 7; MBH = MAXMENP;        (* fÅr MenÅ *)
      MO = (MBY-1) * CHARHEIGHT; MMH = (MBH+1)*CHARHEIGHT;
      MAXGATES = 40; MAXCON = 100;   (* Dimensionierung *)
      MAXNR = 100; MAXINF = MAXCON;
      PLACEF = 0; LINEF = 1; SIMF = 2; ZIPF = 3;
      REDON = 3; HREDON = 4; DREDON = 5; REDOFF = 7;
      HKEN = 32; VKEN = 64; MAXPINS = 7; FREQ = 1200;
      GRAY = 13; MAXDATEIEN = 8; ATTR = $100;
      BACKC = BLACK;
      TRACE = FALSE;
      ErrorMess: ARRAY[2..3] OF STRING[15] = ('Dateifehler','Keine SigDatei');
      SEG7DAT: ARRAY[0..15] OF BYTE =
          (126,48,109,121,51,91,95,112,127,123,119,31,78,61,79,71);
      ANNR = 0; ORNR = 1; NONR = 2; NANR = 3; TFNR = 4; JKNR = 5; VANR = 6;
      MYNR = 7; LANR = 8; SWNR = 9; S7NR = 10; CKNR = 11;

TYPE  BezTyp = STRING[2];
        VTyp = BYTE;
    DListTyp = STRING[8+4];   (* Name+Suffix *)
    DateiTyp = TEXT;
       RKTyp = BYTE;
    TitleTyp = STRING[6];
     PathTyp = STRING[20];

(* Records *)

    GATETTYP = RECORD
                 name: STRING[10];
                  b,h: BYTE;
                   ap: BYTE;               (* Anzahl AnschlÅsse *)
               END;
     GATETYP = RECORD
                  x,y: BYTE;
                  typ: BYTE;
                 zust: BYTE;               (* Zustand *)
                    p: ARRAY[1..MAXPINS] OF BYTE;
               END;

      CONTYP = RECORD
                  x,y: BYTE;             (* Koordinaten *)
                   le: SHORTINT;         (* LÑnge + Richtung *)
                p1,p2: BYTE;             (* Zeiger auf nÑchste Leitung *)
                point: BYTE;             (* Zeiger auf INFO-TAB *)
               END;

VAR MATRIX: ARRAY[0..RX+1,0..RY] OF BYTE;
     GATET: ARRAY[0..MAXGATET] OF GATETTYP;
      GATE: ARRAY[1..MAXGATES] OF GATETYP;
       CON: ARRAY[1..MAXCON] OF CONTYP;
     INFOT: ARRAY[1..MAXINF] OF BOOLEAN;
      MENU: ARRAY[0..MAXMENP] OF STRING[10];
     DList: ARRAY[1..MAXDATEIEN] OF DListTyp;
    Path: PathTyp;
    Datei: DateiTyp;
    FCB: SearchRec;
    DName: DListTyp;
    ECode: BYTE;  (* Error Code bei FileRequester *)
    i,j: INTEGER;
    gm,gd: INTEGER;
    x,y,k: INTEGER;
    xo,yo,xxr,yyr,px,py,xr,yr: RKTYP;
    br,ho: BYTE;
    SCRMAXX,SCRMAXY,GRX,GRY,rastx,rasty: INTEGER;
    ConP,GatP: INTEGER; (* Zeiger *)
    Size,SizeL: WORD;
    Obj,ObjLON,ObjLOFF: POINTER;
    Typ,MFlag,mi: BYTE;
    MBFlag,EFlag,FFlag,PFlag,HVFlag,QuitF: BOOLEAN;
    Reg: REGISTERS;
    lfd,mp,gp,opm,opg: BYTE;
    pu1,pu2: BYTE;

(*                          ØØØØ Mouse Pack ÆÆÆÆ                            *)

PROCEDURE MouseOn;
BEGIN
  Reg.AX:= $0001;
  Intr($33,Reg);
END;

PROCEDURE MouseOff;
BEGIN
  Reg.AX:= $0002;
  Intr($33,Reg);
END;

PROCEDURE GetMouseXY(VAR x,y,k: INTEGER);
BEGIN
  Reg.AX:= $0003;
  Intr($33,Reg);
  x:= Reg.CX;
  y:= Reg.DX;
  k:= Reg.BX;
END;

PROCEDURE MoveMouse(x,y: INTEGER);
BEGIN
  Reg.AX:= $4; Reg.CX:= x; Reg.DX:= y;
  Intr($33,Reg);
END;

PROCEDURE SetRGB(nr,r,g,b: BYTE);
BEGIN
  Port[$3c8]:= nr;
  Inline($FA);
  Port[$3c9]:= r;
  Port[$3c9]:= g;
  Port[$3c9]:= b;
  Inline($FB);
END;

PROCEDURE LAMP(x,y: INTEGER; onoff: BOOLEAN);
VAR off:BYTE;
BEGIN
  IF onoff THEN off:= redon ELSE off:= redoff;
  SetFillStyle(1,off); SetColor(off);
  FillEllipse(x+GXH,y+GYH,GXH-1,GYH-1);
  SetColor(HREDON);
  MoveTo(x+1,y+6); LineRel(0,-2); LineRel(3,-3); LineRel(3,0);
  SetColor(WHITE);
  MoveTo(x+3,y+6); LineRel(0,-2); LineRel(2,-2);
  SetColor(DREDON);
  MoveTo(x+4,y+11); LineRel(4,0); LineRel(3,-3); LineRel(0,-4);
END;

{--------------------------}
PROCEDURE Resta(x,y: RKTYP); (* 130892 *)   (* alte Stelle restaurieren *)
{--------------------------}
BEGIN
  SetWriteMode(NORMALPUT);
  IF MATRIX[x,y]=VKEN THEN
    BEGIN
      MoveTo(x*GX+GXH,y*GY+GYH-1); LineRel(0,3);
  END;
  IF MATRIX[x,y]=HKEN THEN
    BEGIN
      MoveTo(x*GX+GXH-1,y*GY+GYH); LineRel(2,0);
  END;
  SetColor(2); SetLineStyle(1,1,1);
  IF x MOD 6 = 0 THEN BEGIN MoveTo(x*GX,y*GY); LineRel(0,GY); END;
  IF y MOD 6 = 0 THEN BEGIN MoveTo(x*GX-1,y*GY); LineRel(GX+3,0); END;
  SetColor(WHITE); SetLineStyle(0,0,0);
END;

{--------------------------------------------------}
PROCEDURE FillMatrix(x,y,b,h: RKTYP; v: BYTE; res:BOOLEAN);  (*210892*)
{--------------------------------------------------}
BEGIN
  FOR j:= y TO PRED(y+h) DO      (* Matrix mit 255 fÅllen *)
    FOR i:= x TO PRED(x+b) DO
      BEGIN
        MATRIX[i,j]:= v;            (* Kennung fÅr Gate *)
        IF res THEN Resta(i,j);
  END;
END;

{----------------}
PROCEDURE Init;
{----------------}
VAR error: INTEGER;

BEGIN
  gd:= VGA; gm:= GMODE;
  InitGraph(gd,gm,'c:\language\bp\bgi');
  error:= GraphResult;
  IF error<>grok THEN
    BEGIN
      writeln('Graphics-Error: ',GraphErrorMsg(error));
      readln; Halt(1);
  END;

  DirectVideo:= FALSE;         (* Crt+BIOS *)
  WindMax:= 256*31+79;         (* Bildschirmgrenzen anpassen *)

  (* Farben *)

  SetRGB(2,17,17,17);
  SetRGB(REDON,60,10,10);
  SetRGB(HREDON,50,20,20);
  SetRGB(DREDON,30,4,4);
  SetRGB(REDOFF,27,2,2);
  SetRGB(GRAY,30,30,30);
  SetRGB(59,0,20,0);

  SCRMAXX:= GetMaxX;
  SCRMAXY:= GetMaxY;
  GRX:= 570; GRY:= 10;
  rastx:= RX;
  rasty:= RY;
  FillMatrix(0,0,RX,RY,0,FALSE);
  WITH GATET[ANNR] DO BEGIN name:= 'AND'; b:= 3; h:= 3; ap:= 3; END;
  WITH GATET[ORNR] DO BEGIN name:= 'OR';  b:= 3; h:= 3; ap:= 3; END;
  WITH GATET[NONR] DO BEGIN name:= 'NOT'; b:= 3; h:= 3; ap:= 2; END;
  WITH GATET[NANR] DO BEGIN name:= 'NAND'; b:= 3; h:= 3; ap:= 3; END;
  WITH GATET[TFNR] DO BEGIN name:= 'T-FF'; b:= 5; h:= 3; ap:= 4; END;
  WITH GATET[JKNR] DO BEGIN name:= 'JK-MS'; b:= 4; h:= 5; ap:= 7; END;
  WITH GATET[VANR] DO BEGIN name:= 'V-ADD'; b:= 4; h:= 3; ap:= 5; END;
  WITH GATET[MYNR] DO BEGIN name:= 'MAYBE'; b:= 3; h:= 3; ap:= 1; END;
  WITH GATET[LANR] DO BEGIN name:= 'LED'; b:= 2; h:= 1; ap:= 1; END;
  WITH GATET[SWNR] DO BEGIN name:= 'Switch'; b:= 3; h:= 1; ap:= 1; END;
  WITH GATET[S7NR] DO BEGIN name:= 'Seg7'; b:= 4; h:= 4; ap:= 4; END;
  WITH GATET[CKNR] DO BEGIN name:= 'Clock'; b:= 3; h:= 2; ap:= 1; END;

  MENU[0]:= 'Draw'; MENU[1]:= 'Gates'; MENU[2]:= 'Zip';
  MENU[3]:= 'Load'; MENU[4]:= 'Save'; MENU[5]:= 'Simul.';
  MENU[6]:= 'New'; MENU[7]:= 'Quit';

  Path:= 'C:\*.*';
  Reg.AX:=$21; Intr($33,Reg); (* Reset des Maustreibers *)

  Size:= ImageSize(GOX,GOY,GOX+(4*GX),GOY+(4*GY));
  SizeL:= ImageSize(GOX,GOY,GOX+GX,GOY+GY);
  GetMem(Obj,Size);
  GetMem(ObjLON,SizeL);
  GetMem(ObjLOFF,SizeL);
  LAMP(GOX,GOY,FALSE); GetImage(GOX,GOY,GOX+GX,GOY+GY,ObjLON^);
  LAMP(GOX,GOY,TRUE);  GetImage(GOX,GOY,GOX+GX,GOY+GY,ObjLOFF^);
  ClearDevice;
  ConP:= 1; lfd:= 0; (* Verwaltung fÅr Verbindungsleitungen *)
  GatP:= 1;          (* Verwaltung fÅr Gates *)
  pu2:= 0;
  opm:= 0; opg:= 0;
  PFlag:= FALSE;
  MFlag:= LINEF;
END;

PROCEDURE CloseAll;
BEGIN
  FreeMem(Obj,Size);
  FreeMem(ObjLON,SizeL);
  FreeMem(ObjLOFF,SizeL);
  CloseGraph;
END;

PROCEDURE Title;
BEGIN
  SetColor(LightBlue);
  SetTextStyle(TriplexFont,0,8);
  OutTextXY(180,20,'SimDig');
  SetTextStyle(TriplexFont,0,4);
  OutTextXY(20,120,'v1.0 Copyright (C) 1992 by Mike Hesser');
  SetColor(REDON);
  Line(40,170,600,170);
  SetTextStyle(1,{SansSerifFont}0,4);
  SetFillStyle(1,11);
  Bar(0,190,640,479);
  SetColor(LIGHTGREEN);
  OutTextXY(140,200,'GREENGATE informiert:');
  SetTextStyle(1,0,2);
  OutTextXY(10,250,'Mit SimDig brauchen Sie nicht mehr lÑnger echte GATES fÅr');
  OutTextXY(10,280,'Ihre Tests zu mi·brauchen. Dieses Programm ermîglicht es');
  OutTextXY(10,310,'Ihnen fast vollstÑndig auf Gateversuche zu verzichten und');
  OutTextXY(10,340,'damit einen wichtigen Beitrag zum Schutze der GATES zu');
  OutTextXY(10,370,'leisten.');

  SetTextStyle(1,0,4);
  OutTextXY(80,410,'DIE GATES BRAUCHEN UNS NICHT,');
  OutTextXY(80,440,'ABER WIR BRAUCHEN DIE GATES!');

  REPEAT UNTIL KeyPressed;
END;

FUNCTION Moved:BOOLEAN;
BEGIN
  Reg.AX:= $000b;
  Intr($33,Reg);
  Moved:= (Reg.CX+Reg.DX<>0);
END;

PROCEDURE DrawRaster;
BEGIN
  MouseOff;
  SetFillStyle(1,BACKC); Bar(1,1,GRX-1,SCRMAXY-STATUSY-1);
  SetColor(2); SetLineStyle(1,1,1);
  FOR i:= 1 TO 7 DO
    BEGIN
      MoveTo(i*(GX*6),0);
      LineRel(0,SCRMAXY-STATUSY-1);
  END;
  FOR i:= 1 TO 6 DO
    BEGIN
      MoveTo(0,i*(GY*6));
      LineRel(GRX,0);
  END;
  MouseOn;
END;

PROCEDURE View;
BEGIN
  ClearDevice;
  SetLineStyle(0,0,0);
  SetColor(LIGHTBLUE);
  Rectangle(0,0,SCRMAXX,SCRMAXY-STATUSY);
  Line(GRX,0,GRX,SCRMAXY-STATUSY);
  Line(GRX,GMO-2,SCRMAXX,GMO-2);
  Line(GRX,MO-2,SCRMAXX,MO-2);
{  SetViewPort(1,1,SCRMAXX-1,SCRMAXY-1,CLIPON);}
  TextColor(YELLOW);
  GotoXY(35,30); writeln('SimDig Copyright (c) by Mike Hesser TG13');
  TextColor(LIGHTBLUE);
  FOR i:= 0 TO MH DO
    BEGIN
      GotoXY(MX,MY+i);
      writeln(GATET[i].name);
  END;
  FOR i:= 0 TO MAXMENP DO
    BEGIN
      GotoXY(MX,MBY+i);
      writeln(MENU[i]);
  END;
END;

PROCEDURE ShowEMess(Code:BYTE);
BEGIN
  TextColor(YELLOW);
  GotoXY(35,30); writeln('SimDig Copyright (c) by Mike Hesser TG13');
  TextColor(REDON);
  IF Code>1 THEN
    BEGIN
      GotoXY(35,30);
      writeln('Fehler: ',ErrorMess[Code],'         ');
  END;
END;

PROCEDURE Statusline;
BEGIN
  TextColor(YELLOW);
  GotoXY(2,30); writeln('X: ',xr:2);
  GotoXY(10,30); writeln('Y: ',yr:2);
{  GotoXY(20,30); writeln('Matrix: ',MATRIX[xr,yr]:3);}

{  gotoxy(73,24); writeln('pu1',pu1:2);
  gotoxy(73,25); writeln('pu2',pu2:2);}

  IF TRACE THEN
  BEGIN
  for i:= 1 to conp-1 do
  begin
  gotoxy(2+i*3,22); writeln(CON[i].point:2);
{  gotoxy(60,4+i);
  writeln(i:2,':',CON[i].p1:2,',',CON[i].p2:2,';',CON[i].point:1);}
  end;
  END;
END;

(*                          ØØØØ Bauteile zeichnen ÆÆÆÆ                      *)

PROCEDURE DLAMP(x,y: INTEGER; onoff: BOOLEAN);
BEGIN
  IF onoff THEN PutImage(x,y,ObjLOFF^,NORMALPUT) ELSE
                PutImage(x,y,ObjLON^,NORMALPUT);
END;

PROCEDURE DSWITCHOFF(x,y: INTEGER);
BEGIN
  SetColor(WHITE);
  MoveTo(x+9,y+3); LineRel(GX-1,GYH-3); LineRel(GX-9,0);
  SetTextStyle(2,0,4); OutTextXY(x+1,y,'O');
END;

PROCEDURE DSWITCHON(x,y: INTEGER);
BEGIN
  SetColor(WHITE);
  MoveTo(x+9,y+GY-3); LineRel(GX-1,-GYH+3); LineRel(GX-9,0);
  SetTextStyle(2,0,4); OutTextXY(x+2,y,#179);
END;

{-----------------------------------------}
PROCEDURE DSEG7D(x,y: INTEGER; z:BYTE);  (* 140892 *)
{-----------------------------------------}
VAR c: BOOLEAN;

PROCEDURE DSCOL;
BEGIN
  IF c THEN SetColor(REDON) ELSE SetColor(BACKC);
END;

PROCEDURE DS7LR(xr,yr: BYTE; d: SHORTINT);
BEGIN
  DSCOL;
  MoveTo(x+xr,y+yr); LineRel(0,15); MoveRel(d,-1); LineRel(0,-13);
  MoveRel(d,1); LineRel(0,11);
END;

PROCEDURE DS7OU(xr,yr: BYTE; d: SHORTINT);
BEGIN
  DSCOL;
  MoveTo(x+xr,y+yr); LineRel(12,0); MoveRel(-1,d); LineRel(-10,0);
  MoveRel(1,d); LineRel(8,0);
END;

BEGIN
  c:= (z AND 64)>0; DS7OU(12,7,1);
  c:= (z AND 32)>0; DS7LR(26,8,-1);
  c:= (z AND 16)>0; DS7LR(26,25,-1);
  c:= (z AND 8)>0;  DS7OU(12,41,-1);
  c:= (z AND 4)>0;  DS7LR(10,25,1);
  c:= (z AND 2)>0;  DS7LR(10,8,1);
  c:= (z AND 1)>0;  DSCOL;
  MoveTo(x+13,y+23); LineRel(10,0); MoveRel(1,1); LineRel(-12,0);
  MoveRel(1,1); LineRel(10,0);
END;

PROCEDURE DGATE(VAR x,y: RKTYP; mode:WORD);
BEGIN
  PutImage(x*GX+GXH,y*GY,Obj^,mode);
END;

{-------------------------------}
FUNCTION SearchCon(xx,yy: RKTYP):BYTE; (* 010892 *)
{-------------------------------}
VAR i: INTEGER;
    QF: BOOLEAN;

BEGIN
  i:= 0; QF:= FALSE;
  WHILE (i<ConP) AND (QF=FALSE) DO
    BEGIN
      Inc(i);
      WITH CON[i] DO
        BEGIN
          IF le<0 THEN
            BEGIN
              IF yy=y THEN
                IF (xx>=x) AND (xx<=x-le) THEN   (* - weil le negativ! *)
                  QF:= TRUE;
            END
          ELSE
            BEGIN
              IF xx=x THEN
                IF (yy>=y) AND (yy<=y+le) THEN
                  QF:= TRUE;
          END;
      END;
  END;
  IF QF=FALSE THEN i:=0;  (* SearchCon=0 -> *)
  SearchCon:= i;
END;

{--------------------------------------------------------------------}
PROCEDURE CPoints(VAR x,y: RKTYP; VAR typ:BYTE; VAR GateNr: INTEGER);
{--------------------------------------------------------------------}

PROCEDURE SetP(x,y: RKTYP;nr:BYTE);
VAR i: BYTE;

BEGIN
  IF (MATRIX[x,y] AND ANPL)<>0 THEN
    BEGIN
      IF MATRIX[x,y]=255 THEN MATRIX[x,y]:= 0;
      IF MFlag<>ZIPF THEN
        BEGIN
          i:= SearchCon(x,y);
          IF i<>0 THEN GATE[GateNr].p[nr]:= i;
      END;
  END;
  MATRIX[x,y]:= MATRIX[x,y] XOR nr;
END;

PROCEDURE C3;
BEGIN
  SetP(x,y,1);
  SetP(x,y+2,3);
  SetP(x+2,y+1,2);
END;

PROCEDURE C2;
BEGIN
  SetP(x,y+1,1);
  SetP(x+2,y+1,2);
END;

BEGIN {CPoints}
  CASE typ OF
    ANNR,ORNR,NANR: C3;
    NONR: C2;
    JKNR: BEGIN
            SetP(x,y,1); SetP(x,y+1,2); SetP(x,y+2,3); SetP(x,y+3,4);
            SetP(x,y+4,5); SetP(x+3,y+1,6); SetP(x+3,y+3,7);
          END;
    TFNR: BEGIN
            SetP(x,y+1,1); SetP(x+4,y,2);
            SetP(x+4,y+2,3);
          END;
    MYNR: SetP(x+2,y+1,1);
    VANR: BEGIN
            SetP(x,y,1); SetP(x,y+1,2); SetP(x,y+2,3);
            SetP(x+3,y,4); SetP(x+3,y+2,5);
          END;
    LANR: SetP(x,y,1);
    SWNR: SetP(x+2,y,2);
    S7NR: BEGIN
            SetP(x,y,1); SetP(x,y+1,2);
            SetP(x,y+2,3); SetP(x,y+3,4);
          END;
    CKNR: SetP(x+2,y,2);
  END;
END;

{------------------}
FUNCTION LfdNr:BYTE;
{------------------}
BEGIN
  LfdNr:= lfd;
  Inc(lfd);
END;

{----------------------------------------------------}
PROCEDURE PlaceGate(VAR xr,yr:RKTYP; VAR t:BYTE);  (* Baustein setzen *)
{----------------------------------------------------}
VAR f: BYTE;

BEGIN
  f:= 0;
  FOR i:= 1 TO MAXPINS DO
    GATE[GatP].p[i]:= 0;
  FOR j:= yr TO PRED(yr+ho) DO
    FOR i:= xr TO xr+br DO
      f:= f OR MATRIX[i,j];
  IF ((f AND 31)=0) AND (GatP<=MAXGATES) THEN
    BEGIN
      DGATE(xr,yr,NORMALPUT);           (* Gate darstellen *)
      FillMatrix(xr,yr,GATET[t].b-1,GATET[t].h,255,FALSE);
      CPoints(xr,yr,t,GatP);
      WITH GATE[GatP] DO
        BEGIN
          x:= xr; y:= yr;               (* Koordinaten merken *)
          zust:= 0;
          typ:= t;
      END;
      Inc(GatP);
  END;
END;

{----------------------------------------}
PROCEDURE DrawGate(t: BYTE;x,y: INTEGER); (*060892*)
{----------------------------------------}

(* Einzelteile *)

PROCEDURE DRec(xr,yr: BYTE);
BEGIN
  MoveTo(x+AX,y+AY);
  LineRel(xr*GX-AX2,0); LineRel(0,yr*GY-AY2);
  LineRel(-(xr*GX-AX2),0); LineRel(0,-(yr*GY-AY2));
END;

PROCEDURE DIn(p: BYTE);
BEGIN
  MoveTo(x+1,y+(PRED(p)*GY)+GYH);
  LineRel(AX-1,0);
END;

PROCEDURE DIn2;
BEGIN
  DIn(1); DIn(3);
END;

PROCEDURE DAus(xr,yr: BYTE);
BEGIN
  MoveTo(x+(xr*GX)-AX+1,y+PRED(yr)*GY+GYH);
  LineRel(AX-2,0);
END;

PROCEDURE DTxt(xr,yr: SHORTINT; txt: BezTyp);
BEGIN
  SetTextStyle(2,0,4);
  MoveTo(x+AX+4+xr,y+AY+2+yr);
  OutText(txt);
END;

PROCEDURE DNaus(xr,yr:BYTE);
BEGIN
  Circle(x+(xr*GX)-AX+2,y+PRED(yr)*GY+GYH,2);
END;

PROCEDURE DRec23;
BEGIN
  DRec(2,3);
END;

PROCEDURE Dreieck(p: BYTE); (* p gibt Hîhe des Bausteins an *)
BEGIN
  MoveTo(x+AX,y+((p*GY) DIV 2)-4); LineRel(7,4); LineRel(-7,4);
END;

(* Gatter *)

PROCEDURE DAND;
BEGIN
  DRec23; DIn2; DAus(2,2); DTxt(0,0,'&');
END;

PROCEDURE DOR;
BEGIN
  DRec23; DIn2; DAus(2,2); DTxt(0,0,#242'1');
END;

PROCEDURE DNAND;
BEGIN
  DRec23; DIn2; DNaus(2,2); DTxt(0,0,'&');
END;

PROCEDURE DNOR;
BEGIN
  DRec23; DIn2; DNaus(2,2); DTxt(0,0,#242'1');
END;

PROCEDURE DNOT;
BEGIN
  DIn(2); Drec23; DNaus(2,2); DTxt(0,0,'1');
END;

PROCEDURE DMAYBE;
BEGIN
  DAus(2,2); Drec23; DTxt(0,0,'?');
END;

PROCEDURE DVADD;
BEGIN
  DRec(3,3); DIn2; Din(2); DAus(3,1); DAus(3,3);
  SetTextStyle(2,0,6);
  OutTextXY(x+AX+10,y+3,#228);
  SetTextStyle(2,0,4);
  DTxt(-1,3*GY-AY-17,'ci');
  DTxt(10,3*GY-AY-17,'co');
END;

PROCEDURE DJKMSFF;
BEGIN
  DRec(3,5); DIn(1); DIn(2); DIn(3); Din(4); Din(5);
  DAus(3,2); DNaus(3,4); Dreieck(5);
  SetLineStyle(3,1,1); MoveRel(16,-4); LineRel(3*GX-16-AX*2,0);
  SetLineStyle(0,0,0);
  DTxt(0,-1,'S'); DTxt(0,10,'J'); DTxt(9,19,'C'); DTxt(0,30,'K');
  DTxt(0,40,'R'); MoveTo(x+AX+16,y+AY+5); LineRel(5,0); LineRel(0,6);
  MoveTo(x+AX+16,y+AY+43); LineRel(5,0); LineRel(0,6);
END;

PROCEDURE DTFF;  (*150892*)
BEGIN
  DRec(4,3); DIn(2); DAus(4,1); DNAus(4,3);
  Dreieck(3);
  SetLineStyle(3,1,1); MoveRel(16,-4); LineRel(4*GX-16-AX*2,0);
  SetLineStyle(0,0,0); SetTextStyle(2,0,4);
  DTxt(6,7,'T');
END;

PROCEDURE DSEG7;
BEGIN
  Din(1); Din(2); Din(3); Din(4); DRec(3,4);
  DSEG7D(x,y,126);
END;

PROCEDURE DCLOCK;
BEGIN
  DRec(2,2); DAus(2,1);
  MoveTo(x+8,y+14); LineRel(2,0); LineRel(0,-4);
  LineRel(4,0); LineRel(0,4); LineRel(2,0);
END;

BEGIN {DrawGate}
  SetColor(WHITE); SetWriteMode(NORMALPUT);
  SetLineStyle(0,0,0); SetFillStyle(1,BACKC);
  Bar(x+1,y+1,x+PRED(GATET[t].b)*GX,y+GATET[t].h*GY-1);
  CASE t OF
    ANNR: DAND;
    ORNR: DOR;
    NONR: DNOT;
    NANR: DNAND;
    JKNR: DJKMSFF;
    TFNR: DTFF;
    MYNR: DMAYBE;
    VANR: DVADD;
    LANR: DLAMP(x,y,FALSE);
    SWNR: DSWITCHOFF(x,y);
    S7NR: DSEG7;
    CKNR: DCLOCK;
  END;
END;

{-----------------------------}
PROCEDURE ShowGate(nr: BYTE);
{-----------------------------}
BEGIN
  typ:= nr;
  SetFillStyle(0,BACKC);
  Bar(GRX+1,1,SCRMAXX-1,GMO-3);
  DrawGate(nr,GOX,GOY);
  br:= GATET[typ].b-1;
  ho:= GATET[typ].h;
  GetImage(GOX,GOY,GOX+(br*GX)-1,GOY+(ho*GY)-1,Obj^);
END;

PROCEDURE DrawLine(x,y: RKTYP);
BEGIN
  xxr:= x; yyr:= y;
  IF PFlag= TRUE THEN
    BEGIN
      HVFlag:= ABS(x-px) >= ABS(y-py);
      IF HVFlag THEN
        BEGIN
          Line((px*GX)+GXH,(py*GY)+GYH,(x*GX)+GXH,(py*GY)+GYH);
          yyr:= py;
        END
      ELSE
        BEGIN
          Line((px*GX)+GXH,(py*GY)+GYH,(px*GX)+GXH,(y*GY)+GYH);
          xxr:= px;
      END;
  END;
END;

PROCEDURE Cursor(x,y:RKTYP; mode:BYTE);
BEGIN
  MouseOff;
  SetColor(WHITE);
  IF MFlag = LINEF THEN
    BEGIN
      SetWriteMode(mode);
      DrawLine(x,y);
  END;
  IF MFlag = PLACEF THEN
    DGATE(x,y,XORPUT);
  MouseOn;
END;

{-----------------------------------}
FUNCTION SearchGate(x,y: RKTYP):BYTE; (* x,y mu· im Bereich des Gates liegen *)
{-----------------------------------}
VAR i: INTEGER;
    QF: BOOLEAN;

BEGIN
  i:= 1; QF:= FALSE;
  WHILE (i<GatP) AND (QF=FALSE) DO
    BEGIN
      IF (x>=GATE[i].x) AND (x<GATE[i].x+GATET[GATE[i].typ].b) THEN
        IF (y>= GATE[i].y) AND (y<GATE[i].y+GATET[GATE[i].typ].h) THEN
          QF:= TRUE;
      Inc(i);
  END;
  Dec(i);
  IF QF = FALSE THEN i:= 0;
  SearchGate:= i;
END;

(***************************************************************************)
(*                         Verbindungsleitung                              *)
(***************************************************************************)

{-----------------------------}
PROCEDURE LineMVH(x,y: RKTYP; d:SHORTINT);   (*270892*)
{-----------------------------}
VAR i: INTEGER;
    bit: BYTE;

BEGIN
  IF d<0 THEN bit:= HKEN ELSE bit:= VKEN;
  MATRIX[x,y]:= MATRIX[x,y] OR ANPL; (* Anschlusspunkt *)
  FOR i:=1 TO ABS(d) DO
    BEGIN
      MATRIX[x,y]:= MATRIX[x,y] OR bit;
      IF d<0 THEN Inc(x) ELSE Inc(y);
  END;
  MATRIX[x,y]:= MATRIX[x,y] OR ANPL OR bit;
END;

{----------------}
PROCEDURE CLine;
{----------------}
VAR h:BOOLEAN;
    xb,yb,xh,yh: RKTYP;
    d: SHORTINT;

{-----------------}
FUNCTION CVG:BOOLEAN;
{-----------------}
VAR i: BYTE;

BEGIN
  CVG:= TRUE;
  mi:= MATRIX[xxr,yyr];
  IF mi=0 THEN
    BEGIN
      i:= 0;
      CVG:= FALSE;
    END
  ELSE
    IF mi>9 THEN    (* Anschlu· an Kabel *)
      BEGIN
        i:= SearchCon(xxr,yyr);
        IF NOT ((mi>127) AND (mi<>HKEN+VKEN+ANPL) AND ((mi AND 15)=0)) THEN
          BEGIN
{           SetWriteMode(NORMALPUT);
            MoveTo(xxr*GX+GXH-1,yyr*GY+GYH-1);
            LineRel(2,0); LineRel(0,2);         (* Punkt zeichnen *)
            LineRel(-2,0); LineRel(0,-2);}
          END
        ELSE
          IF CON[i].p2= 0 THEN CON[i].p2:= ConP ELSE CON[i].p1:= ConP;
    END
      ELSE  (* Anschlu· Kabel an Gate *)
        BEGIN
          i:= SearchGate(xxr,yyr);
          GATE[i].p[mi]:= ConP;
          i:= 0;
  END;
  IF PFlag THEN pu2:= i ELSE pu1:= i;
END;

{----------------------}
FUNCTION LineC:BOOLEAN; (*290892 testet auf LinienÅberschneidung *)
{----------------------}
VAR i,c: BYTE;

BEGIN
  xh:= xb; yh:= yb;
  c:= 0;
  FOR i:= 1 TO ABS(d)-1 DO
    BEGIN
      IF d<0 THEN Inc(xh) ELSE Inc(yh);
      c:= c OR MATRIX[xh,yh];
  END;
  IF d<0 THEN LineC:= (c AND (HKEN+31))>0 ELSE LineC:= (c AND (VKEN+31))>0;
END;

BEGIN {CLine}
  IF (px=xr) AND (py=yr) THEN
    BEGIN
      PFlag:= FALSE;
      IF CON[pu1].p1=ConP THEN CON[pu1].p1:= 0 ELSE CON[pu1].p2:= 0;
      px:= 0; py:= 0;
    END
  ELSE
    BEGIN
      IF (ConP<=MAXCON) AND (MATRIX[xxr,yyr]<>255) THEN
        BEGIN
          IF NOT PFlag THEN
            BEGIN
              px:= xr; py:= yr; EFlag:= CVG;
            END
          ELSE
            BEGIN
              IF px>xr THEN xb:= xxr ELSE xb:= px;
              IF py>yr THEN yb:= yyr ELSE yb:= py;
              CON[ConP].x:= xb; CON[ConP].y:= yb;
              IF HVFlag=TRUE THEN              (* HVFlag = TRUE -> horiz. *)
                d:= -ABS(xr-px)                (* negativ fÅr Kennung *)
              ELSE
                d:= ABS(yr-py);
              IF NOT LineC THEN
                BEGIN
                  EFlag:= CVG;
                  Cursor(xr,yr,NORMALPUT);
                  LineMVH(xb,yb,d);  (* Linie in Matrix eintragen *)
                  CON[ConP].le:= d;
                  CON[ConP].p1:= pu1;
                  CON[ConP].p2:= pu2;
                  Inc(ConP); PFlag:= FALSE;
                  IF NOT EFlag THEN h:= CVG;
                  PFlag:= TRUE;
                  IF HVFlag THEN px:= xr ELSE py:= yr;
              END;
          END;

          IF PFlag AND EFlag THEN PFlag:= FALSE ELSE PFlag:= TRUE;
        END;
  END;
END;

{--------------------}
PROCEDURE DeletePart; (* 130892 *)
{--------------------}
VAR nr,i,j: INTEGER;
    xb,yb: RKTYP;
    CMASK,t: BYTE;

PROCEDURE Del1(VAR po:BYTE; nr:BYTE);
BEGIN
  IF po=nr THEN po:= 0;        (* "Zahlen" nachrÅcken *)
  IF po>nr THEN Dec(po);
END;

BEGIN
  nr:= SearchGate(xr,yr);
  IF nr>0 THEN
    BEGIN
      xb:= GATE[nr].x; yb:= GATE[nr].y; t:= GATE[nr].typ;
      SetFillStyle(1,BACKC);
      Bar(xb*GX+GXH+1,yb*GY,(xb+GATET[t].b)*GX-1-GXH,(yb+GATET[t].h)*GY-1);
      CPoints(xb,yb,t,GatP);
      FillMatrix(xb+1,yb,GATET[t].b-2,GATET[t].h,0,TRUE);
      FOR i:= nr TO PRED(GatP) DO
        GATE[i]:= GATE[i+1];  (* alle nachfolgenden Gates nachrÅcken *)
      Dec(GatP);
    END
  ELSE
    BEGIN
      nr:= SearchCon(xr,yr);
        IF nr>0 THEN
          BEGIN
            xb:= CON[nr].x; yb:= CON[nr].y; i:= CON[nr].le;
            SetFillStyle(1,BACKC);
            SetWriteMode(NORMALPUT); SetColor(BACKC);
            MoveTo(xb*GX+GXH,yb*GY+GYH);
            IF i<0 THEN
              BEGIN
                LineRel(-i*GX,0);
                CMASK:= HKEN;
              END
            ELSE
              BEGIN
                LineRel(0,i*GY);
                CMASK:= VKEN;
            END;
            SetColor(WHITE);

            FOR j:= 1 TO SUCC(ABS(i)) DO
              BEGIN
                MATRIX[xb,yb]:= MATRIX[xb,yb] XOR CMASK;
                IF (MATRIX[xb,yb] AND 224)= 128 THEN      (* unter 4 Bits weg *)
                   MATRIX[xb,yb]:= MATRIX[xb,yb] XOR 128;
                Resta(xb,yb);
                IF i<0 THEN Inc(xb) ELSE Inc(yb);
            END;
            FOR i:= nr TO PRED(ConP) DO
              CON[i]:= CON[i+1];
            Dec(ConP);
            FOR i:= 1 TO PRED(ConP) DO
              BEGIN
                Del1(CON[i].p1,nr); Del1(CON[i].p2,nr); (* Nummern anpassen *)
            END;
            FOR i:= 1 TO PRED(GatP) DO
              FOR j:= 1 TO MAXPINS DO
                Del1(GATE[i].p[j],nr);
        END;
  END;
END;

(***************************************************************************)
(*                         SIMULATIONSTEIL (310792)                        *)
(***************************************************************************)

{----------------------}
FUNCTION GI(nr: BYTE): BOOLEAN; (*210892*)  (* Lese Flag aus INFOTAB *)
{----------------------}
VAR a: BYTE;

BEGIN
  a:= GATE[i].p[nr];
  IF a=0 THEN
    GI:= FALSE
  ELSE
    GI:= INFOT[CON[a].point];
END;

{--------------------------------}
PROCEDURE SI(nr:BYTE; f:BOOLEAN);  (*210892*) (* Setze Flag in INFOTAB *)
{--------------------------------}
VAR a: BYTE;

BEGIN
  a:= GATE[i].p[nr];
  IF a<>0 THEN INFOT[CON[GATE[i].p[nr]].point]:= f;
END;

{-------------------------}
PROCEDURE Fuse(a,b: BYTE);
{-------------------------}
VAR i: INTEGER;
BEGIN
  FOR i:= 1 TO PRED(ConP) DO
    BEGIN
      IF CON[i].point = b THEN
        CON[i].point:= a
      ELSE
        IF CON[i].point>b THEN
          Dec(CON[i].point);
  END;{for}
END;

{-----------------------}
PROCEDURE BuildInfoTab; (*280792*)
{-----------------------}
VAR iz: INTEGER;
    n1,n2,h,p1,p2,p3: BYTE;

BEGIN
  iz:= 1;        (* lfd. Nr. fÅr INFO-TAB *)
  FOR i:= 1 TO PRED(ConP) DO
    BEGIN
      CON[i].point:= 0;
      INFOT[i]:= FALSE;
  END;

  FOR i:= 1 TO PRED(ConP) DO
    BEGIN
      n1:= CON[i].p1;
      n2:= CON[i].p2;
      IF n1=0 THEN p1:= 0 ELSE p1:= CON[n1].point;
      p2:= CON[i].point;
      IF n2=0 THEN p3:= 0 ELSE p3:= CON[n2].point;
      IF p1>p2 THEN BEGIN h:= p1; p1:= p2; p2:= h; END;  (* p1,p2,p3 der *)
      IF p2>p3 THEN BEGIN h:= p2; p2:= p3; p3:= h; END;  (* Grî·e nach *)
      IF p1>p2 THEN BEGIN h:= p1; p1:= p2; p2:= h; END;  (* sortieren *)

      IF p3=0 THEN     (* p1=p2=p3 = 0 ? *)
        BEGIN
          p1:= iz;
          Inc(iz); (* neue Nummer vergeben *)
        END
      ELSE
        BEGIN
          IF (p2<>p3) AND (p2<>0) THEN BEGIN Fuse(p2,p3); p3:= p2; END
            ELSE IF (p1=0) THEN p1:= p2;
          IF (p1<>p3) AND (p1<>0) THEN Fuse(p1,p3) ELSE p1:= p3;
      END;{if}

      IF n1<>0 THEN CON[n1].point:= p1;
      CON[i].point:= p1;
      IF n2<>0 THEN CON[n2].point:= p1;
  END; {for}
END;

{-------------------}
PROCEDURE Simulate;
{-------------------}
VAR x,y: RKTYP;
      f: BOOLEAN;

PROCEDURE NotZust;
BEGIN
  IF GATE[i].zust<>0 THEN GATE[i].zust:= 0 ELSE GATE[i].zust:= 255;
END;

PROCEDURE SAND;
BEGIN
  SI(2,GI(1) AND GI(3));
END;

PROCEDURE SOR;
BEGIN
  SI(2,GI(1) OR GI(3));
END;

PROCEDURE SNOT;
BEGIN
  SI(2,NOT GI(1));
END;

PROCEDURE SNAND;
BEGIN
  SI(2,NOT(GI(1) AND GI(3)));
END;

PROCEDURE SMAYBE; (* Zufallsgenerator NICHT GENORMT! *)
VAR z: WORD;

BEGIN
  z:= GATE[i].p[3]*256+GATE[i].p[4];   (* 16-Bit Counter *)
  IF z = 0 THEN
    BEGIN
      GATE[i].zust:= RANDOM(2);
      SI(1,(GATE[i].zust<>0));
      z:= FREQ;
    END
  ELSE
    Dec(z);
  GATE[i].p[3]:= z DIV 256; GATE[i].p[4]:= z MOD 256;
END;

PROCEDURE SVADD; (*290892*)
VAR e1,e2,c: BOOLEAN;

BEGIN
  e1:= GI(1); e2:= GI(2); c:= GI(3);
  f:=e1 XOR e2 XOR c;
  SI(4,f);
  IF (e1=e2) AND (e1=c) THEN SI(5,f) ELSE SI(5,NOT f); (* wenn e1=e2=c *)
END;

PROCEDURE SJKMSFF; (*050992*)
VAR z: BYTE;
BEGIN
  z:= GATE[i].zust;
  IF GI(3) THEN
    BEGIN
      IF (z AND 2)=0 THEN    (* öbergang Low auf High ? *)
        BEGIN
          z:= z OR 2;
          IF GI(2) THEN              (* J gesetzt *)
            BEGIN
              IF GI(4) THEN         (* auch K gesetzt *)
                z:= z XOR 4          (* dann Zustand Master negieren *)
              ELSE
                z:= z OR 4;          (* nur J, dann Zustand = HIGH *)
            END
          ELSE IF GI(4) THEN
            z:= z AND 251;           (* nur K, dann Zustand = LOW *)
        END;
    END
  ELSE
    IF (z AND 2)<>0 THEN  (* öbergang von High auf Low *)
      BEGIN
        z:= z AND 253; (* Bit 2 lîschen *)
        IF (z AND 4)=0 THEN
          z:= z AND 254  (* Ausgangszustand LOW *)
        ELSE
          z:= z OR 1; (* Master auf Slave *)
  END;
  IF GI(1) THEN z:=z OR 5;       (* Ausgang setzen *)
  IF GI(5) THEN z:=z AND 250;    (* Ausgang lîschen *)

  GATE[i].zust:= z;
  SI(6,(z AND 1)<>0); SI(7,(z AND 1)=0);
END;

PROCEDURE STFF; (*150892*)

BEGIN
  IF GI(1) THEN
    BEGIN
      IF GATE[i].p[4]=0 THEN
        BEGIN
          GATE[i].p[4]:= 1;
          NotZust;
      END;
    END
  ELSE
    GATE[i].p[4]:= 0;
  f:= GATE[i].zust<>0;
  SI(2,f); SI(3,NOT f);
END;

PROCEDURE SLAMP;
BEGIN
  f:= GI(1);
  IF f<>(GATE[i].zust<>0) THEN
    BEGIN
      MouseOff;
      DLAMP(GX*x+GXH,GY*y,f);
      MouseOn;
      IF f THEN GATE[i].zust:= 255 ELSE GATE[i].zust:= 0;
  END;
END;

PROCEDURE SSWITCH;
BEGIN
  (* Der Schalterzustand ist in zust gespeichert ! *)
  f:= GATE[i].zust<>0;
  SI(2,f);
  IF f<>(GATE[i].p[1]=1) THEN
    BEGIN
      MouseOff;
      SetFillStyle(1,BACKC);
      Bar(GX*x+GXH,GY*y,GX*x+GX+7+GXH,GY*y+GY);
      IF f THEN
        BEGIN
          DSWITCHON(x*GX+GXH,y*GY);
          GATE[i].p[1]:= 1;
        END
      ELSE
        BEGIN
          DSWITCHOFF(x*GX+GXH,y*GY);
          GATE[i].p[1]:= 0;
      END;
      MouseOn;
  END;
END;
{----------------}
PROCEDURE SSEG7;  (* 140892 *)
{----------------}
VAR z: BYTE;

BEGIN
  z:= 0;
  IF GI(1) THEN z:= z OR 1; IF GI(2) THEN z:= z OR 2;
  IF GI(3) THEN z:= z OR 4; IF GI(4) THEN z:= z OR 8;
  IF z<>GATE[i].zust THEN
    BEGIN
      GATE[i].zust:= z;
      MouseOff;
      DSEG7D(x*GX+GXH,y*GY,SEG7DAT[z]);
      MouseOn;
  END;
END;

{-----------------}
PROCEDURE SCLOCK;  (*140892*)
{-----------------}
VAR z: WORD;
BEGIN
  z:= GATE[i].p[3]*256+GATE[i].p[4];   (* 16-Bit Counter *)
  IF z = 0 THEN
    BEGIN
      NotZust;
      SI(2,GATE[i].zust<>0);
      z:= FREQ;
    END
  ELSE
    Dec(z);
  GATE[i].p[3]:= z DIV 256; GATE[i].p[4]:= z MOD 256;
END;

 { Simulate }

BEGIN
  SetWriteMode(NORMALPUT);
  FOR i:= 1 TO PRED(GatP) DO
    BEGIN
      x:= GATE[i].x; y:= GATE[i].y;
      CASE GATE[i].typ OF
        ANNR: SAND;
        ORNR: SOR;
        NONR: SNOT;
        NANR: SNAND;
        JKNR: SJKMSFF;
        TFNR: STFF;
        MYNR: SMAYBE;
        VANR: SVADD;
        LANR: SLAMP;
        SWNR: SSWITCH;
        S7NR: SSEG7;
        CKNR: SCLOCK;
      END;
  END;
END;

{-----------------}
PROCEDURE HLMenu;  (* 060892 *)
{-----------------}
BEGIN
  GotoXY(MX,opm+MBY); TextColor(LIGHTBLUE); writeln(MENU[opm]);
  opm:= mp;
  GotoXY(MX,mp+MBY); TextColor(YELLOW); writeln(MENU[mp]);
END;

{-----------------}
PROCEDURE HLGate;  (* 060892 *)
{-----------------}
BEGIN
  GotoXY(MX,opg+MY); TextColor(LIGHTBLUE); writeln(GATET[opg].name);
  opg:= gp;
  GotoXY(MX,gp+MY); TextColor(YELLOW); writeln(GATET[gp].name);
  ShowGate(opg);
END;

{-----------------}
PROCEDURE Rebuild;  (*210892*)
{-----------------}
VAR i,j: INTEGER;
    xx,yy: RKTYP;

BEGIN
  MouseOff;
  FillMatrix(0,0,RX,RY,0,FALSE); DrawRaster;
  FOR i:= 1 TO PRED(GatP) DO    (* alle Gatter zeichnen *)
    WITH GATE[i] DO
      BEGIN
        DrawGate(typ,x*GX+GXH,y*GY);
        FillMatrix(x,y,GATET[typ].b,GATET[typ].h,255,FALSE);
        CPoints(x,y,typ,i);
  END;

  PFlag:= TRUE;
  SetColor(WHITE);
  FOR i:= 1 TO PRED(ConP) DO
    WITH CON[i] DO
    BEGIN
      px:= x; py:=  y; xx:= px; yy:= py;
      IF le<0 THEN xx:= xx - le ELSE yy:= yy + le;
      DrawLine(xx,yy); LineMVH(px,py,le);
  END;
  PFlag:= FALSE;
  MouseOn;
END;

(***************************************************************************)
(*                         IO-Package (120892)                             *)
(***************************************************************************)
{------------------------------------}
FUNCTION Requester(title:TitleTyp; VAR ErrorCode: BYTE):DListTyp; (*310892*)
{------------------------------------}

VAR i: INTEGER;
    fzei,z,xt,yt,nr,onr: BYTE;
    Canf,ActF: BOOLEAN;
    name: STRING;

CONST REQX = 25; REQY = 8; REQB = 30; REQH = 15;
      ASWX = 3; ASWY = 4; ASWB = 14; ASWH = 9; (* Auswahlfenster *)
      ACTX = 20; ACTY = 5; ACTB = 8; ACTH = 3; (* Aktionsknopf *)
      CANX = 20; CANY = 9; CANB = 8; CANH = 3; (* Cancel-Knopf *)

PROCEDURE GoXY(x,y:BYTE);
BEGIN
  GotoXY(REQX+x,REQY+y);
END;

PROCEDURE GetString(VAR text:STRING; mxl:BYTE);
VAR xp,yp,x:BYTE;
    key: CHAR;

BEGIN
  xp:= WhereX; yp:= WhereY;
  x:= Length(text);
  REPEAT
    GotoXY(xp,yp);
    write(text,'_ ');
    IF Keypressed THEN
      BEGIN
        key:= ReadKey;
        IF (key=#8) AND (x>0) THEN
          BEGIN
            Dec(x); Dec(text[0]);
        END;
        IF (key>#31) AND (x<mxl) THEN
          BEGIN
            text:=text+key;
            Inc(x);
        END;
    END;
  UNTIL key=#13;
END;


PROCEDURE Gadget(x,y: BYTE; text:TitleTyp);
BEGIN
  GoXY(x,y);   write('⁄ƒƒƒƒƒƒø');
  GoXY(x,y+1); write('≥',text,'≥');
  GoXY(x,y+2); write('¿ƒƒƒƒƒƒŸ');
END;

FUNCTION Pressed(x,y,b,h: BYTE):BYTE;
BEGIN
  Pressed:= 0;
  IF (xt>=x) AND (xt<x+b) THEN
    IF (yt>=y) AND (yt<y+h) THEN
      Pressed:= SUCC(yt-y);         (* Nr. der Zeile *)
END;

PROCEDURE ShowDateien;
VAR i,j:BYTE;

BEGIN
  z:= 0; TextColor(13);
  GoXY(8,3); write(' ',path);
  FindFirst(Path,ATTR,FCB);
  WHILE (DosError = 0) AND (z-fzei<MAXDATEIEN) DO
    BEGIN
      Inc(z);
      IF z>fzei THEN
        DList[z-fzei]:= FCB.name;
      FindNext(FCB);
  END;
  FOR i:= 1 TO z-fzei DO
    BEGIN
      GoXY(ASWX+1,ASWY+i);
      write(Dlist[i]);
      FOR j:= 1 TO ASWB-Length(DList[i])-1 DO
        write(' ');
  END;
  FOR i:= SUCC(z-fzei) TO MAXDATEIEN DO
    BEGIN
      GoXY(ASWX+1,ASWY+i);
      writeln('            ');
  END;
END;

BEGIN
  TextColor(3);
  GoXY(1,1); write('…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª');
  FOR i:= 2 TO REQH-1 DO
    BEGIN
      GoXY(1,i);write('∫                            ∫');
  END;
  GoXY(1,REQH); write('»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº');
  GoXY(ASWX,ASWY); write('⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒø');
  FOR i:= 1 TO MAXDATEIEN DO
    BEGIN
      GoXY(ASWX,ASWY+i); write('≥             ≥');
  END;
  GoXY(ASWX,ASWY+MAXDATEIEN+1); write('¿ƒƒƒƒƒƒƒƒƒƒƒƒƒŸ');
  GoXY(ASWX+ASWB,ASWY+1); write(#30);
  GoXY(ASWX+ASWB,ASWY+MAXDATEIEN); write(#31);
  GoXY(10,2); write(title,'File');
  GoXY(3,3); write('Path:');

  GoXY(3,REQH-1); write('Name:');

  Gadget(ACTX,ACTY,title);
  Gadget(CANX,CANY,'Cancel');

  CanF:= FALSE; ActF:= FALSE;
  fzei:= 0; ECode:= 0; onr:= 1; name:='';
  ShowDateien; MouseOn;
  REPEAT
    GetMouseXY(x,y,k);
    xt:= (x DIV 8)+1;  (* Textmodus-Raster *)
    yt:= (y DIV CHARHEIGHT)+1;
    IF (xt>REQX) AND (xt<REQX+REQB) AND (k=MOUSELEFT) THEN
      IF (yt>REQY) AND (yt<REQY+REQH) THEN
        BEGIN
          MouseOff;
          Dec(xt,REQX);
          Dec(yt,REQY);
          IF Pressed(ACTX,ACTY,ACTB,ACTH)>0 THEN ActF:= TRUE;
          IF Pressed(CANX,CANY,CANB,CANH)>0 THEN CanF:= TRUE;
          IF Pressed(ASWX+ASWB,ASWY+1,1,1)>0 THEN
            IF fzei>0 THEN BEGIN Dec(fzei); ShowDateien; END;
          IF Pressed(ASWX+ASWB,ASWY+MAXDATEIEN,1,1)>0 THEN
            IF fzei<z THEN BEGIN Inc(fzei); ShowDateien; END;
          IF Pressed(9,REQH-1,10,1)>0 THEN
            BEGIN
              GoXY(9,REQH-1); TextColor(YELLOW);
              GetString(name,15);
          END;
          nr:= Pressed(ASWX,ASWY,ASWB,ASWH);
          IF (nr>1) AND (nr-1<>onr) THEN
            BEGIN
              GoXY(ASWX+1,ASWY+onr);
              TextColor(13); writeln(DList[onr]);
              onr:= nr-1;
              name:= DList[onr];
              GoXY(ASWX+1,ASWY+onr);
              TextColor(YELLOW);
              writeln(name);
              GoXY(9,REQH-1); writeln('            ');
              GoXY(9,REQH-1); writeln(name);
          END;
          MouseOn;
          Delay(50);
    END;
  UNTIL CanF OR ActF;
  IF Canf THEN ECode:= 1;
  Requester:= name;
END;

{---------------}
PROCEDURE Save;  (*190892*)
{---------------}
BEGIN
  DName:= Requester(' Save ',Ecode);
  IF ECode=0 THEN
    BEGIN
      Assign(Datei,DName);
      Rewrite(Datei);
      writeln(Datei,'SIGDATEI');
      writeln(Datei,GatP);
      FOR i:= 1 TO PRED(GatP) DO
        WITH GATE[i] DO
          BEGIN
            writeln(Datei,x);
            writeln(Datei,y);
            writeln(Datei,typ);
            FOR j:= 1 TO MAXPINS DO writeln(Datei,p[j]);
      END;
      writeln(Datei,ConP);
      FOR i:= 1 TO PRED(ConP) DO
        WITH CON[i] DO writeln(Datei,x,#13,y,#13,le,#13,p1,#13,p2);
      Close(Datei);
  END;
  Rebuild;
END;

{----------------}
PROCEDURE Load;   (*190892*)
{----------------}
VAR Kennung: STRING[8];

BEGIN
  DName:= Requester(' Load ',ECode);
  IF ECode=0 THEN
    BEGIN
      Assign(Datei,DName);
      {$I-} Reset(Datei); {$I+}
      IF IOResult=0 THEN
        BEGIN
          Readln(Datei,Kennung);
          IF Kennung='SIGDATEI' THEN
            BEGIN
              Readln(Datei,GatP);
              FOR i:= 1 TO PRED(GatP) DO
                BEGIN
                  WITH GATE[i] DO
                    BEGIN
                      readln(Datei,x,y,typ);
                      FOR j:= 1 TO MAXPINS DO readln(Datei,p[j]);
                      zust:= 0;
                  END;
              END;
              readln(Datei,ConP);
              FOR i:= 1 TO PRED(ConP) DO
                WITH CON[i] DO readln(Datei,x,y,le,p1,p2);
              Close(Datei);
            END
          ELSE
            ECode:= 3; (* Keine SIG-Datei *)
      END
    ELSE
      ECode:= 2;
  END;
  IF ECode= 1 THEN ECode:= 0;
  Rebuild; (* Schaltplan neu aufbauen *)
END;

{------------------}
PROCEDURE New;
{------------------}
BEGIN
  FillMatrix(0,0,RX,RY,0,FALSE); DrawRaster;
  GatP:= 1; ConP:= 1; lfd:= 0;
  typ:= 0; gp:= 0; mp:= 1; MFlag:= PLACEF;
  HLMenu; HLGate;
END;

{-----------------}
PROCEDURE Action;
{-----------------}
VAR h:BYTE;

BEGIN
  MouseOff;
  ECode:= 0;
  IF x>GRX THEN
    BEGIN
      IF (y>GMO) AND (y<GMO+GMH) THEN
        BEGIN                                 (* GATE-Bereich *)
          gp:= SUCC(y DIV CHARHEIGHT)-MY;
          HLGate; MFlag:= PLACEF; mp:= 1;
          HLMenu;
      END;

      IF (y>MO) AND (y<MO+MMH) THEN
        BEGIN
          mp:= SUCC(y DIV CHARHEIGHT)-MBY;   (* MenÅ-Bereich *)
          HLMenu;
          CASE mp OF
            0: MFlag:= LINEF;
            1: MFLag:= PLACEF;
            2: MFlag:= ZIPF;
            3: Load;
            4: Save;
            5: BEGIN MFlag:= SIMF; BuildInfoTab; END;
            6: New;
            7: QuitF:= TRUE;
          END;
       END;{if}
    END
  ELSE
    CASE MFlag OF
      LINEF: CLine;
     PLACEF: PlaceGate(xr,yr,typ);
       ZIPF: DeletePart;
       SIMF: BEGIN
               i:= SearchGate(xr,yr);
               IF GATE[i].typ = SWNR THEN
                   GATE[i].zust:= NOT GATE[i].zust;
             END;
  END;
  ShowEMess(ECode);
  MouseOn;
  Delay(200);
END;

(***************************************************************************)
(*                                   MAIN                                  *)
(***************************************************************************)

BEGIN {main}
  Init; Title; View;
  New;
  MouseOn;
  QuitF:= FALSE;

  (* Rasterschleife *)
  xr:= 0;
  FFlag:= FALSE; MBFlag:= FALSE;
  REPEAT
    IF NOT FFlag THEN Cursor(xr,yr,XORPUT);
    REPEAT
      IF MFlag = SIMF THEN Simulate; (* Simulationsmodus *)
      xo:= xr; yo:= yr;
      GetMouseXY(x,y,k);
      xr:= (x DIV GX);
      yr:= (y DIV GY);
      MBFlag:= (xr+br)>rastx;
      IF (yr+ho)>rasty THEN yr:= rasty-ho;
    UNTIL (xo<>xr) OR (yo<>yr) OR (k>0) OR MBFlag;
    IF NOT FFlag THEN Cursor(xo,yo,XORPUT);
    FFlag:= MBFlag;
    Statusline;
    IF k = MOUSELEFT THEN Action;
  UNTIL Quitf = TRUE;
  CloseAll;  (* Speicher, Modus in alten Zustand versetzen *)
END.
(* FINISCH! *)