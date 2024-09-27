**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/09/17                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Subfile for Transaction Details                           //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/09/17|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt) NOMain;

// File Declaration
Dcl-F TrHistoryP Printer Usage(*Output) UsrOpn;
Dcl-F MngDsbD WorkStn Indds(IndicatorDs) Sfile(TRHISSFL01 : #Rrn)
                                         Sfile(TRHISSLF02 : #Rrn1);
// Data Structure Declaration
Dcl-Ds IndicatorDs;
   IndExit         Ind Pos(03);
   IndRefresh      Ind Pos(05);
   IndStatement    Ind Pos(09);
   IndCancel       Ind Pos(12);
   IndSflDsp       Ind Pos(21);
   IndSflDspCtl    Ind Pos(22);
   IndSflClr       Ind Pos(23);
   IndSflEnd       Ind Pos(24);
   IndOptRI        Ind Pos(25);
   IndOptPC        Ind Pos(26);
   IndSflDsp1      Ind Pos(42);
   IndSflDspCtl1   Ind Pos(43);
   IndSflClr1      Ind Pos(44);
   IndSflEnd1      Ind Pos(45);
   IndOverlay      Ind Pos(46);
End-Ds;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-S #Rrn  Zoned(4) Inz(*Zero);
Dcl-S #Rrn1 Zoned(4) Inz(*Zero);
Dcl-S Stmt  Char(200) Inz(*Blank);
Dcl-C QT    Const('''');

// Main Code
Dcl-Proc TrHistorySubFile Export;
   IndExit = *Off;
   Dow IndExit = *Off;
      ClearSfl();
      LoadSfl();
      DisplaySfl();

      Exec Sql
         Set Option Commit = *None, DatFmt= *Iso;

      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            Clear MngErrMsg;
            Clear MngMainScr;
            IndOptRI = *Off;
            IndOptPC = *Off;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear MngErrMsg;
            Clear S1Position;
            Clear S1Option;
            IndOptRI = *Off;
            IndOptPC = *Off;
         Other;
            OtherOption();
     Endsl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ClearSfl                                                            //
// Description   : Procedure to Clear Transaction History Subfile                      //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearSfl;
   IndSflClr = *On;
   #Rrn      = 0;
   Write TrHisCtl01;
   IndSflClr = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Transaction History Subfile                       //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   IndOptPC = *On;
   Clear Stmt;
   Stmt = 'Select C.CId, C.CName, A.AccNO, A.AccBlnce ' +
          'From CustPf C Join AccPf  A On C.CId = A.CustId';

   If S1Position <> *Blank;
      Stmt = %Trim(Stmt) + ' Where C.CId Like ' + QT + '%' + %Trim(S1Position) + '%' + QT +
             ' Or C.CName Like ' + QT + '%' + %Trim(S1Position) + '%' + QT;
   EndIf;

   Exec Sql
      Prepare SqlStmt From :Stmt;

   Exec Sql
      Declare C01 Cursor for Sqlstmt;

   Exec Sql
      Open C01;

   Exec Sql
      Fetch from C01 Into :S1CId, :S1CName, :S1AccNo, :S1AccBlnce;

   Dow SqlCode = 0;
      #Rrn += 1;

      If #Rrn > 9999;
         Leave;
      EndIF;

      Write TrHisSfl01;

   Exec Sql
      Fetch from C01 Into :S1CId, :S1CName, :S1AccNo, :S1AccBlnce;

   EndDo;

   Exec Sql
      Close C01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Transaction History Subfile                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp    = *On;
   IndSflDspCtl = *On;
   IndSflEnd    = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIf;
   Clear S1Position;
   MngHdr   = '       Display Transaction History'       ;
   MngFtrL2 = 'F3=Exit   F5=Refresh   F12=Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt TrHisCtl01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: OtherOption                                                         //
// Description   : Procedure to read sfl for display transaction                       //
//------------------------------------------------------------------------------------ //
Dcl-Proc OtherOption;
   ReadC TrHisSfl01;
   DoW S1Option <> 0;
      If S1Option = 5;
         DisplayTr();
      EndIf;
      ReadC TrHisSfl01;
   Enddo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplayTr                                                         //
// Description   : Procedure to Display Custome Details                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplayTr;
   IndExit = *Off;
   Dow IndExit = *Off;
      ClearSfl1();
      LoadSfl1();
      DisplaySfl1();

      Select;
         When IndCancel = *On;
            IndOverlay = *Off;
            IndCancel = *Off;
            Clear MngErrMsg;
            Clear S1Option;
            Leave;

         When IndStatement = *On;
            IndStatement   = *Off;
            TrStatement();

      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ClearSfl1                                                            //
// Description   : Procedure to Clear Transaction History Subfile                      //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearSfl1;
   IndSflClr1 = *On;
   #Rrn1      = 0;
   Write TrHisCtl02;
   IndSflClr1 = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Transaction History Subfile                       //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl1;
   Exec Sql
      Declare TrCursor Cursor for
      Select TrId, ToAccNo, FromAccNO, TrAmount,
      TrType, TrStatus, TrDate, TrTime
      From TrHstryPf
      Where CAccNo = :S1AccNo;

   Exec Sql
      Open TrCursor;

   Exec Sql
      Fetch From TrCursor Into :S4TrId, :S4ToAccNO, :S4FrmAccNo, :S4TrAmount,
      :S4TrType, :S4TrStatus, :S4TrDate, :S4TrTime;

      Dow SqlCode = 0;
         TrStatus01 = 'Tr Status:';
         TrDate01   = 'Tr Date:';
         TrTime01   = 'Tr Time:';

         #Rrn1 += 1;

         If #Rrn1 > 9999;
            Leave;
         EndIF;

         Write TRHISSLF02;

         Exec Sql
            Fetch From TrCursor Into :S4TrId, :S4ToAccNO, :S4FrmAccNo, :S4TrAmount,
            :S4TrType, :S4TrStatus, :S4TrDate, :S4TrTime;
      EndDo;

      Exec Sql
         Close TrCursor;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl1                                                          //
// Description   : Procedure to Display Transaction History Subfile                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl1;
   IndOverlay    = *On;
   IndSflDsp1    = *On;
   IndSflDspCtl1 = *On;
   IndSflEnd1    = *On;

   If #Rrn1 < 1;
      IndSflDsp1 = *Off;
   EndIf;

   Exec Sql
      Select C.CId, C.CName, A.AccNo
      Into :S4CustId, :S4CustName, :S4AccNmbr
      From Custpf C
      Join AccPf  A
      On C.CId = A.CustId
      Where C.CId = :S1CId;


   MngHdr   = '       Display Transaction History'       ;
   MngFtrL2 = 'F9 =Generate Statement   F11=Fold   F12=Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt TrHisCtl02;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: TrStatement                                                         //
// Description   : Procedure to Generate Transaction Statement                         //
//------------------------------------------------------------------------------------ //
Dcl-Proc TrStatement;
   Open TrHistoryP;
   Write Header;
   Write Colhdg;

   Exec Sql
      Select TrId, ToAccNo, FromAccNO, TrAmount,
      TrType, TrStatus, TrDate, TrTime
      Into :PTrId, :PToAccNo, :PFromAccNo, :PTrAmount,
      :PTrType, :PTrStatus, :PTrDate, :PTrTime
      From TrHstryPf
      Where CAccNo = :S1AccNo;

   Write Records;
   Close TrHistoryP;
   MngErrMsg = 'Statement Generated';
End-Proc;
