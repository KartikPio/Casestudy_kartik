**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/08/30                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Subfile for Custromer                                     //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/08/30|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt) BndDir('KARTIKCS/STATEBND');
Ctl-Opt NoMain;

// File Declaration
Dcl-F MngDsbD WorkStn Indds(IndicatorDs) Sfile(CUSTSflM01 : #Rrn)
                                         Sfile(CrDltSflM1: #Rrn1);

// Data Sturcture Declaration
Dcl-Ds IndicatorDs;
   IndExit      Ind Pos(03);
   IndRefresh   Ind Pos(05);
   IndInsert    Ind Pos(06);
   IndCancel    Ind Pos(12);
   IndSflDsp    Ind Pos(21);
   IndSflDspCtl Ind Pos(22);
   IndSflClr    Ind Pos(23);
   IndSflEnd    Ind Pos(24);
   IndOptRI     Ind Pos(25);
   IndOptPC     Ind Pos(26);
End-Ds;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-S #Rrn         Zoned(4) Inz(*Zero);
Dcl-S #Rrn1        Zoned(4) Inz(*Zero);

// Main Code
Dcl-Proc CustomerSubFile Export;
   Dow IndExit = *Off;
      ClearSfl();
      LoadSfl();
      DisplaySfl();

      Exec Sql
         Set Option Commit = *None, DatFmt= *Iso;

      Select;
         When IndExit = *On Or IndCancel = *On;
            IndCancel = *Off;
            Clear MngErrMsg;
            Clear MngMainScr;
            IndOptRI = *Off;
            IndOptPC = *Off;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear MngErrMsg;
            Clear S1OPTION;
            Clear S1Position;
            IndOptRI = *Off;
            IndOptPC = *Off;

          When IndInsert = *On;
            IndInsert   = *Off;
            Clear MngErrMsg;
            InsertNewRec();
         Other;
            OtherOption();
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ClearSfl                                                            //
// Description   : Procedure to Clear Custome Representative Subfile                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearSfl;
   IndSflClr = *On;
   #Rrn      = 0;
   Write CustCtlM01;
   IndSflClr = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Custome Representative Subfile                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   IndOptPC = *On;
   Exec Sql
      Declare SflCursor Cursor for
      Select CId, CName, CPan, CMob From CustPF;

   Exec Sql
      Open SflCursor;

   Exec Sql
       Fetch From SflCursor Into :S1Cid, :S1Cname, :S1CPan, :S1CMob;

   Dow SqlCode = 0;

      #Rrn += 1;

      If #Rrn > 9999;
         Leave;
      EndIF;

      Write CustSflM01;

      Exec Sql
       Fetch From SflCursor Into :S1Cid, :S1Cname, :S1CPan, :S1CMob;

   EndDo;
   Exec Sql
      Close SflCursor;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Custome Subfile                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp     = *On;
   IndSflDspCtl  = *On;
   IndSflEnd     = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIf;

   MngHdr   = 'Work With Customer Representative';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F6=Add New Record   F12= Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt CustCtlM01;
End-Proc;

