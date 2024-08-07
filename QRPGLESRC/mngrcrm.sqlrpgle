**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/08/01                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Subfile for Custromer Representative                      //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/08/01|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt);
Ctl-Opt NoMain;

// File Declaration
Dcl-F MngDsbD WorkStn Indds(IndicatorDs) Sfile(CRSflM01 : #Rrn);

// Data Sturcture Declaration
Dcl-Ds IndicatorDs;
   IndExit      Ind Pos(03);
   IndPrompt    Ind Pos(04);
   IndRefresh   Ind Pos(05);
   IndInsert    Ind Pos(06);
   IndConfirm   Ind Pos(07);
   IndCancel    Ind Pos(12);
   IndSflDsp    Ind Pos(21);
   IndSflDspCtl Ind Pos(22);
   IndSflClr    Ind Pos(23);
   IndSflEnd    Ind Pos(24);
   IndOptRi     Ind Pos(25);
   IndOptPc     Ind Pos(26);
   IndCrNameRi  Ind Pos(31);
   IndCrDobRi   Ind Pos(32);
   IndCrDojRi   Ind Pos(33);
   IndCurAddRi  Ind Pos(34);
   IndPrmAddRi  Ind Pos(35);
   IndCrSateRi  Ind Pos(36);
   IndCrCityRi  Ind Pos(37);
   IndMobNORi   Ind Pos(38);
   IndAltMObRi  Ind Pos(39);
   IndCrEmail   Ind Pos(40);
End-Ds;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,CopyBook

// Variable Declaration
Dcl-S #Rrn       Zoned(4) Inz(*Zero);
Dcl-S PCrId      Char(10) Inz(*Blank);
Dcl-S CrIdSubfix Zoned(8) Inz(*Zero);

// Main Code

Dcl-Proc CRSubFile Export;
   IndExit = *Off;
   Dow INdExit = *Off;
      ClearSfl();
      LoadSfl();
      DisplaySfl();

      Exec Sql
         Set Option Commit = *None;

      Select;
         When IndExit = *On Or IndCancel = *On;
             IndCancel = *Off;
             Clear MngErrMsg;
             Clear MngMainScr;
             IndOptRi = *Off;
             IndOptPc = *Off;
             Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear MngErrMsg;
            Clear S1OPTION;
            Clear POSITIONTO;
            IndOptRi = *Off;
            IndOptPc = *Off;

         When IndInsert = *On;
            IndInsert   = *Off;
            InsertNewRec();
         Other;
            //

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
   Write CRCTLM01;
   IndSflClr = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Custome Representative Subfile                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   Exec Sql
      Set Option Commit = *None;

   Exec Sql
      Declare SflCursor Cursor for
      Select CrId, CrName, CrDoj, CrMob From CustRepPf;

   Exec Sql
      Open SflCursor;

   Dow SqlCode = 0;
      Exec Sql
         Fetch Next From SflCursor Into :S1CrId, :S1CrName, :S1CrDoj, :S1CRMob;

      #Rrn += 1;

      If #Rrn > 9999;
         Leave;
      EndIF;

      Write CRSflM01;

   EndDo;
   Exec Sql
      Close SflCursor;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Custome Representative Subfile                 //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp    = *On;
   IndSflDspCtl = *On;
   IndSflEnd    = *On;

   If #Rrn < 1;
   IndSflDsp = *Off;
   EndIf;

   MngHdr   = 'Work With Customer Representative';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F6=Add New Record   F12= Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt CrCtlM01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: InsertNewRec                                                        //
// Description   : Procedure to Insert New Custome Representative Details              //
//------------------------------------------------------------------------------------ //
Dcl-Proc InsertNewRec;
   MNGHDR   = 'Add New Customer Representative Details';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';

   Exec Sql
      Select Max(CrId) Into :PCrId From CustRepPf;

   If PCrId  = *Blank;
      S2CrId = 'CR001';
   Else;
      CrIdSubfix = %Int(%Subst(PCrId : 3)) + 1;
      S2CrId     = 'CR' + %Editc(CrIdSubfix : 'X');
   EndIF;

   Dow IndExit = *Off Or IndCancel = *Off;
      Write MngHeader;
      Write MngFooter;
      Exfmt MngCurdScr;

      Clear MngErrmsg;
      ResetInd();

      Select;
         When IndExit = *on Or IndCancel = *On;
            IndCancel = *Off;
            Clear MngErrMsg;
            Clear MngCurdScr;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear MngErrMsg;
            Clear MngCurdScr;

         When IndPrompt = *On;
            //StateCityPrmpt();

         When IndConfirm = *On;
            IndConfirm   = *Off;
            //MngCurdVld();
            If MngErrMsg = *Blank;
              InsertRec();
            EndIf;

         Other;
            //MngCurdVld();
      EndSl;
   EndDo;
End-Proc;

Dcl-Proc ResetInd;
   IndOptRi     = *Off;
   IndCrNameRi  = *Off;
   IndCrDobRi   = *Off;
   IndCrDojRi   = *Off;
   IndCurAddRi  = *Off;
   IndPrmAddRi  = *Off;
   IndCrSateRi  = *Off;
   IndCrCityRi  = *Off;
   IndMobNORi   = *Off;
   IndAltMObRi  = *Off;
   IndCrEmail   = *Off;
End-Proc;

Dcl-Proc InsertRec;
   Exec Sql
      Insert Into KartikCs/CustRerpPF
      (CrId, CrName, CrGender, CrDob, CrDoj, CrCurAddr, CrPrmAddr, State, City, PinCode,
       CrMob, CrAltMob, CrEmail)
       Values(:S2CrId, :S2Crname, :Gender, :S2CrDob, :S2CrDoj, :S2CrCurAdd, :S2CrPrmAdd,
               :S2State, :S2City, :S2PinCode, :S2CrMob, :S2CrAltMob, :S2CrEmail);

End-Proc;
