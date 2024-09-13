**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/09/12                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Subfile for Account Details                               //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/09/12|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt) NOMain;

// File Declaration
Dcl-F MngDsbD WorkStn Indds(IndicatorDs) Sfile(AccSfl01 : #Rrn);


// Data Structure Declaration
Dcl-Ds IndicatorDs;
   IndExit         Ind Pos(03);
   IndPrompt       Ind Pos(04);
   IndRefresh      Ind Pos(05);
   IndConfirm      Ind Pos(07);
   IndCancel       Ind Pos(12);
   IndSflDsp       Ind Pos(21);
   IndSflDspCtl    Ind Pos(22);
   IndSflClr       Ind Pos(23);
   IndSflEnd       Ind Pos(24);
   IndOptRI        Ind Pos(25);
   IndOptPC        Ind Pos(26);
   IndBrCodeRI     Ind Pos(27);
   IndAccOpnDtRI   Ind Pos(28);
   IndAccStatusRI  Ind Pos(29);
   IndChoiceRI     Ind Pos(30);
   IndFieldPR      Ind Pos(98);
End-Ds;

Dcl-Ds AccDetails;
   S2BrCode    Char(11)   Inz;
   S2AccOpnDt  Date       Inz;
   S2IRate     Zoned(5:2) Inz;
   S2AccStats  Char(10)   Inz;
End-Ds;
//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-S #Rrn Zoned(4) Inz(*Zero);
Dcl-C AlphaNum 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';

// Main Code
Dcl-Proc CustAccSubFile Export;
   IndExit = *Off;
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
            Clear S1Option;
            Clear S1Position;
            IndOptRI = *Off;
            IndOptPC = *Off;

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
   Write ACCCTL01;
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
      Select A.CustId, A.AccType, A.AccStatus, C.CName
      From AccPf A
      Join CustPf C
      On A.CustId = C.CId;

    Exec Sql
       Open SflCursor;

    Exec Sql
       Fetch From SflCursor Into :S1CustId, :S1AccType, :S1AccStats, :S1Cname2;

    Dow SqlCode = 0;
       #Rrn += 1;

       If #Rrn > 9999;
          Leave;
       EndIF;

       Write AccSfl01;

       Exec Sql
          Fetch From SflCursor Into :S1CustId, :S1AccType, :S1AccStats, :S1Cname2;
    EndDo;
    Exec Sql
       Close SflCursror;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Custome Subfile                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp    = *On;
   IndSflDspCtl = *On;
   IndSflEnd    = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIf;
   Clear S1Position;
   MngHdr   = '        Work With Account Details        ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F12=Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt AccCtl01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: OtherOption                                                         //
// Description   : Procedure to read sfl for other crud operation                      //
//------------------------------------------------------------------------------------ //
Dcl-Proc OtherOption;
   ReadC AccSfl01;
   DoW S1Option <> 0;
      Select;
         When S1Option = 2;
            UpdateAcc();
         When S1Option = 4;
            //DeleteAcc();
         When S1Option = 5;
            //DisplayAcc();
         Other;
            Clear S1Option;
      EndSl;
      Clear S1Option;
      ReadC AccSfl01;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: UpdateAcc                                                           //
// Description   : Procedure to update account details                                 //
//------------------------------------------------------------------------------------ //
Dcl-Proc UpdateAcc;

   Exec Sql
      Select A.CustId, A.AccNo, A.BrCode, A.AccType, A.AccOpnDt, A.AccStatus, C.CName
      Into :S2CustId, :S2AccNo, :S2BrCode, :S2AccType, :S2AccOpnDt, :S2AccStats, :S2CName3
      From AccPf A
      Join CustPf C
      On A.CustId = C.CId;

   MNGHDR   = '         Update Account Details         ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';

   Dow IndExit = *Off Or IndCancel = *Off;
      Write MngHeader;
      Write MngFooter;
      Exfmt AccOprtion;

      Clear MngErrMsg;
      ResetInd();

      Select;
         When IndExit = *On Or IndCancel = *On;
            IndCancel = *Off;
            S1Option  = 0;
            Clear MngErrMsg;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear MngErrMsg;
            ResetInd();
            Exec Sql
               Select A.CustId, A.AccNo, A.BrCode, A.AccType, A.AccOpnDt, A.AccStatus, C.CName
               Into :S2CustId, :S2AccNo, :S2BrCode, :S2AccType, :S2AccOpnDt, :S2AccStats, :S2CName3
               From AccPf A
               Join CustPf C
               On A.CustId = C.CId;

         When PrmtFld = 'S2ACCSTATS' And IndPrompt = *On;
            IndPrompt = *Off;
            GetAccStatus();

         When IndConfirm = *On;
            IndConfirm   = *Off;
            AccValidation();
            If MngErrMsg = *Blank;
               UpdateRec();
            EndIf;

         Other;
            AccValidation();
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ResetInd                                                            //
// Description   : Procedure to turn off all field level indicators                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc ResetInd;
   IndOptRI        = *Off;
   IndOptPC        = *Off;
   IndBrCodeRI     = *Off;
   IndAccOpnDtRI   = *Off;
   IndAccStatusRI  = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: AccValidation                                                          //
// Description   : Procedure to Validate Fields for Account Update Operation            //
//------------------------------------------------------------------------------------ //
Dcl-Proc AccValidation;

   // Validation for BranchCode
   If S2BrCode    = *Blank;
      IndBrCodeRI = *On;
      MngErrMsg   = 'Branch Code field cannot be blank';
      Return;
   EndIf;
   If %Check(AlphaNum : S2BrCode) <> 0;
      IndBrCodeRI = *On;
      MngErrMsg   = 'Branch Code field cannot contain special character';
      Return;
   EndIf;
   // Validation for Account opening date
   If %Char(S2AccOpnDt) = '0001-01-01';
      IndAccOpnDtRI     = *On;
      MngErrMsg         = 'Account Opening date cannot be blank';
      Return;
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: GetAccStatus                                                          //
// Description   : Procedure to Select Account type                                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc GetAccStatus;
   Dow IndCancel = *Off;
      ExFmt AccStatsWn;

      If IndCancel = *On;
         IndCancel = *Off;
         Clear W2Choice;
         Clear W2ErrorMsg;
         IndChoiceRI = *Off;
         Leave;
      Else;
         Select;
            When W2Choice = '1';
               S2AccStats = 'PENDING';
               Clear W2ErrorMsg;
            When W2Choice = '2';
               S2AccStats  = 'APPROVED';
               Clear W2ErrorMsg;
             When W2Choice = '2';
               S2AccStats  = 'APPROVED';
               Clear W2ErrorMsg;
            Other;
              IndChoiceRI = *On;
              W2ErrorMsg  = 'Invalid Opt';
         EndSl;
         If W2ErrorMsg  = *Blank;
            IndChoiceRI = *Off;
            Clear W2Choice;
            Leave;
         EndIf;
      EndIf;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name:  UpdateRec;                                                         //
// Description   : Procedure to update account details                                 //
//------------------------------------------------------------------------------------ //
Dcl-Proc  UpdateRec;

   If S2AccType  = 'SAVING';
      S2Irate    = 2.70;
   Else;
      S2Irate  =   3.00;
   EndIf;

   Exec Sql
      Update AccPf
      Set BrCode=:S2BrCode, AccOpnDt=:S2AccOpnDt, IRate=:S2IRate, AccStatus=:S2AccStats
      Where CustId = :S2CustId;
   MngErrMsg  = 'Record Updated Successfully';
End-Proc;
