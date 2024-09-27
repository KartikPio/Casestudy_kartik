     ** ------------------------------------------------------------------------------
     ** Created By..........: Programmers.io @ 2024
     ** Created Date........: 2024/07/31
     ** Developer...........: Kartik Parihar
     ** Description.........: Manager Deshboard Main Module
     ** -------------------------------------------------------------------------------
     ** MODIFICATION LOG:
     ** -------------------------------------------------------------------------------
     ** Date    | Mod_ID  | Developer  | Case and Description
     ** --------|---------|------------|-----------------------------------------------
     ** 24/07/31|         | Kartik P   | Module Creation
     ** -------------------------------------------------------------------------------

     ** Control Option
     HOption(*Nodebugio : *SrcStmt)
     HNomain

     ** File declaration
     FCRDSBD    CF   E             WorkStn Indds(IndctrDs)

     ** Data structure declaration
     DIndctrDs         Ds
     DIndExit                          N   Overlay(Indctrds : 03)
     DIndCancel                        N   Overlay(Indctrds : 12)

     ** Copy book declaration
     C/Copy KartikCS/Qrpglesrc,Copy_Book

**Free
// Main Code
Dcl-Proc CrDshbrd Export;
   Dcl-Pi CrDshbrd;
      UserId Char(10);
   End-Pi;

   Dow IndExit = *Off or IndCancel = *Off;
      CrHeading = '   Customer Representative Main Manu   ';
      CrFtrL1  = 'F3=Exit   F12=Cancel';
      Write CrHeader;
      Write CrFooter;
      Exfmt CrMainScr;

      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            Clear CrErrMsg;
            Clear SCrChoice;
            Leave;
         When SCrChoice = 1;
            DisplayCr(UserId);
            Clear SCrChoice;

        When SCrChoice = 2;
           //CustomerSubFile();
           Clear SCrChoice;

        When SCrChoice = 3;
           //CustAccSubFile();
           Clear SCrChoice;

        When SCrChoice = 4;
           //Transaction();
           Clear SCrChoice;

        When SCrChoice = 5;
           //TrHistorySubFile();
           Clear SCrChoice;

        When SCrChoice = 6;
           //ApplyLoan();
           Clear SCrChoice;

         When SCrChoice = 7;
            //ChangPswd();
            Clear SCrChoice;

         Other;
            Clear SCrChoice;

      EndSl;
   EndDo;
End-Proc;
