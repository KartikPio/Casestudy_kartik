**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/07/31                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Manager Deshboard Main Module                             //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/07/31|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt) Bnddir('KARTIKCS/MANAGERBND');
Ctl-Opt Nomain;

// File Declaration
Dcl-F MngDsbD WorkStn Indds(IndicatorDs);

// Data Sturcture Declaration
Dcl-Ds IndicatorDs;
   IndExit    Ind Pos(03);
   IndRefresh Ind Pos(05);
   IndCancel  Ind Pos(12);
End-Ds;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

// Main Code

// Call Manager Deshboard screen
Dcl-Proc MngDsbrD Export;
   Dcl-Pi MngDsbrD;
       P_UserId Char(10);
   End-Pi;
   Dow IndExit = *Off Or IndCancel = *Off;
      MngHdr   = '           Manager Main Manu            ';
      MNGFTRL2 = 'F3=Exit   F5=Refresh   F12=Cancel';
      Write MngHeader;
      Write MngFooter;
      Exfmt MngMainScr;

      Select;
         When IndExit = *On Or IndCancel = *On;
            IndCancel = *Off;
            Clear MngErrMsg;
            Clear SMngChoice;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear SMngChoice;

         When SMngChoice = 1;
            CRSubFile(P_UserID);
            Clear SMngChoice;

         When SMngChoice = 2;
            CustomerSubFile();
            Clear SMngChoice;

         When SMngChoice = 3;
            CustAccSubFile();
            Clear SMngChoice;

         When SMngChoice = 4;
            TrHistorySubFile();
            Clear SMngChoice;

         When SMngChoice = 5;
            LoanSubFile();
            Clear SMngChoice;

         When SMngChoice = 6;
            ChangPswd();
            Clear SMngChoice;

         Other;
            Clear SMngChoice;

      EndSl;
   EndDo;
End-Proc;
