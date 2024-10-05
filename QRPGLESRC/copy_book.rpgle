**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/07/31                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Copybook                                                  //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/07/31|         | Kartik P   | Copybook Creation                              //
// ------------------------------------------------------------------------------- //

Dcl-Pr MngDsbrd;
   P_UserId Char(10);
End-Pr;

Dcl-Pr  CRSubFile;
   P_UserId Char(10);
End-Pr;

Dcl-Pr StateCityPrmpt Char(15);
   PState Char(15);
End-Pr;

Dcl-Pr CustomerSubFile;
End-Pr;

Dcl-Pr CustAccSubFile;
End-pr;

Dcl-Pr TrHistorySubFile;
End-Pr;

Dcl-Pr LoanSubFile;
End-Pr;

Dcl-Pr ChangPswd;
End-Pr;

Dcl-Pr CrDshbrd;
   P_UserId Char(10);
End-Pr;

Dcl-Pr DisplayCr;
   P_UserId Char(10);
End-Pr;

Dcl-Pr Transaction;
End-Pr;

Dcl-Pr ApplyLoan
End-Pr;
