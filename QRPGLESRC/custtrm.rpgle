     ** ------------------------------------------------------------------------------
     ** Created By..........: Programmers.io @ 2024
     ** Created Date........: 2024/10/01
     ** Developer...........: Kartik Parihar
     ** Description.........: Module to perform operation on transaction
     ** -------------------------------------------------------------------------------
     ** MODIFICATION LOG:
     ** -------------------------------------------------------------------------------
     ** Date    | Mod_ID  | Developer  | Case and Description
     ** --------|---------|------------|-----------------------------------------------
     ** 24/10/01|         | Kartik P   | Module Creation
     ** -------------------------------------------------------------------------------

     ** Control Option
     HOption(*Nodebugio : *SrcStmt) NoMain

     ** File declaration
     FCRDSBD    CF   E             WorkStn Indds(IndctrDs)
     F                                     Sfile(CUSTTRSFL1:#RRN)
     FJOINACCLF IF   E             Disk
     FAccPf     UF A E           K Disk

     ** Data structure declaration
     DIndctrDs         Ds
     DIndExit                          N   Overlay(Indctrds : 03)
     DIndRefresh                       N   Overlay(Indctrds : 05)
     DIndCancel                        N   Overlay(Indctrds : 12)
     DIndSflDsp                        N   Overlay(Indctrds : 21)
     DIndSflDspCtl                     N   Overlay(Indctrds : 22)
     DIndSflClr                        N   Overlay(Indctrds : 23)
     DIndSflEnd                        N   Overlay(Indctrds : 24)
     DIndOptRI                         N   Overlay(Indctrds : 25)
     DIndOptPC                         N   Overlay(Indctrds : 26)
     DIndFldND                         N   Overlay(Indctrds : 27)
     DIndToAccNoRI                     N   Overlay(Indctrds : 28)
     DIndTrAmtRI                       N   Overlay(Indctrds : 29)

     ** Copy book declaration
     C/Copy KartikCS/Qrpglesrc,Copy_Book

     ** Variable declaration
     D#Rrn             S              4S 0

**Free

// Main Code
Dcl-Proc Transaction Export;
   IndExit     = *Off;
   Dow IndExit = *Off;
       ClearSfl();
       LoadSfl();
       DisplaySfl();

       // Select statement to control the flow of program
       Select;
          When IndExit = *On or IndCancel = *On;
             IndCancel = *Off;
             Clear CrErrMsg;
             Clear CrMainScr;
             IndOptRI = *Off;
             IndOptPC = *Off;
             Leave;

          When Indrefresh = *On;
             Indrefresh   = *Off;
             Clear CrErrMsg;
             Clear S1Option;
             Clear Positionto;
             IndOptRI = *Off;
             IndOptPC = *Off;

          Other;
             OtherOption();
       EndSl;

   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ClearSfl                                                            //
// Description   : Procedure to Clear Customer transaction Subfile                     //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearSfl;
   IndSflClr = *On;
   #Rrn      = 0;
   Write CustTrCtl1;
   IndSflClr = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Customer transaction Subfile                      //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   IndOptPC = *On;

   // Read data from the lf
   Setll *Start JoinAccLf;
   Read JoinAccLf;
   DoW Not %Eof(JoinAccLf);
      S1CName    = CName;
      S1AccNo    = AccNo;
      S1AccType  = AccType;
      S1AccBlnce = AccBlnce;

      If PositionTo <> *Blank and
         %Scan(%Trim(PositionTo) : S1CName) = 0 and
         %Scan(%Trim(PositionTo) : %Char(S1AccNo)) = 0;

      Read JoinAccLf;
      Iter;
      EndIf;

      #Rrn += 1;

      If #Rrn > 9999;
         Leave;
      EndIf;

      Write CustTrSfl1;

      Read JoinAccLf;
   EndDo;

End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Customer Transaction Subfile                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp     = *On;
   IndSflDspCtl  = *On;
   IndSflEnd     = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIf;

   Clear PositionTo;

   CrHeading = '         Work With Transaction        ';
   CrFtrL1   = 'F3=Exit   F5=Refresh   F12=Cancel';
   Write CrHeader;
   Write CrFooter;
   Exfmt CustTrCtl1;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: OtherOption                                                         //
// Description   : Procedure to read sfl for operation on transaction                  //
//------------------------------------------------------------------------------------ //
Dcl-Proc OtherOption;
   ReadC CustTrSfl1;
   DoW S1Option <> 0;
      Select;
         When S1Option = 2;
            WithdrawAmt();
         When S1Option = 3;
            CreditAmt();
         When S1Option = 4;
            TransferAmt();
         Other;
            Clear S1Option;
      EndSl;
      Clear S1Option;
      ReadC CustTrSfl1;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: CreditAmt                                                           //
// Description   : Procedure to Credit amount to the customer account                  //
//------------------------------------------------------------------------------------ //
Dcl-Proc CreditAmt;

   IndFldND   = *On;
   S2AccNO    = S1AccNo;
   S2CName    = S1CName;
   S2AccBlnce = S1AccBlnce;
   S2AccType  = S1AccType;
   CrHeading  = '             Credit Amount              ';
   CrFtrL1    = 'F3=Exit   F5=Refresh   F12=Cancel';

   Dow IndExit = *Off Or IndCancel = *Off;

      Write CrHeader;
      Write CrFooter;
      Exfmt TrOpration;

      Clear CrErrMsg;
      ResetInd();

      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            S1Option  = 0;
            Clear CrErrMsg;
            Clear TrOpration;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear CrErrMsg;
            ResetInd();
            Reset S2TRAMT1;

         When BtnFld = 'S2SEND';
            Validation();
            If CrErrMsg = *Blank;
               PrCreditAmt();
            EndIf;
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ResetInd                                                            //
// Description   : Procedure to turn off all field level indicators                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc ResetInd;
   IndOptRI     = *Off;
   IndToAccNoRI = *Off;
   IndTrAmtRI   = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: Validation                                                          //
// Description   : Procedure to Validate Fields for Customer CRUD Operation            //
//------------------------------------------------------------------------------------ //
Dcl-Proc Validation;
   // Validation for Transaction amount
   If S1Option  = 2 or S1Option = 3;
      If S2TrAmt1  = *Zero;
         IndTrAmtRI = *On;
         CrErrMsg   = 'Transaction amount field cannot be blank';
         Return;
      EndIf;
   EndIf;

   // Validation for withdraw amount
   If S1Option  = 2;
      Chain(N) S1AccNo Accpf;
      If S2TrAmt1 > AccBlnce;
         IndTrAmtRI  = *On;
         CrErrMsg    = 'Insufficient amount available in your account';
         Return;
      EndIf;
   EndIf;

   // Validation for Transfer amount
   If S1Option        = 4;
      If S2ToAccNo1   = *Zero;
         IndToAccNoRI = *On;
         CrErrMsg     = 'Transfer to account number field cannot be blank';
         Return;
      EndIf;
      Chain(N) S2ToAccNo1 AccPf;
      If Not %Found(AccPf);
         IndToAccNoRI = *On;
         CrErrMsg     = 'Invalid account number';
         Return;
      EndIf;
      If S2TrAmt1  = *Zero;
         IndTrAmtRI = *On;
         CrErrMsg   = 'Transaction amount field cannot be blank';
         Return;
      EndIf;
      Chain(N) S1AccNo Accpf;
      If S2TrAmt1 > AccBlnce;
         IndTrAmtRI  = *On;
         CrErrMsg    = 'Insufficient amount available in your account';
         Return;
      EndIf;
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: PrCreditAmt                                                         //
// Description   : Procedure to update Amount details                                  //
//------------------------------------------------------------------------------------ //
Dcl-Proc PrCreditAmt;
   Chain S1AccNo Accpf;
   AccBlnce   = AccBlnce + S2TrAmt1;
   Update AccpfR;
   S2AccBlnce = S2AccBlnce + S2TrAmt1;
   CrErrMsg   = 'Amount Credited Successfully';
   Clear S2TrAmt1;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: WithdrawAmt                                                         //
// Description   : Procedure to Withdraw amount from the customer account           //
//------------------------------------------------------------------------------------ //
Dcl-Proc WithdrawAmt;
   IndFldND   = *On;
   S2AccNO    = S1AccNo;
   S2CName    = S1CName;
   S2AccBlnce = S1AccBlnce;
   S2AccType  = S1AccType;
   CrHeading  = '            Withdraw Amount             ';
   CrFtrL1    = 'F3=Exit   F5=Refresh   F12=Cancel';

   Dow IndExit = *Off Or IndCancel = *Off;

      Write CrHeader;
      Write CrFooter;
      Exfmt TrOpration;

      Clear CrErrMsg;
      ResetInd();

      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            S1Option  = 0;
            Clear CrErrMsg;
            Clear TrOpration;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear CrErrMsg;
            ResetInd();
            Reset S2TRAMT1;

         When BtnFld = 'S2SEND';
            Validation();
            If CrErrMsg = *Blank;
               PrWithdrawAmt();
            EndIf;
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: PrWithdrawAmt                                                       //
// Description   : Procedure to update amount details                                  //
//------------------------------------------------------------------------------------ //
Dcl-Proc PrWithdrawAmt;
   Chain S1AccNo Accpf;
   AccBlnce   = AccBlnce - S2TrAmt1;
   Update AccpfR;
   S2AccBlnce = S2AccBlnce - S2TrAmt1;
   CrErrMsg   = 'Amount Withdrawal Successful';
   Clear S2TrAmt1;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: TransferAmt                                                         //
// Description   : Procedure to transfer amount from the customer account to another   //
//------------------------------------------------------------------------------------ //
Dcl-Proc TransferAmt;
   IndFldND   = *Off;
   S2AccNO    = S1AccNo;
   S2CName    = S1CName;
   S2AccBlnce = S1AccBlnce;
   S2AccType  = S1AccType;
   CrHeading  = '            Transfer Amount             ';
   CrFtrL1    = 'F3=Exit   F5=Refresh   F12=Cancel';
   S2ToAccNo  = 'Tranfer to account no .';
   Dow IndExit = *Off Or IndCancel = *Off;

      Write CrHeader;
      Write CrFooter;
      Exfmt TrOpration;

      Clear CrErrMsg;
      ResetInd();

      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            S1Option  = 0;
            Clear CrErrMsg;
            Clear TrOpration;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear CrErrMsg;
            ResetInd();
            Reset S2TrAmt1;
            Reset S2ToAccNo1;

         When BtnFld = 'S2SEND';
            Validation();
            If CrErrMsg = *Blank;
               PrTransferAmt();
            EndIf;
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: PrTransferAmt                                                       //
// Description   : Procedure to update amount details                                  //
//------------------------------------------------------------------------------------ //
Dcl-Proc PrTransferAmt;
   Chain S2ToAccNo1 AccPf;
   AccBlnce = AccBlnce + S2TrAmt1;
   Update AccPfr;

   Chain S1AccNo Accpf;
   AccBlnce   = AccBlnce - S2TrAmt1;
   Update AccpfR;
   S2AccBlnce = S2AccBlnce - S2TrAmt1;
   CrErrMsg   = 'Amount Transferred Successful';
   Clear S2TrAmt1;
   Clear S2ToAccNo1;
End-Proc;
