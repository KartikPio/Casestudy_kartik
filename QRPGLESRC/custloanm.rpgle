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

     ** Control option
     HOption(*Nodebugio : *SrcStmt) NoMain

     ** File declaration
     FCRDSBD    CF   E             WORKSTN Indds(IndctrDs)
     F                                     Sfile(LOANSFL01:#RRN)
     FJOINLOANLFIF   E             Disk
     FJOINVLDLF IF   E           K Disk
     FLOANPF    O    E           K Disk

     ** Data structure declaration
     DIndctrDs         DS
     DIndExit                          N   Overlay(IndctrDs : 03)
     DIndPrompt                        N   Overlay(IndctrDs : 04)
     DIndRefresh                       N   Overlay(IndctrDs : 05)
     DIndInsert                        N   Overlay(IndctrDs : 06)
     DIndConfirm                       N   Overlay(IndctrDs : 07)
     DIndCancel                        N   Overlay(IndctrDs : 12)
     DIndSflDsp                        N   Overlay(Indctrds : 21)
     DIndSflDspCtl                     N   Overlay(Indctrds : 22)
     DIndSflClr                        N   Overlay(Indctrds : 23)
     DIndSflEnd                        N   Overlay(Indctrds : 24)
     DIndOptRI                         N   Overlay(Indctrds : 25)
     DIndOptPC                         N   Overlay(Indctrds : 26)
     DIndNameRI                        N   Overlay(Indctrds : 27)
     DIndAccNoRI                       N   Overlay(Indctrds : 28)
     DIndIncomeRI                      N   Overlay(Indctrds : 29)
     DIndLoanTypeRI                    N   Overlay(Indctrds : 30)
     DIndLoanDurRI                     N   Overlay(Indctrds : 31)
     DIndLoanAmtRI                     N   Overlay(Indctrds : 32)
     DIndChoiceRI                      N   Overlay(Indctrds : 33)
     ** Copy book declaration
     C/Copy KartikCS/Qrpglesrc,Copy_Book

     ** Variable declaration
     D#Rrn             S              4S 0

**Free

// Main Code
Dcl-Proc ApplyLoan Export;
   IndExit = *Off;
   Dow IndExit = *Off;
      ClearSfl();
      LoadSfl();
      DisplaySfl();

      //Contro the flow of the program
      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            Clear CrErrMsg;
            Clear CrMainScr;
            IndOptRI = *Off;
            IndOptPC = *Off;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear CrErrMsg;
            Clear S1Option;
            Clear Positionto;
            IndOptRI = *Off;
            IndOptPC = *Off;

         When IndInsert = *On;
            IndInsert   = *Off;
            Clear CrErrMsg;
            NewLoan();
         Other;
            //OtherOption();
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ClearSfl                                                            //
// Description   : Procedure to Clear Loan Subfile                                     //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearSfl;
   IndSflClr = *On;
   #Rrn      = 0;
   Write LoanCtl01;
   IndSflClr = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Loan Subfile                                      //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   IndOptPC = *On;

   // Read data from the Lf
   Setll *Start JoinLoanLf;
   Read JoinLoanLf;
   DoW Not %Eof(JoinLoanLf);
      S1CstAccNo = AccNo;
      S1CName    = CName;
      S1LType    = LType;
      S1LAmnt    = LAmnt;
      S1LStatus  = LStatus;
      LOANSTATUS = 'Loan status:';
      #Rrn += 1;

      If #Rrn > 9999;
         Leave;
      EndIf;

      Write LoanSfl01;

      Read JoinLoanLf;
   EndDo;

End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Loan Subfile                                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp     = *On;
   IndSflDspCtl  = *On;
   IndSflEnd     = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIf;

   Clear PositionTo;

   CrHeading = '             Work With Loan             ';
   CrFtrL1   = 'F3=Exit   F5=Refresh   F6=Apply for loan   F11=Drop   F12=Cancel';
   Write CrHeader;
   Write CrFooter;
   Exfmt LoanCtl01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: NewLoan                                                             //
// Description   : Procedure to apply for a new loan                                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc NewLoan;
   CrHeading = '            Apply for Loan            ';
   CrFtrL1   = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';

   DoW IndExit = *Off or IndCancel = *Off;
      Write CrHeader;
      Write CrFooter;
      Exfmt ApplyLoans;
      Clear CrErrMsg;
      ResetInd();

      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            Clear CrErrMsg;
            Clear ApplyLoans;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear CrErrMsg;
            Clear ApplyLoans;
            ResetInd();

         When PrmtFld = 'S2LTYPE' And IndPrompt = *On;
            IndPrompt = *Off;
            GetLoanType();

         When IndConfirm = *On;
            IndConfirm   = *Off;
            LoanVld();
            If CrErrMsg = *Blank;
               InsertRec();
            EndIf;

         Other;
            LoanVld();
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ResetInd                                                            //
// Description   : Procedure to turn off all field level indicators                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc ResetInd;
   IndOptRI       = *Off;
   IndNameRI      = *Off;
   IndAccNoRI     = *Off;
   IndIncomeRI    = *Off;
   IndLoanTypeRI  = *Off;
   IndLoanDurRI   = *Off;
   IndLoanAmtRI   = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: GetLoanType                                                         //
// Description   : Procedure to Select Loan type                                       //
//------------------------------------------------------------------------------------ //
Dcl-Proc GetLoanType;
   DoW IndCancel = *Off;
      Exfmt LoanTypeW;

      If IndCancel = *On;
         IndCancel = *Off;
         Clear W1Choice;
         Clear W1ErrorMsg;
         IndChoiceRI = *Off;
         Leave;
      Else;
         Select;
            When W1Choice = 1;
               S2LType = 'PERSONAL';
               Clear W1ErrorMsg;
            When W1Choice = 2;
               S2LType = 'HOME';
               Clear W1ErrorMsg;
            When W1Choice = 3;
               S2LType = 'EDUCATION';
               Clear W1ErrorMsg;
            Other;
               IndChoiceRI = *On;
               W1ErrorMsg  = 'Invalid Opt';
         EndSl;
         If W1ErrorMsg = *Blank;
            IndChoiceRI = *Off;
            Clear W1Choice;
            Leave;
         EndIf;
      EndIf;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoanVld                                                             //
// Description   : Procedure to Validate Fields for Apply loan                         //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoanVld;
  // Validation for Account number
   If S2CstAccNo = *Zero;
      IndAccNoRI = *On;
      CrErrMsg   = 'Accont Number field cannot be blank';
      Return;
   EndIf;
   Chain S2CstAccNo JoinVldLf;
   If AccNo     <> S2CstAccNo;
      IndAccNoRI = *On;
      CrErrMsg   = 'Invalid Account Number';
      Return;
   EndIf;

  // Validation for Customer name
   If S2CName   = *Blank;
      IndNameRI = *On;
      CrErrMsg  = 'Customer Name field Cannot be blank';
      Return;
   EndIf;
   If CName    <> S2CName;
      IndNameRI = *On;
      CrErrMsg  = 'Invalid Customer name';
      Return;
   EndIf;

  // Validation for Customer income
   If S2CIncome   = *Zero;
      IndIncomeRI = *On;
      CrErrMsg    = 'Customer income field cannot be blank';
      Return;
   EndIf;

  // Validation for Loan type
   If S2LType       = *Blank;
      IndLoanTypeRI = *On;
      CrErrMsg      = 'Loan type field cannot be blank';
      Return;
   EndIf;

  // Validation for Loan dureation
   If S2LTerm      = *Zero;
      IndLoanDurRI = *On;
      CrErrMsg     = 'Loan duration field cannot be blank';
      Return;
   EndIf;

  // Validation for Loan amount
   If S2LAmnt      = *Zero;
      IndLoanAmtRI = *On;
      CrErrMsg     = 'Loan amount field cannot be blank';
      Return;
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: InsertRec                                                           //
// Description   : Procedure to Write Pf from Display file                             //
//------------------------------------------------------------------------------------ //
Dcl-Proc InsertRec;
   LId       = %Int(%Subst(%Char(S2CstAccNo):6)+%SubSt(%Char(%TimeStamp():*Iso0):15));
   S2LId     = LId;
   CustAccNo = S2CstAccNo;
   CIncome   = S2CIncome;
   LType     = S2LType;
   LAmnt     = S2LAmnt;
  // Set Interest rate
   Select;
      When  S2LType = 'PERSONAL';
         LIRate     = 10.00;
      When  S2LType = 'HOME';
         LIRate     = 8.50;
      When  S2LType = 'EDUCATION';
         LIRate     = 7.50;
   EndSl;
   LTerm     = S2LTerm;
   LStatus   = 'PENDING';
   Write LoanPfR;
   CrErrMsg  = 'Loan applied successfully';
End-Proc;
