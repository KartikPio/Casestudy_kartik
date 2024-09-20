**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/09/19                                               //
// Developer...........: Kartik Parihar                                            //
// Description.........: Subfile for Loan Details                           //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/09/19|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt) NoMain;

// File Declaration
Dcl-F MngDsbD WorkStn Indds(IndicatorDs) Sfile(LoanSfl01 : #Rrn);

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
   IndLTermPC      Ind Pos(27);
   IndSdatePC      Ind Pos(28);
   IndChoiceRI     Ind Pos(30);
   IndFieldPR      Ind Pos(98);
End-Ds;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-S #Rrn  Zoned(4)  Inz(*Zero);
Dcl-S Stmt  Char(100) Inz(*Blank);
Dcl-C QT    Const('''');

// Main Code
Dcl-Proc LoanSubFile Export;
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
// Description   : Procedure to Clear Loan Subfile                                     //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearSfl;
   IndSflClr = *On;
   #Rrn      = 0;
   Write loanCtl01;
   IndSflClr = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Transaction History Subfile                       //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   IndOptPC = *On;
   Clear Stmt;
   Stmt = 'Select CustAccNo, LType, LAmnt, LStatus From LoanPf' ;

   If S1Position <> *Blank;
      Stmt = %Trim(Stmt) + ' Where CustAccNo Like ' + QT + '%' + %Trim(S1Position) + '%' + QT ;
   EndIf;

   Exec Sql
      Prepare SqlStmt From :Stmt;

   Exec Sql
      Declare C01 Cursor for Sqlstmt;

   Exec Sql
      Open C01;

   Exec Sql
      Fetch from C01 Into :S1CstAccNo, :S1LType, :S1LAmnt, :S1LStatus;

   Dow SqlCode = 0;
      #Rrn += 1;

      If #Rrn > 9999;
         Leave;
      EndIF;

      Write LoanSfl01;

      Exec Sql
         Fetch from C01 Into :S1CstAccNo, :S1LType, :S1LAmnt, :S1LStatus;

   EndDo;

   Exec Sql
      Close C01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Loan Subfile                                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp    = *On;
   IndSflDspCtl = *On;
   IndSflEnd    = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIf;
   Clear S1Position;
   MngHdr   = '        Display Loan Details         ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F12=Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt LoanCtl01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: OtherOption                                                         //
// Description   : Procedure to read Sfl to work on Loan Request                       //
//------------------------------------------------------------------------------------ //
Dcl-Proc OtherOption;
   ReadC LoanSfl01;
   DoW S1Option <> 0;
      Select;
         When S1Option = 2;
            WrokWthLoan();
         When S1Option = 5;
            DisplayLoan();
         Other;
            Clear S1Option;
      EndSl;
      Clear S1Option;
      ReadC LoanSfl01;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: UpdateCust                                                          //
// Description   : Procedure to update customer details                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc WrokWthLoan;

   LoadUpdateScr();

   MNGHDR   = '    Approve/Deny Loan Request     ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';

   Dow IndExit = *Off Or IndCancel = *Off;

      Write MngHeader;
      Write MngFooter;
      Exfmt LoanOprtn;

      Clear MngErrMsg;
      ResetInd();

      Select;
         When IndExit = *On Or IndCancel = *On;
            IndCancel = *Off;
            S1Option  = 0;
            Clear MngErrMsg;
            Leave;

         When IndRefresh = *On;
            IndRefresh = *Off;
            clear MngErrMsg;
            ResetInd();
            LoadUpdateScr();

         When PrmtFld = 'S2LSTATUS' And IndPrompt = *On;
            IndPrompt = *Off;
            GetLoanStatus();

         When IndConfirm = *On;
            IndConfirm   = *Off;
            LoanValidation();
            If MngErrMsg = *Blank;
               UpdateRec();
            EndIf;
            LoanValidation();
         Other;

      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadUpdateScr                                                       //
// Description   : Procedure to Load Update Screen for loan detail                     //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadUpdateScr;
    Exec Sql
      Select C.CName, L.CustAccNo, L.CIncome, L.LId, L.LType, L.LIRate, L.LTerm,
      L.LAmnt, L.AmntoPay, L.LSDate, L.LEDate, L.LMPaymnt, L.LRAmount, L.LStatus
      Into :S2CstName, :S2CstAccNo, :S2CIncome, :S2LId, :S2LType, :S2LIRate, :S2LTerm,
      :S2LAmnt, :S2AmntToPy, :S2LSDate, :S2LEDate, :S2LMPaymnt, :S2LRAmount, :S2LStatus
      From Cust C
      Join AccPf A
      On C.CId = A.CustId
      Join LoanPf L
      On A.AccNo = L.CustAccNo
      Where CustAccNo = :S1CstAccNo;

   If SqlCode <> 0;
   Exec Sql
      Select C.CName, L.CustAccNo, L.CIncome, L.LId, L.LType,
      L.LIRate, L.LAmnt, L.LStatus
      Into :S2CstName, :S2CstAccNo, :S2CIncome, :S2LId, :S2LType,
      :S2LIRate, :S2LAmnt, :S2LStatus
      From CustPf C
      Join AccPf A
      On C.CId = A.CustId
      Join LoanPf L
      On A.AccNo = L.CustAccNo
      Where CustAccNo = :S1CstAccNo;
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: GetAccStatus                                                          //
// Description   : Procedure to Select Account type                                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc GetLoanStatus;
   Dow IndCancel = *Off;
      Exfmt AccStatsWn;

      If IndCancel = *On;
         IndCancel = *Off;
         Clear W2Choice;
         Clear W2ErrorMsg;
         IndChoiceRI = *Off;
         Leave;
      Else;
         Select;
            When W2Choice = '1';
               S2LStatus = 'PENDING';
               Clear W2ErrorMsg;
            When W2Choice = '2';
               S2LStatus  = 'APPROVED';
               Clear W2ErrorMsg;
             When W2Choice = '3';
               S2LStatus  = 'DENY';
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
// Procedure Name: ResetInd                                                            //
// Description   : Procedure to turn off all field level indicators                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc ResetInd;
   IndOptRI        = *Off;
   IndOptPC        = *Off;
   IndLTermPC      = *Off;
   IndSdatePC      = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoanValidation                                                      //
// Description   : Procedure to Validate Fields for Loan Update Operation              //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoanValidation;
  //Validation for Loan Duration
  If S2LTerm    = *Zero;
     IndLTermPc = *On;
     MngErrMsg  = 'Loan duration field cannot be blank';
     Return;
   EndIf;

   // Validation for Loan starting date
   // If %Char(S2LSDate) = '0001-01-01'
   //    IndSdatePc      = *On;
   //    MngErrMsg       = 'Loan Starting date cannot be blank';
   //    Return;
   // EndIf;

   // If %Diff(%Date() : S2LSDate : *Y) < 0;
   //
   // EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name:  UpdateRec;                                                         //
// Description   : Procedure to update Loan details                                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc  UpdateRec;

   Dcl-S Principle Zoned(15)  Inz(*Zero);
   Dcl-S IntRate   Zoned(5:2) Inz(*Zero);
   Dcl-S Time      Zoned(3)   Inz(*Zero);

   If S2LStatus = 'DENY';
      Exec Sql
         Update LoanPf Set LStatus = :S2LStatus
      Where CustAccNo = :S1CstAccNo;

   Else;

   Exec Sql
      Select LAmnt, LIRate, LTerm
      Into :Principle, :IntRate, :Time
      From LoanPf
      Where CustAccNo = :S1CstAccNo;

   S2AmntToPy = Principle * ((1 + (IntRate / Time)) ** Time);
   S2LSDate   = %Date();
   S2LEDate   = S2LSDate + %Years(S2LTerm);
   S2LMPayMnt = S2AmntToPy / (S2LTerm * 12);
   S2LRAmount = S2AmntToPy - (%Diff(%Date() : S2LSDate : *M) * S2LmPaymnt);

   Exec Sql
      Update LoanPf Set LTerm = :S2LTerm, AmntoPay = :S2AmntToPy, LSDate = :S2LSDate,
      LEDate = :S2LEDate, LMPaymnt = :S2LMPaymnt,  LRAmount = :S2LRAmount, LStatus = :S2LStatus
      Where CustAccNo = :S1CstAccNo;

   EndIf;
   MngErrMsg = 'Record Updated Successfully';
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplayCust                                                         //
// Description   : Procedure to Display Loan Details                                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplayLoan;
   LoadUpdateScr();

   MNGHDR     = 'Display Loan Details';
   MngFtrL2   = 'F3=Exit   F12=Cancel';
   IndFieldPR = *On;

   Dow IndExit = *Off And IndCancel = *Off;
      Write MngHeader;
      Write MngFooter;
      Exfmt LoanOprtn;

      If IndExit = *On or IndCancel = *On;
         IndCancel = *Off;
         IndFieldPR = *Off;
         Clear S1Option;
         Leave;
      EndIf;
   EndDo;
End-Proc;
