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
                                         Sfile(CustDltSfl : #Rrn1);
// Data Sturcture Declaration
Dcl-Ds IndicatorDs;
   IndExit         Ind Pos(03);
   IndPrompt       Ind Pos(04);
   IndRefresh      Ind Pos(05);
   IndInsert       Ind Pos(06);
   IndConfirm      Ind POs(07);
   IndCancel       Ind Pos(12);
   IndSflDsp       Ind Pos(21);
   IndSflDspCtl    Ind Pos(22);
   IndSflClr       Ind Pos(23);
   IndSflEnd       Ind Pos(24);
   IndOptRI        Ind Pos(25);
   IndOptPC        Ind Pos(26);
   IndCNameRI      Ind Pos(31);
   IndCGender      Ind Pos(27);
   IndCDobRI       Ind Pos(32);
   IndCAdharRI     Ind Pos(33);
   IndCPanRI       Ind Pos(34);
   IndCurAddRI     Ind Pos(35);
   IndPrmAddRI     Ind Pos(36);
   IndCStateRI     Ind Pos(37);
   IndCCityRI      Ind Pos(38);
   IndCMobRI       Ind Pos(39);
   IndCAltMobRI    Ind Pos(40);
   IndCEmailRI     Ind Pos(41);
   IndNmneeNameRI  Ind Pos(52);
   IndNmneeMobRI   Ind Pos(53);
   IndNmnAdharRI   Ind Pos(54);
   IndAccTypeRI    Ind Pos(46);
   IndChoiceRI     Ind Pos(47);
   IndDltSflDsp    Ind Pos(42);
   IndDltSflDspCtl Ind Pos(43);
   IndDltSlfClr    Ind Pos(44);
   IndDltSflEnd    Ind Pos(45);
   IndFieldPR      Ind Pos(98);
End-Ds;

Dcl-Ds CustDetails;
   S2CId         Char(10)  Inz;
   S2CName       Char(20)  Inz;
   S2CGender1    Char(6)   Inz;
   S2CDob        Date      Inz;
   S2CMob        Zoned(10) Inz;
   S2CAltMob     Zoned(10) Inz;
   S2CAdhar      Zoned(12) Inz;
   S2CPan        Char(10)  Inz;
   S2CEmail      Char(50)  Inz;
   S2CCrntAdr    Char(50)  Inz;
   S2CPermAdr    Char(50)  Inz;
   S2CState      Char(15)  Inz;
   S2CCity       Char(15)  Inz;
   S2CPin        Zoned(6)  Inz;
   S2NmneeNm     Char(20)  Inz;
   S2NmneeMob    Zoned(10) Inz;
   S2NmnAdhar    Zoned(12) Inz;
   S2CAddTmStmt  Timestamp Inz;
   S2CUpdTmStmt  Timestamp Inz;
End-DS;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-S #Rrn         Zoned(4) Inz(*Zero);
Dcl-S #Rrn1        Like(#Rrn);
Dcl-S CIdSubfix    Zoned(6) Inz(*Zero);
Dcl-S Cnt          Zoned(5) Inz(*Zero);
Dcl-S Idx          Zoned(5) Inz(*Zero);
Dcl-S Idx1         Zoned(4) Inz(1);
Dcl-S PCId         Char(10) Inz(*Blank);
Dcl-S S2CId1       Like(S2CId);
Dcl-S Stmt         Char(200)Inz(*Blank);
Dcl-S GetState     Char(15) Inz(*Blank);
Dcl-S ArrCId       Char(20) Dim(9999);
Dcl-S Deleteflag   Ind      Inz(*Off);
Dcl-C QT           Const('''');
DCl-C CapsAlpha    Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ ');
Dcl-C Regex1       Const('^(?:\w+\.?)*\w+@(?:\w+\.)*\w+(?:\s+\.?)*$');

// Main Code
Dcl-Proc CustomerSubFile Export;
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
   Clear Stmt;
   Stmt = 'Select CId, CName, CPan, CMob From CustPF';

   If S1Position <> *Blank;
      Stmt = %Trim(Stmt) + ' Where CId Like ' + QT + '%' + %Trim(S1Position) + '%' + QT +
             ' Or CName Like ' + QT + '%' + %Trim(S1Position) + '%' + QT;
   EndIf;

   Exec Sql
      Prepare SqlStmt From :Stmt;

   Exec Sql
      Declare C01 Cursor for Sqlstmt;

   Exec Sql
      Open C01;

   Exec Sql
      Fetch From C01 Into :S1Cid, :S1Cname, :S1CPan, :S1CMob;

   Dow SqlCode = 0;
      #Rrn += 1;

      If #Rrn > 9999;
         Leave;
      EndIF;

      Write CustSflM01;

      Exec Sql
         Fetch From C01 Into :S1Cid, :S1Cname, :S1CPan, :S1CMob;
   EndDo;

   Exec Sql
      Close C01;
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
   Clear S1Position;
   MngHdr   = '       Work With Customer Details       ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F6=Add New Record   F12= Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt CustCtlM01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: InsertNewRec                                                        //
// Description   : Procedure to Insert New Customer Details                            //
//------------------------------------------------------------------------------------ //
Dcl-Proc InsertNewRec;
   MNGHDR   = '         Add New Customer Details        ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';
   Reset CustDetails;

   Exec Sql
      Select Max(CId) Into :PCId From CustPf;

   If PCId  = *Blank;
      S2CId = 'CUST000001';
   Else;
      CIdSubFix = %Int(%SubSt(PCId : 5)) + 1;
      S2CId     = 'CUST' + %EditC(CIdSubfix : 'X');
      S2CId1    = S2CId;
   EndIf;

   DoW IndExit = *Off Or IndCancel = *Off;

      Write MngHeader;
      Write MngFooter;
      Exfmt MngCurdCst;
      Clear MngErrMsg;
      ResetInd();

      Select;
         When IndExit = *on Or IndCancel = *On;
         IndCancel = *Off;
         Clear MngErrMsg;
         Clear MngCurdCst;
         Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear MngErrMsg;
            Clear MngCurdCst;
            S2CId = S2CId1;

         When PrmtFld = 'S2CSTATE' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = 'STATE';
            S2CState  = StateCityPrmpt(GetState);

         When PrmtFld ='S2CCITY' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = S2CState;
            S2CCity   = StateCityPrmpt(GetState);

            If S2CCity <> ' ';
               Exec Sql
                 Select Pincode Into :S2CPin
                 From StatePf
                 Where City = :S2CCity;
            EndIf;

         When PrmtFld = 'S2ACCTYPE' And IndPrompt = *On;
            IndPrompt = *Off;
            GetAccType();

         When IndConfirm = *On;
            IndConfirm   = *Off;
            MngCurdVld();
            If MngErrMsg = *Blank;
               InsertRec();
            EndIF;

         Other;
            MngCurdVld();
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ResetInd                                                            //
// Description   : Procedure to turn off all field level indicators                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc ResetInd;
   IndOptRI       = *Off;
   IndCNameRI     = *Off;
   IndCDobRI      = *Off;
   IndCAdharRI    = *Off;
   IndCPanRI      = *Off;
   IndCurAddRI    = *Off;
   IndPrmAddRI    = *Off;
   IndCStateRI    = *Off;
   IndCCityRI     = *Off;
   IndCMobRI      = *Off;
   IndCAltMobRI   = *Off;
   IndCEmailRI    = *Off;
   IndNmneeNameRI = *Off;
   IndNmneeMobRI  = *Off;
   IndNmnAdharRI  = *Off;
   IndAccTypeRI   = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: GetAccType                                                          //
// Description   : Procedure to Select Account type                                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc GetAccType;
   Dow IndCancel = *Off;
      Exfmt AccTypWndw;

      If IndCancel = *On;
         IndCancel   = *Off;
         Clear W1Choice;
         Clear W1ErrorMsg;
         IndChoiceRI = *Off;
         Leave;
      Else;
         Select;
            When W1Choice = '1';
               S2AccType  = 'SAVING';
               Clear W1ErrorMsg;
            When W1Choice = '2';
               S2AccType  = 'CURRENT';
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
// Procedure Name: InsertRec                                                           //
// Description   : Procedure to Write Pf from Display file                             //
//------------------------------------------------------------------------------------ //
Dcl-Proc InsertRec;
   Dcl-S PAccNo Zoned(11) Inz(*Zero);
   Clear S2CUpdTmStmt;
   S2CAddTmStmt = %TimeStamp();
   Select;
      When S2CGender = 'M';
         S2CGender1  = 'Male';
      When S2CGender = 'F';
         S2CGender1  = 'Female';
      When S2CGender = 'O';
         S2CGender1  = 'Other';
   EndSl;

   Exec Sql
      Insert Into CustPf
      Values(:CustDetails);

   Exec Sql
      Insert Into LoginPf(UserId, UserPass)
      Values(:S2CId, 'WELCOME');

   PAccNo = %Dec(%SubSt(%Char(%TimeStamp() : *Iso0) : 14) + %SubSt(%Char(S2CADHAR) : 9) : 11 : 0);

   Exec Sql
      Insert Into AccPf(CustId, AccNo, AccType, AccStatus)
      Values(:S2CId, :PAccNo, :S2AccType, 'PENDING');

   Exec Sql
      Insert Into TrHstryPf(CACCNO)
      Values(:PAccNo);

   Exec Sql
      Insert Into LoanPf(CUSTACCNO)
      Values(:PAccNo);

   Clear MngCurdCst;
   Exec Sql
      Select Max(CId) Into :PCId From CustPf;

   CIdSubFix = %Int(%SubSt(PCId : 5)) + 1;
   S2CId     = 'CUST' + %EditC(CIdSubfix : 'X');
   MngErrMsg = 'Record inserted successfully';
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: MngCurdVld                                                          //
// Description   : Procedure to Validate Fields for Customer CRUD Operation            //
//------------------------------------------------------------------------------------ //
Dcl-Proc MngCurdVld;
   Dcl-S Email Varchar(100) Inz(*Blank);

   // Validation for Customer name
   If S2CName    = *Blank;
      IndCNameRI = *On;
      MngErrMsg   = 'Name field cannot be blank';
      Return;
   EndIf;
   If %Check(CapsAlpha : S2CName) <>0;
      IndCNameRI = *On;
      MngErrMsg   = 'Name field cannot contain number and special character';
      Return;
   EndIf;

  // Validation for Customer gender
  If S2CGender   = ' ';
     IndCGender  = *On;
     MngErrMsg   = 'Gender cannot be blank';
     Return;
  EndIf;

  // Validation for Customer date of birth
  If %Char(S2CDob) = '0001-01-01';
     IndCDobRI     = *On;
     MngErrMsg     = 'Date of Birth field cannot be blank';
     Return;
  EndIf;
  If %Diff(%Date() : S2CDob : *Years) < 0;
     IndCDobRI  = *On;
     MngErrMsg  = 'Age cannot be less then current date';
     Return;
  EndIf;
  If %Diff(%Date() : S2CDob : *Months) < 0;
     IndCDobRI  = *On;
     MngErrMsg  = 'Age cannot be less then current date';
     Return;
  EndIf;
  If %Diff(%Date() : S2CDob : *Days) < 0;
     IndCDobRI  = *On;
     MngErrMsg  = 'Age cannot be less then current date';
     Return;
  EndIf;

  // Validation for Customer adhaar number;
  If S2CAdhar    = *Zero;
     IndCAdharRI = *On;
     MngErrMsg  = 'Adhaar Number field cannot be blank';
     Return;
  EndIf;
  If %Len(%Trim(%Char(S2CAdhar))) < 12;
     IndCAdharRI = *On;
     MngErrMsg  = 'Adhaar Number should have 12 digit';
     Return;
  EndIf;

  // Validation for Customer PAN number;
  If S2CPan    = *Blank;
     IndCPanRI = *On;
     MngErrMsg  = 'PAN Number field cannot be blank';
     Return;
  EndIf;
  If %Len(%Trim(S2CPan)) < 10;
     IndCPanRI = *On;
     MngErrMsg  = 'PAN Number should have 10 character';
     Return;
  EndIf;

   // Validation for Customer Current Address
   If S2CCrntAdr  = *Blank;
      IndCurAddRI = *On;
      MngErrMsg   = 'Current Address cannot be blank';
      Return;
   EndIf;

   // Validation for Customer Permanent Address
   If S2CPermAdr   = *Blank;
      IndPrmAddRI  = *On;
      MngErrMsg   = 'Permanent Address cannot be blank';
      Return;
   EndIf;

   // Validation for Customer State
   If S2CState    = *Blank;
      IndCStateRI = *On;
      MngErrMsg   = 'State field cannot be blank';
      Return;
   EndIf;

   // Validation for Customer City
   If S2CCity    = *Blank;
      IndCCityRI = *On;
      MngErrMsg  = 'City field cannot be blank';
      Return;
   EndIf;

   // Validation for Customer Mobile Number
   If S2CMob    = *Zero;
      IndCMobRI = *On;
      MngErrMsg = 'Mobile Number field cannot be blank';
      Return;
   EndIf;
   If %Len(%Trim(%Char(S2CMob))) < 10;
      IndCMobRI = *On;
      MngErrMsg = 'Mobile Number should have 10 digit';
      Return;
   EndIf;
   // Validation for Customer Alternate Mobile Number
   If S2CAltMob    = *Zero;
      IndCAltMobRI = *On;
      MngErrMsg    = 'Alternate Mobile Number field cannot be blank';
      Return;
   EndIf;
   If %Len(%Trim(%Char(S2CAltMob))) < 10;
      IndCAltMobRI = *On;
      MngErrMsg    = 'Alternate Mobile Number should have 10 digit';
      Return;
   EndIf;

   // Validation for Customer Email Address
   If S2CEmail    = *Blank;
      IndCEmailRI = *On;
      MngErrMsg   = 'Email Address Cannot be blank';
      Return;
   EndIf;

   Email = S2CEmail;
   Exec Sql
      Set :Cnt = RegExp_Count(:Email, :Regex1);
   If Cnt <> 1;
      IndCEmailRI = *On;
       MngErrMsg  = 'Invalid Email Address';
      Return;
   EndIf;

    // Validation for nominee name
    If S2NmneeNm      = *Blank;
       IndNmneeNameRI = *On;
       MngErrMsg   = 'Nominee Name field cannot be blank';
       Return;
   EndIf;
   If %Check(CapsAlpha : S2NmneeNm) <> 0;
      IndNmneeNameRI = *On;
      MngErrMsg   = 'Nominee Name field cannot contain number and special character';
      Return;
   EndIf;

   // Validation for Nominee Mobile Number
   If S2NmneeMob    = *Zero;
      IndNmneeMobRI = *On;
      MngErrMsg = 'Nominee Mobile Number field cannot be blank';
      Return;
   EndIf;

   If %Len(%Trim(%Char(S2NmneeMob))) < 10;
      IndNmneeMobRI = *On;
      MngErrMsg = 'Nominee Mobile Number should have 10 digit';
      Return;
   EndIf;

    // Validation for Nominee adhaar number;
   If S2NmnAdhar    = *Zero;
      IndNmnAdharRI = *On;
      MngErrMsg  = 'Nominee Adhaar Number field cannot be blank';
      Return;
   EndIf;
   If %Len(%Trim(%Char(S2NmnAdhar))) < 12;
      IndNmnAdharRI = *On;
      MngErrMsg  = 'Nominee Adhaar Number should have 12 digit';
      Return;
   EndIf;

   // Validation For Account type;
   If S2AccType    = *Blank;
      IndAccTypeRI = *On;
      MngErrMsg    = 'Account Type field cannot be blank';
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: OtherOption                                                         //
// Description   : Procedure to read sfl for other crud operation                      //
//------------------------------------------------------------------------------------ //
Dcl-Proc OtherOption;
   ReadC CustSflM01;
   DoW S1Option <> 0;
      Select;
         When S1Option   = 2;
            UpdateCust();
         When S1Option   = 4;
            ArrCId(Idx1) = S1CId;
            Idx1 += 1;
            DeleteFlag   =  *On;
         When S1Option   = 5;
            DisplayCust();
         Other;
            Clear S1Option;
      EndSl;
      Clear S1Option;
      ReadC CustSflM01;
   EndDo;
   If DeleteFlag = *On;
      DeleteCust();
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: UpdateCust                                                          //
// Description   : Procedure to update customer details                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc UpdateCust;
   Clear S2CId1;
   S2CId1 = S1CId;
   Reset CustDetails;
   Exec Sql
      Select * Into :CustDetails
      From CustPf Where CId = :S2CId1;

   Select;
      When S2CGender1  = 'Male';
         S2CGender = 'M';
      When S2CGender1  = 'Female';
         S2CGender = 'F';
      When S2CGender1  = 'Other';
         S2CGender = 'O';
   EndSl;

   MNGHDR   = '         Update Customer Details        ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';

   Dow IndExit = *Off Or IndCancel = *Off;

      Write MngHeader;
      Write MngFooter;
      Exfmt MngCurdCst;

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
            Clear MngErrmsg;
            ResetInd();
            Reset CustDetails;
            Exec Sql
               Select * Into :CustDetails
               From CustPf Where CId = :S2CId1;

          When PrmtFld = 'S2CSTATE' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = 'STATE';
            S2CState  = StateCityPrmpt(GetState);

         When PrmtFld ='S2CCITY' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = S2CState;
            S2CCity   = StateCityPrmpt(GetState);
            If S2CCity <> ' ';
               Exec Sql
                 Select Pincode Into :S2CPin
                 From StatePf
                 Where City = :S2CCity;
            EndIf;

         When IndConfirm = *On;
            IndConfirm   = *Off;
            MngCurdVld();
            If MngErrMsg = *Blank;
              UpdateRec();
            EndIf;

         Other;
            MngCurdVld();
      EndSl;
   EndDo;
End-Proc;

Dcl-Proc UpdateRec;
   S2CUpdTmStmt = %TimeStamp();

   Select;
      When S2CGender = 'M';
         S2CGender1  = 'Male';
      When S2CGender = 'F';
         S2CGender1  = 'Female';
      When S2CGender = 'O';
         S2CGender1  = 'Other';
   EndSl;

   Exec Sql
      Update CustPF
      Set Row = :CustDetails Where CId = :S2CId1;

   MngErrMsg = 'Record Updated Successfully';
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplayCust                                                         //
// Description   : Procedure to Display Custome Details                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplayCust;
   Clear S2CId1;
   S2CId1 = S1CId;
   Reset CustDetails;

   Exec Sql
      Select * Into :CustDetails
      From CustPf Where CId = :S2CId1;

   Select;
      When S2CGender1  = 'Male';
         S2CGender = 'M';
      When S2CGender1  = 'Female';
         S2CGender = 'F';
      When S2CGender1  = 'Other';
         S2CGender = 'O';
   EndSl;

   MNGHDR     = 'Display Customer Representative Details';
   MngFtrL2   = 'F3=Exit   F12=Cancel';
   IndFieldPR = *On;

   Dow IndExit = *Off And IndCancel = *Off;
      Write MngHeader;
      Write MngFooter;
      Exfmt MngCurdCst;

      If IndExit = *On or IndCancel = *On;
         IndCancel = *Off;
         IndFieldPR = *Off;
         Clear S1Option;
         Leave;
      EndIf;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DeleteCust                                                           //
// Description   : Procedure to delete customer details                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc DeleteCust;
  Dow IndCancel = *Off;
     ClearDltSfl();
     LoadDltSfl();
     DisplayDltSfl();

     If IndCancel = *On;
        IndCancel = *Off;
        Reset Idx;
        Reset Idx1;
        Reset S2CId1;
        Clear ArrCId;
        DeleteFlag = *Off;
        Clear S1Option;
        Leave;
     Else;
     For Idx   = 1 to Idx1-1;
         S2CId1 = ArrCId(Idx);
         Exec Sql
            Delete From CustPf
            Where CId = :S2CId1;

         Exec Sql
            Delete From LoginPf
            Where UserId = :S2CId1;
      EndFor;
      MngErrMsg = 'Data deleted succuessfully';
       Deleteflag = *Off;
        Reset Idx;
        Reset Idx1;
        Reset S2CId1;
        Clear ArrCId;
        Deleteflag = *Off;
        Clear S1Option;
       Leave;
     EndIf;
  EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ClearDltSfl                                                         //
// Description   : Procedure to Clear delete subfile                                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearDltSfl;
   IndDltSlfClr = *On;
   #Rrn1        = 0;
   Write CustDltCtl;
   IndDltSlfClr = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadDltSfl                                                         //
// Description   : Procedure to load delete subfile                                   //
//------------------------------------------------------------------------------------//
Dcl-Proc LoadDltSfl;
   For Idx =1 to Idx1-1;
      S2CId1 = ArrCId(Idx);
      Exec Sql
         Select CId, CName, CPan, CMob
         Into :S3CId, :S3CName, :S3CPan, :S3CMob
         From CustPf
         Where CId = :S2CId1;
      #Rrn1 += 1;

      If #Rrn1 > 9999;
         Leave;
      EndIf;

      Write CustDltSfl;

   EndFor;

End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplayDltSfl                                                       //
// Description   : Procedure to display delete subfile                                 //
//------------------------------------------------------------------------------------ //

Dcl-Proc DisplayDltSfl;
   IndDltSflDsp    = *On;
   IndDltSflDspCtl = *On;
   IndDltSflEnd    = *On;

   If #Rrn1 < 1;
      IndDltSflDsp = *Off;
   EndIf;

   MngHdr = '        Delete Customer Details        ';
   MngFtrL2 = 'F12= Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt CustDltCtl;
End-Proc;
