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
Ctl-Opt Option(*Nodebugio : *Srcstmt) BndDir('KARTIKCS/STATEBND');
Ctl-Opt NoMain;
/Title  ('MNGRCRM - Module to manage the customer represetative details');

// File Declaration
Dcl-F MngDsbD WorkStn Indds(IndicatorDs) Sfile(CrSflM01 : #Rrn)
                                         Sfile(CrDltSflM1: #Rrn1);
// Data Structure Declaration
// Indicator data structure
Dcl-Ds IndicatorDs;
   IndExit         Ind Pos(03);
   IndPrompt       Ind Pos(04);
   IndRefresh      Ind Pos(05);
   IndInsert       Ind Pos(06);
   IndConfirm      Ind Pos(07);
   IndCancel       Ind Pos(12);
   IndSflDsp       Ind Pos(21);
   IndSflDspCtl    Ind Pos(22);
   IndSflClr       Ind Pos(23);
   IndSflEnd       Ind Pos(24);
   IndOptRI        Ind Pos(25);
   IndOptPC        Ind Pos(26);
   IndCrNameRI     Ind Pos(31);
   IndCrDobRI      Ind Pos(32);
   IndCrDojRI      Ind Pos(33);
   IndCurAddRI     Ind Pos(34);
   IndPrmAddRI     Ind Pos(35);
   IndCrSateRI     Ind Pos(36);
   IndCrCityRI     Ind Pos(37);
   IndMobNORI      Ind Pos(38);
   IndAltMobRI     Ind Pos(39);
   IndCrEmail      Ind Pos(40);
   IndGenderRI     Ind Pos(41);
   IndDltSflDsp    Ind Pos(42);
   IndDltSflDspCtl Ind Pos(43);
   IndDltSlfClr    Ind Pos(44);
   IndDltSflEnd    Ind Pos(45);
   IndFieldPR      Ind Pos(98)  Inz(*Off);
End-Ds;

// data sctructure to contail fields of screen
Dcl-Ds CrDetails1;
   S2CrId       Char(10)  Inz;
   S2CrName     Char(20)  Inz;
   S2CrGender1  Char(6)   Inz;
   S2CrMob      Zoned(10) Inz;
   S2CrAltMob   Zoned(10) Inz;
   S2CrDob      Date      Inz;
   S2CRDOJ      Date      Inz;
   S2CREMAIL    Char(50)  Inz;
   S2CRCURADD   Char(50)  Inz;
   S2CRPRMADD   Char(50)  Inz;
   S2STATE      Char(15)  Inz;
   S2CITY       Char(15)  Inz;
   S2PINCODE    Zoned(6)  Inz;
   S2AddTs      Timestamp Inz;
   S2UpdTs      Timestamp Inz;
End-Ds;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-S #Rrn         Zoned(4) Inz(*Zero);
Dcl-S #Rrn1        Zoned(4) Inz(*Zero);
Dcl-S CrIdSubfix   Zoned(8) Inz(*Zero);
Dcl-S Cnt          Zoned(5) Inz(*Zero);
Dcl-S Idx          Zoned(5) Inz(*Zero);
Dcl-S Idx1         Zoned(4) Inz(1);
Dcl-S PCrId        Char(10) Inz(*Blank);
Dcl-S GetState     Char(15) Inz(*Blank);
Dcl-S S2CrId#      Char(10) Inz(*Blank);
Dcl-S S2CrId1      Like(S2CrId);
Dcl-S #CrId        Char(20) Inz(*Blank);
Dcl-S ArrCrId      Char(20) Dim(9999);
Dcl-S Stmt         Char(200)Inz(*Blank);
Dcl-S Deleteflag   Ind      Inz(*Off);
Dcl-C QT           Const('''');
DCl-C CapsAlpha    Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ ');
Dcl-C Regex1       Const('^(?:\w+\.?)*\w+@(?:\w+\.)*\w+(?:\s+\.?)*$');
Dcl-S GetTimeStamp TimeStamp;

// Main Code
Dcl-Proc CRSubFile Export;
   IndExit = *Off;
   Dow IndExit = *Off;
      ClearSfl();
      LoadSfl();
      DisplaySfl();

      Exec Sql
         Set Option Commit = *None, DatFmt= *Iso;

      // Select statement to control the flow of program
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
            Clear POSITIONTO;
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
   Write CrCtlM01;
   IndSflClr = *Off;
End-proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Custome Representative Subfile                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   IndOptPC = *On;
   Clear Stmt;
   Stmt = 'Select CrId, CrName, CrDoj, CrMob From CustRepPf';

   // If condition to load the subfile according to position to filed
   If PositionTo <> *Blank;
      Stmt = %Trim(Stmt) + ' Where CrId Like ' + QT + '%' + %Trim(PositionTo) + '%' + QT +
             'Or CrName Like ' + QT + '%' + %Trim(PositionTo) + '%' + QT;
   EndIf;

   Exec Sql
      Prepare SqlStmt From :Stmt;

   Exec Sql
      Declare C01 Cursor for Sqlstmt;

   Exec Sql
      Open C01;

      Exec Sql
         Fetch From C01 Into :S1CrId, :S1CrName, :S1CrDoj, :S1CRMob;

      Dow SqlCode = 0;
         #Rrn += 1;

         If #Rrn > 9999;
            Leave;
         EndIF;

         Write CRSflM01;

         Exec Sql
            Fetch From C01 Into :S1CrId, :S1CrName, :S1CrDoj, :S1CRMob;
      EndDo;

      Exec Sql
         Close C01;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Custome Representative Subfile                 //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   IndSflDsp     = *On;
   IndSflDspCtl  = *On;
   IndSflEnd     = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIf;

   Clear PositionTo;
   MngHdr   = '   Work With Customer Representative   ';
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
   Reset CrDetails1;

   //Id Auto increment
   Exec Sql
      Select Max(CrId) Into :PCrId From CustRepPf;

   If PCrId  = *Blank;
      S2CrId = 'CR00000001';
   Else;
      CrIdSubfix = %Int(%Subst(PCrId : 3)) + 1;
      S2CrId     = 'CR' + %Editc(CrIdSubfix : 'X');
      S2CrId1    = S2CrId;
   EndIF;

   Dow IndExit = *Off Or IndCancel = *Off;

      Exec Sql
         Select Pincode Into :S2Pincode
         From StatePf
         Where City = :S2City;

      Write MngHeader;
      Write MngFooter;
      Exfmt MngCurdScr;
      Clear MngErrmsg;
      ResetInd();

      // Select statement to control the flow of the program
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
            S2CrId = S2CrId1;

         When PrmtFld ='S2STATE' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = 'STATE';
            S2State   = StateCityPrmpt(GetState);

         When PrmtFld ='S2CITY' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = S2State;
            S2City    = StateCityPrmpt(GetState);

         When IndConfirm = *On;
            IndConfirm   = *Off;
            MngCurdVld();
            If MngErrMsg = *Blank;
               InsertRec();
            EndIf;

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
   IndOptRI     = *Off;
   IndCrNameRI  = *Off;
   IndCrDobRI   = *Off;
   IndCrDojRI   = *Off;
   IndCurAddRI  = *Off;
   IndPrmAddRI  = *Off;
   IndCrSateRI  = *Off;
   IndCrCityRI  = *Off;
   IndMobNORI   = *Off;
   IndAltMobRI  = *Off;
   IndCrEmail   = *Off;
   IndGenderRI  = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: InsertRec                                                           //
// Description   : Procedure to Write Pf from Display file                             //
//------------------------------------------------------------------------------------ //
Dcl-Proc InsertRec;

   Clear S2UpdTs;
   S2AddTs = %Timestamp();

   // Select statement to get the value for gender
   Select;
      When S2CrGender = 'M';
         S2CrGender1  = 'Male';
      When S2CrGender = 'F';
         S2CrGender1  = 'Female';
      when S2CrGender = 'O';
         S2CrGender1  = 'Other';
   EndSl;

   Exec Sql
      Insert Into CustRepPF
      Values(:CrDetails1);

   Exec Sql
      Insert Into LoginPf(UserId, UserPass)
      Values (:S2CrId, 'WELCOME');

      Clear MNGCURDSCR;
      Exec Sql
      Select Max(CrId) Into :PCrId From CustRepPf;

      // Id auto increment
      CrIdSubfix = %Int(%Subst(PCrId : 3)) + 1;
      S2CrId     = 'CR' + %Editc(CrIdSubfix : 'X');
      MngErrMsg  = 'Record inserted successfully';
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: MngCurdVld                                                          //
// Description   : Procedure to Validate Fields for CR CRUD Operation                  //
//------------------------------------------------------------------------------------ //
Dcl-Proc MngCurdVld;

   Dcl-S Email Varchar(100) Inz(*Blank);
   // Validation for Customer Representative name
   If S2CrName    = *Blank;
      IndCrNameRI = *On;
      MngErrMsg   = 'Name field cannot be blank';
      Return;
   EndIf;
   IF %check(CapsAlpha : S2CrName) <> 0;
      IndCrNameRI = *On;
      MngErrMsg   = 'Name field cannot contain number and special character';
      Return;
   EndIf;

   // Validation for Customer Representative gender
   If S2CrGender  = ' ';
      IndGenderRI = *On;
      MngErrMsg   = 'Gender cannot be unselected';
      Return;
   EndIf;

   // Validation for Customer Representative date of birth
   If %Char(S2CrDob)    = '0001-01-01';
      IndCrDobRI =  *On;
      MngErrMsg  = 'Date of Birth field cannot be blank';
      Return;
   EndIf;
   If %Diff(%Date() : S2CrDob : *Years) < 18;
      IndCrDobRI =  *On;
      MngErrMsg  = 'Age cannot be less then 18 years';
      Return;
   EndIf;

   // Validation for Customer Representative date of join
   If %Char(S2CrDoj)    = '0001-01-01';
      IndCrDojRI = *On;
      MngErrMsg  = 'Date of Join field cannot be blank';
      Return;
   EndIf;
   If %Diff(S2CrDoj : S2CrDob : *Years) < 18;
      IndCrDojRI = *On;
      MngErrMsg  = 'Difference of Date of birth and Date of join cannot be less then 18';
      Return;
   EndIf;

   // Validation for Customer Representative Current Address
   If S2CrCurAdd  = *Blank;
      IndCurAddRI = *On;
      MngErrMsg   = 'Current Address cannot be blank';
      Return;
   EndIf;

   // Validation for Customer Representative Permanent Address
   If S2CrPrmAdd  = *Blank;
      IndPrmAddRI = *On;
      MngErrMsg   = 'Permanent Address cannot be blank';
      Return;
   EndIf;

   // Validation for Customer Representative State
   If S2State     = *Blank;
      IndCrSateRI = *On;
      MngErrMsg   = 'State field cannot be blank';
      Return;
   EndIf;

   // Validation for Customer Representative City
   If S2City      = *Blank;
      IndCrCityRI = *On;
      MngErrMsg   = 'City field cannot be blank';
      Return;
   EndIf;

   // Validation for Customer Representative Mobile Number
   If S2CrMob    = *Zero;
      IndMobNORI = *On;
      MngErrMsg  = 'Mobile Number field cannot be blank';
      Return;
   EndIf;
   If %Len(%Trim(%Char(S2CrMob))) < 10;
      IndMobNoRI = *On;
      MngErrMsg  = 'Mobile Number should have 10 digit';
      Return;
   EndIf;

   // Validation for Customer Representative Alternate Mobile Number
   If S2CrAltMob  = *Zero;
      IndAltMObRI = *On;
      MngErrMsg   = 'Alternate Mobile Number field cannot be blank';
      Return;
   EndIf;
   If %Len(%Trim(%Char(S2CrAltMob))) < 10;
      IndAltMobRI = *On;
      MngErrMsg   = 'Alternate Mobile Number should have 10 digit';
      Return;
   EndIf;

   // Validation for Customer Representative Email Address
   If S2CrEmail  = *Blank;
      IndCrEmail = *On;
      MngErrMsg  = 'Email Address Cannot be blank';
      Return;
   EndIf;

   Email = S2CrEmail;
   Exec Sql
      Set :Cnt = RegExp_Count(:Email, :Regex1);
   If Cnt <> 1;
      IndCrEmail = *On;
      MngErrMsg  = 'Invalid Email Address';
      Return;
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: OtherOption                                                         //
// Description   : Procedure to read sfl for other crud operation                      //
//------------------------------------------------------------------------------------ //
Dcl-Proc OtherOption;
   ReadC CrSflm01;
   DoW S1Option <> 0;
      Select;
         When S1Option = 2;
            UpdateCr();
         When S1Option = 4;
            ArrCrId(Idx1) = S1CrId;
            Idx1 += 1;
            Deleteflag = *On;
         When S1Option = 5;
            DisplayCr();
         Other;
            Clear S1Option;
      EndSl;
      Clear S1Option;
      ReadC CrSflM01;
   EndDo;
   If Deleteflag = *On;
      DeleteCr();
   EndIf;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: UpdateCr                                                            //
// Description   : Procedure to update customer representative details                 //
//------------------------------------------------------------------------------------ //
Dcl-Proc UpdateCr;
   Clear S2CrId#;
   S2CrId# = S1CrId;
   Reset CrDetails1;
   Exec Sql
      Select * Into :CrDetails1
      From CustRepPF Where CrId = :S2CrId#;

   // Select statement to get the value for gender
   Select;
      When S2CrGender1 = 'Male';
         S2CrGender    = 'M';
      When S2CrGender1 = 'Female';
         S2CrGender    = 'F';
      Other;
         S2CrGender    = 'O';
   EndSl;

   MNGHDR   = ' Update Customer Representative Details ';
   MngFtrL2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';

   Dow IndExit = *Off Or IndCancel = *Off;

      Exec Sql
         Select Pincode Into :S2Pincode
         From StatePf
         Where City = :S2City;

      Write MngHeader;
      Write MngFooter;
      Exfmt MngcurdScr;

      Clear MngErrMsg;
      ResetInd();

      // Select statement to control the flow of the program
      Select;
         When IndExit = *on Or IndCancel = *On;
            IndCancel = *Off;
            S1Option = 0;
            Clear MngErrMsg;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear MngErrMsg;
            ResetInd();
            Reset CrDetails1;
            Exec Sql
               Select * Into :CrDetails1
               From CustRepPF Where CrId = :S2CrId#;

         When PrmtFld ='S2STATE' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = 'STATE';
            S2State   = StateCityPrmpt(GetState);

         When PrmtFld ='S2CITY' And IndPrompt = *On;
            IndPrompt = *Off;
            GetState  = S2State;
            S2City    = StateCityPrmpt(GetState);

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

//------------------------------------------------------------------------------------ //
// Procedure Name: UpdateRec                                                            //
// Description   : Procedure to update customer representative details                 //
//------------------------------------------------------------------------------------ //
Dcl-Proc UpdateRec;
   Dcl-S HGender Char(6) Inz(*Blank);

   GetTimeStamp = %Timestamp();

   // select statement to get the value for gender
   Select;
      When S2CrGender = 'M';
         S2CrGender1  = 'Male';
      When S2CrGender = 'F';
         S2CrGender1  = 'Female';
      when S2CrGender = 'O';
         S2CrGender1  = 'Other';
   EndSl;

   S2UpdTs   = %Timestamp();

   Exec Sql
      Update CustRepPf
      Set Row = :CrDetails1 Where CrId = :S2CrId#;

   MngErrMsg  = 'Record updated successfully';
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplayCr                                                           //
// Description   : Procedure to display customer representative details                //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplayCr Export;
   Dcl-Pi DisplayCr;
       UserId Char(10);
   End-Pi;
   Clear S2CrId#;
   S2CrId# = S1CrId;
   Reset CrDetails1;
   Exec Sql
      Select * Into :CrDetails1
      From CustRepPF Where CrId = :S2CrId# Or CrId = :UserId;

   // Select statement to get the value for gender
   Select;
      When S2CrGender1 = 'Male';
         S2CrGender    = 'M';
      When S2CrGender1 = 'Female';
         S2CrGender    = 'F';
      Other;
         S2CrGender    = 'O';
   EndSl;

   MNGHDR     = 'Display Customer Representative Details';
   MngFtrL2   = 'F3=Exit   F12=Cancel';
   IndFieldPR = *On;
   // *In99 = *on;
   Dow IndExit = *Off And IndCancel = *Off;
      Write MngHeader;
      Write MngFooter;
      Exfmt MngcurdScr;

      If IndExit = *On Or IndCancel   = *On;
         IndCancel   = *Off;
   //      *In99 = *Off;
         IndFieldPR  = *Off;
         Clear S1Option;
         Leave;
      EndIf;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DeleteCr                                                            //
// Description   : Procedure to delete customer representative details                //
//------------------------------------------------------------------------------------ //
Dcl-Proc DeleteCr;
  Dow IndCancel = *Off;
     ClearDltSfl();
     LoadDltSfl();
     DisplayDltSfl();

     If IndCancel = *On;
        IndCancel = *Off;
        Reset Idx;
        Reset Idx1;
        Reset #CrId;
        Clear ArrCrId;
        Deleteflag = *Off;
        Clear S1Option;
        Leave;
     Else;
       For Idx = 1 to Idx1-1;
         #CrId = ArrCrId(Idx);
         Exec Sql
            Delete From CustRepPF
            Where CrId = :#CrId;

         Exec Sql
            Delete From LoginPf
            Where UserId = :#CrId;
       EndFor;
       MngErrMsg = 'Data deleted succuessfully';
       Deleteflag = *Off;
        Reset Idx;
        Reset Idx1;
        Reset #CrId;
        Clear ArrCrId;
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
   Write CrDltCtlM1;
   IndDltSlfClr = *Off;
End-Proc;


//------------------------------------------------------------------------------------ //
// Procedure Name: LoadDltSfl                                                         //
// Description   : Procedure to load delete subfile                                   //
//------------------------------------------------------------------------------------//
Dcl-Proc LoadDltSfl;
   For Idx = 1 to Idx1-1;
      #CrId = ArrCrId(Idx);
      Exec Sql
         Select CrId, CrName, CrDoj, CrMob
         Into :S3CrId, :S3CrName, :S3CrDoj, :S3CrMob
         From CustRepPF
         Where CrId = :#CrId;

      #Rrn1 += 1;

      If #Rrn1 > 9999;
         Leave;
      EndIf;

      Write CrDltSflM1;

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

   MngHdr   = 'Delete Customer Representative Details';
   MngFtrL2 = 'F12= Cancel';
   Write MngHeader;
   Write MngFooter;
   Exfmt CrDltCtlM1;
End-Proc;
