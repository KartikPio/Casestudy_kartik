**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/07/24                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Change password module                                    //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/07/24|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt) NoMain;

// File Declaration
Dcl-F LoginD WorkStn Indds(IndicatorDs);

// Data Structure Declaration
Dcl-Ds IndicatorDs;
   IndExit         Ind Pos(03);
   IndConfirm      Ind Pos(07);
   IndCancel       Ind Pos(12);
   IndUserIdRI     Ind Pos(22);
   IndOldPswdPC    Ind Pos(23);
   IndNewPswdPC    Ind Pos(26);
   IndCnfmPswdPC   Ind Pos(27);
End-Ds;

//Copy Book Declaration
/Copy KartikCS/Qrpglesrc,Copy_Book

Dcl-Proc ChangPswd Export;

   Dow IndExit = *Off or IndCancel = *Off;

      Exec Sql
         Set Option Commit = *None;

      S1LogInHdr = '        Change Password                  ';
      S1FtrLIne2 = 'F3=Exit   F7=Confirm   F12=Cancel';
      Write LogInHdr;
      Write LogInFtr;
      Exfmt FgtPswScr;

      Clear S1ErrorMsg;
      ResetInd();

      Select;
         When IndExit = *On or IndCancel = *On;
            IndCancel = *Off;
            Leave;

         When IndConfirm = *On;
            IndConfirm   = *Off;
            ChgPswVld();
            If S1ErrorMsg = *Blank;

               Exec Sql
                  Update LoginPf Set UserPass = :S3CnfmPswd
                  Where UserId = :S3UserId;
                  Clear FgtPswScr;
                  S1ErrorMsg = 'Passwrod updated';
            EndIf;

         Other;
            ChgPswVld();
       EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ResetInd                                                            //
// Description   : Procedure to Reset the field level indicator                        //
//------------------------------------------------------------------------------------ //
Dcl-Proc ResetInd;
   IndUserIdRI    = *Off;
   IndOldPswdPC   = *Off;
   IndNewPswdPC   = *Off;
   IndCnfmPswdPC  = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ChgPswVld                                                           //
// Description   : Procedure to validate field for change password                     //
//------------------------------------------------------------------------------------ //
Dcl-Proc ChgPswVld;
   Dcl-S PUserId Char(20) Inz(*Blank);

   // Validation for User Id
   If S3UserId    = *Blank;
      IndUserIdRI = *On;
      S1ErrorMsg  = 'User Id field cannot be blank';
      Return;
   EndIf;

   Exec Sql
      Select UserId Into :PuserId from LoginPf
      Where UserId = :S3UserId;
   If SqlCode <> 0;
      IndUserIdRI  = *On;
      S1ErrorMsg   = 'Invalid User Id';
      Return;
   EndIf;

   // Validation for Old Password;
   If S3OldPswd    = *Blank;
      IndOldPswdPC = *On;
      S1ErrorMsg   = 'Old Password field cannot be blank';
      Return;
   EndIf;

   Clear PUserId;
   Exec Sql
      Select UserPass Into :PUserId from LoginPf
      Where UserPass  = :S3OldPswd And UserId = :S3UserId;
   If SqlCode <> 0 ;
      IndOldPswdPC = *On;
      S1ErrorMsg   = 'Invalid Old Password';
      Return;
   EndIf;

   // Validation for New password
   If S3NewPswd    = *Blank;
      IndNewPswdPC = *On;
      S1ErrorMsg   = 'New Password field cannot be blank';
      Return;
   EndIf;

   // Validation for Confirm Password
   If S3CnfmPswd   = *Blank;
      IndCnfmPswdPC= *On;
      S1ErrorMsg   = 'Confirm Password field cannot be blank';
      Return;
   EndIf;

   If S3NewPswd <> S3CnfmPswd ;
      IndCnfmPswdPC= *On;
      S1ErrorMsg   = 'New Password and Confirm Password are not matched';
      Return;
   EndIf;

End-Proc;
