**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/07/23                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Login Program                                             //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/07/23|         | Kartik P   | Program Creation                               //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt) BndDir('KARTIKCS/LOGINBND');

// File Declaration
Dcl-F LoginPf Usage(*Input : *Update) Keyed;
Dcl-F LoginD  Workstn Indds(IndicatorDs);

// Data Sturcture Declaration
Dcl-Ds IndicatorDs;
   IndExit       Ind Pos(03);
   IndPrompt     Ind Pos(04);
   IndRefresh    Ind Pos(05);
   IndNewAcc     Ind Pos(06);
   IndConfirm    Ind Pos(07);
   IndFrgtpsw    Ind Pos(08);
   IndCancel     Ind Pos(12);
   IndNodsp      Ind Pos(21);
   IndUserIDRI   Ind Pos(22);
   IndPswrdPC    Ind Pos(23);
   IndSecQusRI   Ind Pos(24);
   IndSecAndRI   Ind Pos(25);
   IndNewPassPc  Ind Pos(26);
   IndCnfmPassPc Ind Pos(27);
   IndChoiceRI   Ind Pos(28);
   IndSecQusPC   Ind Pos(29);
   IndUserIDPR   Ind Pos(30);
End-Ds;


//Copy Book Declaratioin
/Copy KartikCS/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-C Spchar Const('!@#$%^&*()_-+={[]}|\;:,<.>/?');
Dcl-S DefaultPsW Char(20) Inz(*Blank);
Dcl-S Count Zoned(2) Inz(*Zero);

// Main Code

  // Call Login Main Screen
Dow IndExit   = *Off;
   S1LogInHdr = '             Log-In             ';
   S1FtrLIne2 = 'F3=Exit   F5=Refresh   F6=Create new account   F8=Forgot password';
   Write LogInHdr;
   Write LogInFtr;
   Exfmt LogInMain;

   // Clear error message and reset indicator of login screen
   Clear S1ErrorMsg;
   Reset IndUserIDRI;
   Reset IndPswrdPC;

   // Different operation on login screen
   Select;
     When IndExit = *On;
       Leave;

     When IndRefresh = *On;
       Clear LogInMain;
       Clear S1ErrorMsg;
       Reset IndUserIDRI;
       Reset IndPswrdPC;


     When IndNewAcc  = *On;

     When IndFrgtpsw = *On;
        FgtPassword();

     Other;
       LoginUsr();
   EndSl;
EndDo;

// Last Line
*Inlr = *On;

//------------------------------------------------------------------------------------ //
// Procedure Name: Loginusr                                                            //
// Description   : Procedure to handle login                                           //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoginUsr;
      Loginvld();

      IF S1ErrorMsg <> *Blank;
      Return;
      EndIf;

      Select;
         When %Scan('MNG':S1UserID:1) = 1;
            DefaultPsW = 'WELCOME';
            IF %Trim(S1USERPSWD) = DefaultPsW;
               FirstLogin();
            Else;
               MngDsbrd();
               Clear LogInMain;
            EndIF;
            Return;

         When %Scan('CR':S1UserId:1) = 1;
            DefaultPsW = 'WELCOME';
            If %Trim(S1UserPswd) = DefaultPsW;
               FirstLogIn();
            Else;
               CrDshbrd(S1UserId);
               Clear LoginMain;
            EndIf;
            Return;
      EndSl;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: Loginvld                                                            //
// Description   : Procedure to validate login screen field                            //
//------------------------------------------------------------------------------------ //
Dcl-Proc Loginvld;
   If S1UserId    = *Blanks;
      IndUserIDRI = *On;
      S1ErrorMsg  = 'User Id field cannot be blank';
      Return;
   EndIf;

   Chain S1UserId LogInPf;
   If Not %Found(LogInPf);
      IndUserIDRI = *On;
      S1ErrorMsg  = 'Invalid user id';
      Return;
   EndIf;

   If S1USERPSWD = *Blanks;
      IndPswrdPC = *On;
      S1ErrorMsg = 'Password field cannot be blank';
      Return;
   EndIf;

   If  S1USERPSWD <> UserPass;
       IndPswrdPC  = *On;
       S1ErrorMsg  = 'Invalid password';
       Return;
   EndIf ;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: FirstLogin                                                          //
// Description   : Procedure to call screen to change default password                 //
//                 and set security question and answer                                //
//------------------------------------------------------------------------------------ //
Dcl-Proc FirstLogin;
   IndSecQusPC = *On;
   Dow IndExit   = *Off;
      S1LogInHdr = 'Reset Password & Security Answer';
      S1FtrLine2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';
      IndUserIDPR = *on;
      S2UserId   = S1UserId;
      S2Prompt = '(F4=Prompt)';
      Write LogInHdr;
      Write LogInFtr;
      Exfmt FstLginScr;

      Clear S1ErrorMsg;
      ResetInd();

      Select;
         when IndExit  = *on  Or IndCancel = *On;
            IndCancel   = *Off;
            Clear S1ErrorMsg;
            Clear LogInMain;
            Clear FstLginScr;
            Leave;

         When IndRefresh = *On;
            IndRefresh   = *Off;
            Clear FstLginScr;
            Clear S1ErrorMsg;

         When PrmtFld = 'S2SECQUS' And IndPrompt = *On;
            SecQusPrmt();

         When IndConfirm  = *On;
            IndConfirm    = *Off;
            FstlginVld();
            If S1ErrorMsg = *Blank;
               SetNewPsw();
            EndIf;

         Other;
            FstlginVld();
       EndSl;

   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ResetInd                                                            //
// Description   : Procedure to reset indicators                                       //
//------------------------------------------------------------------------------------ //
Dcl-Proc ResetInd;
   IndNodsp      = *Off;
   IndUserIDRI   = *Off;
   IndPswrdPC    = *Off;
   IndSecQusRI   = *Off;
   IndSecAndRI   = *Off;
   IndNewPassPc  = *Off;
   IndCnfmPassPc = *Off;
   IndSecQusPC   = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: Fstlginvld                                                          //
// Description   : Procedure to validate screen when user login with default password  //
//------------------------------------------------------------------------------------ //
Dcl-Proc FstlginVld;
   If S2SecQus   = *Blanks;
      IndSecQusRI  = *On;
      S1ErrorMsg   = 'Security Question field cannot be blank';
      Return;
   EndIf;

   If S2SecAns   = *Blanks;
      IndSecAndRI  = *On;
      S1ErrorMsg   = 'Security Answer field cannot be blank';
      Return;
   EndIf;

   PswdVld();

End-Proc;

//------------------------------------------------------------------------------------------ //
// Procedure Name: SetNewPsw                                                                 //
// Description   : Procedure to Update LoginPF with security question,answer and new password//
//------------------------------------------------------------------------------------ ------//
Dcl-Proc SetNewPsw;
   SECQUS     = S2SECQUS;
   SECANS     = S2SECANS;
   USERPASS   = S2CNFMPASS;
   Update LoginPFR;
   S1ErrorMsg = 'Secutiy Question and Password updated successfully';
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: SecQusPrmt                                                          //
// Description   : Procedure to call IndPrompt screen for security questions           //
//------------------------------------------------------------------------------------ //
Dcl-Proc SecQusPrmt;

   Dow IndCancel = *Off;
      Write LOGINHDR;
      Write LOGINFTR;
      Write FstLginScr;
      Exfmt SECQESWNDW;
      Clear P1ErrorMsg;
      IndChoiceRI = *Off;

      If IndCancel = *On;
         IndCancel   = *Off;
         Clear S1ErrorMsg;
         Clear P1Choice;
         IndChoiceRI = *Off;
         Leave;
      Else;
         Select;
            When P1CHOICE  = 1;
               S2SECQUS    = 'What is your favorite sport?';
               Leave;
            When P1CHOICE  = 2;
               S2SECQUS    = 'What color do you like the most?';
               Leave;
            When P1CHOICE  = 3;
               S2SECQUS    = 'What sport do you prefer the most?';
               Leave;
            When P1CHOICE  = 4;
               S2SECQUS    = 'What color does your child like?';
               Leave;
            Other;
               P1ErrorMsg  = 'Invaid Selection';
               IndChoiceRI = *On;
         EndSl;
      EndIf;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------- //
// Procedure Name: FgtPassword                                                          //
// Description   : Procedure to set new password when user forget password             //
//------------------------------------------------------------------------------------ //
Dcl-Proc FgtPassword;
   S1LOGINHDR = '       Forgot Password       ';
   S1FTRLINE2 = 'F3=Exit   F5=Refresh   F7=Confirm   F12=Cancel';
   Clear S2Prompt;
   Dow IndExit = *Off;
      IndUserIDPR = *Off;
      Write LOGINHDR;
      Write LOGINFTR;
      Exfmt FstLginScr;

      Clear S1ErrorMsg;
      ResetInd();

      Select;
         When IndExit = *On Or IndCancel = *On;
            IndCancel = *Off;
            Clear S1ERRORMSG;
            Clear LOGINMAIN;
            Clear FstLginScr;
            Leave;

         When IndRefresh = *On;
            IndRefresh = *Off;
            Clear FstLginScr;
            Clear S1ERRORMSG;

         When IndConfirm = *On;
            IndConfirm = *Off;
            FgtPswVld();
            If S1ERRORMSG = *Blank;
               UpdPswd();
            EndIF;

         Other;
            FgtPswVld();
      EndSl;
   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: FgtPswVld                                                          //
// Description   : Procedure to validate screen when user forgot password              //
//------------------------------------------------------------------------------------ //
Dcl-Proc FgtPswVld;
   IF S2USERID  = *Blank;
      IndUserIDRI = *On;
      S1ERRORMSG  = 'User Id field cannot be blank';
      Return;
   EndIf;

   Chain S2USERID LoginPf;
   IF %Found(LoginPf);
      S2SECQUS = SecQus;
   Else;
      IndUserIDRI = *On;
      S1ERRORMSG  = 'Invalid User Id';
      Return;
   EndIf;

   If S2SECANS   = *Blanks;
      IndSecAndRI  = *On;
      S1ErrorMsg   = 'Security Answer field cannot be blank';
      Return;
   EndIf;

   If S2SECANS <> SecAns;
      IndSecAndRI  = *On;
      S1ErrorMsg   = 'Invalid Security Answer';
      Return;
   EndIf;

   PswdVld();


End-Proc;

Dcl-Proc  PswdVld;
   If S2NEWPASS  = *Blank;
      IndNewPassPc = *On;
      S1ErrorMsg   = 'New Password field cannot be blank';
      Return;
   EndIf;

   If %Len(%Trim(S2NEWPASS)) < 6;
      IndNewPassPc = *On;
      S1ERRORMSG = 'Password cannot be less than 6 character';
      Return;
   EndIf;

   For Count = 1 To %Len(%Trim(Spchar));
      If %Scan(%subst(Spchar:Count:1): %Trim(S2NEWPASS)) < 1;
         IndNewPassPc = *On;
         S1ERRORMSG = 'Password should contain at least 1 special character';
      Else;
         Clear S1ERRORMSG;
         IndNewPassPc = *Off;
         Leave;
      EndIf;
   EndFor;
   If S1ERRORMSG <> *Blank;
      Return;
   EndIf;

   If S2CNFMPASS = *Blank;
      IndCnfmPassPc= *On;
      S1ErrorMsg   = 'Confirm Password field cannot be blank';
      Return;
   EndIf;

   If S2NEWPASS <> S2CNFMPASS;
      IndCnfmPassPc= *On;
      S1ErrorMsg   = 'New Password & Confirm Password are not same';
      Return;
   EndIf;
End-Proc;

Dcl-Proc UpdPswd;
   USERPASS = S2CNFMPASS;
   Update LoginPFR;
   S1ERRORMSG = 'Your Password has been changed';
   Clear FstLginScr;
End-Proc;

