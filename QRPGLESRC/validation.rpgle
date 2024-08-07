**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/07/24                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: Validation Module                                         //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/07/24|         | Kartik P   | Module  Creation                               //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *SrcStmt) Nomain;

// File Declaration
dcl-f LoginD Workstn Indds(Indds#);
dcl-f Loginpf usage(*input) keyed;

// Data Structure Declaration
dcl-ds Indds#;
 Exit     Ind Pos(03);
 Promt    Ind Pos(04);
 Refresh  Ind Pos(05);
 NewAcc   Ind Pos(06);
 Confirm  Ind Pos(07);
 Frgtpsw  Ind Pos(08);
 Chngpsw  Ind Pos(09);
 Nodsp    Ind Pos(21);
 UserIDRI Ind Pos(22);
 PswrdPc  Ind Pos(23);
end-ds;

/Copy KartikCs/Qrpglesrc,CopyBook

dcl-proc Loginvld Export;
    dcl-pi Loginvld;
      S1USERID     Char(10);
      S1USERPSWD   Char(20);
    end-pi;
    Select;
       When S1USERID = *Blanks;
          UserIDRI = *On;
          S1MESSAGE= 'User Id field cannot be blank';
          Return;
       When S1USERPSWD = *Blanks;
          PswrdPc  = *On;
          S1MESSAGE= 'Password Fild cannot be blank';
          Return;
    EndSl;
end-proc;

