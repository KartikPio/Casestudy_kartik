**Free
// ------------------------------------------------------------------------------- //
// Created By..........: Programmers.io @ 2024                                     //
// Created Date........: 2024/08/08                                                //
// Developer...........: Kartik Parihar                                            //
// Description.........: State City Window subfile module                          //
// ------------------------------------------------------------------------------- //
// MODIFICATION LOG:                                                               //
// ------------------------------------------------------------------------------- //
// Date    | Mod_ID  | Developer  | Case and Description                           //
// --------|---------|------------|----------------------------------------------- //
// 24/08/08|         | Kartik P   | Module Creation                                //
// ------------------------------------------------------------------------------- //

// Control Option
Ctl-Opt Option(*Nodebugio : *Srcstmt);
Ctl-Opt Nomain;

// File Declaration
Dcl-F WindowSfl Workstn Indds(IndicatorDs) Sfile(StateSfl01 : #Rrn);

//Data Sturcture Declaration
Dcl-Ds IndicatorDs;
   IndCancel    Ind Pos(12);
   IndSflDsp    Ind Pos(31);
   IndSflDspCtl Ind Pos(32);
   IndSflClr    Ind Pos(33);
   IndSflEnd    Ind Pos(34);
   IndSflOptRI  Ind Pos(35);
End-Ds;

//Copy Book Declaration
/Copy KartikCs/Qrpglesrc,Copy_Book

// Variable Declaration
Dcl-S #Rrn Zoned(4) Inz(*Zero);
Dcl-S PState Char(15) Inz(*Blank);

// Main Code
Dcl-Proc StateCityPrmpt Export;
   Dcl-Pi StateCityPrmpt Char(15);
      GetState1 Char(15);
   End-Pi;

   Dow IndCancel = *Off;
      Clear W1Option;
      IndSflOptRI = *Off;
      ClearSfl();
      Loadsfl(GetState1);
      DisplaySfl(GetState1);

      Exec Sql
         Set Option Commit = *None;

      If IndCancel = *On;
         IndCancel = *Off;
         IndSflOptRI = *Off;
         Clear W1OPTION;
         Clear W1ERRORMSG;
         PState = *Blanks;
         Return PState;
      Else;
         ReadWsfl(GetState1);
         Return PState;
      EndIf;

   EndDo;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ClearSfl                                                            //
// Description   : Procedure to Clear Window Subfile                                   //
//------------------------------------------------------------------------------------ //
Dcl-Proc ClearSfl;
   Write WindowFtr;
   IndSflClr = *On;
   #Rrn      = 0;
   Write StateCtl01;
   IndSflClr = *Off;
End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: LoadSfl                                                             //
// Description   : Procedure to Load Window Subfile                                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc LoadSfl;
   Dcl-Pi LoadSfl;
     GetState1 Char(15);
   End-Pi;

   If GetState1 = 'STATE';
      Exec Sql
         Declare SflCursor Cursor for
         Select Distinct States From StatePf;

      Exec Sql
         Open SflCursor;

      Exec Sql
         Fetch From SflCursor Into :W1State;

      Dow SqlCode = 0;

         #Rrn += 1;

         If #Rrn > 9999;
            Leave;
         EndIf;

         Write StateSfl01;

         Exec Sql
            Fetch Next From SflCursor Into :W1State;

      EndDo;

      Exec Sql
         Close SflCursor;

   Else;
      Exec Sql
         Declare SflCursor1 Cursor for
         Select City From StatePf Where States = :GetState1;

      Exec Sql
         Open SflCursor1;

       Exec Sql
         Fetch From SflCursor1 Into :W1State;

      Dow SqlCode = 0;

         #Rrn += 1;

         If #Rrn > 9999;
            Leave;
         EndIf;

         Write StateSfl01;

         Exec Sql
            Fetch Next From SflCursor1 Into :W1State;

      EndDo;

      Exec Sql
         Close SflCursor1;
   EndIf;

End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: DisplaySfl                                                          //
// Description   : Procedure to Display Window Subfile                                 //
//------------------------------------------------------------------------------------ //
Dcl-Proc DisplaySfl;
   Dcl-Pi DisplaySfl;
      GetState1 Char(15);
   End-Pi;

   IndSflDsp    = *On;
   IndSflDspCtl = *On;
   IndSflEnd    = *On;

   If #Rrn < 1;
      IndSflDsp = *Off;
   EndIF;

   If GetState1 = 'STATE';
      W1Header  = 'STATE';
      W1FldH01  = 'STATE';
   Else;
      W1Header = ' CITY ';
      W1FLDH01 = 'CITY';
   EndIf;

   Exfmt StateCtl01;

End-Proc;

//------------------------------------------------------------------------------------ //
// Procedure Name: ReadWsfl                                                            //
// Description   : Procedure to Read Window Subfile                                    //
//------------------------------------------------------------------------------------ //
Dcl-Proc ReadWsfl;
   Dcl-Pi ReadWsfl Char(15);
     GetState1 Char(15);
   End-Pi;
   ReadC StateSfl01;
   DoW Not %Eof;
   If W1Option = 1;
     PState = W1State;
     Return PState;
   Else;
     Clear W1Option;
     Pstate = *Blank;
   EndIf;
     ReadC StateSfl01;
   EndDo;
     Return Pstate;
End-Proc;
