program PasswordChange;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
   {$ENDIF} {$IFDEF HASAMIGA}
  athreads,
   {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms,
  Controls,
  Dialogs,
  PasswordDialog { you can add units after this };

{$R *.res}

var
  UserID:         string;
  NewPassword:    string;
  ChangeRequired: boolean;

begin
  RequireDerivedFormResource := True;
  Application.Scaled         := True;
  Application.Initialize;
  //Application.CreateForm(TPasswordChangeDialog, PasswordChangeDialog);
  //Application.ProcessMessages;

  // This is only a shortcut for testing purposes
  // In real life you would call the PasswordChangeDialog from your Form
  // Instead of directly from the Application.

  UserID := 'MyUserID';
  PCD_PasswordReset( nil, UserID, NewPassword, ChangeRequired );

  //PasswordChangeDialog := TPasswordChangeDialog.Create( nil );
  //try
  //  // All of the parameters except for Salt are optional
  //  // Each has a default, which may or may not suit your needs
  //  with PasswordChangeDialog do begin
  //    Mode              := pcm_Reset;    // pcm_Reset or pcm_Change
  //    Caption           := 'Password Reset: ' + UserID;
  //    Iterations        := 429937;       // Number of Iterations for Hash Routine
  //    Salt              := UserID;       // Usually the UserID, but you can get creative...
  //    PwdLength         := 12;           // If not specified Default is Minimum Length
  //    MinLength         := 1;            // Minimum Password length - Default 8
  //    MaxLength         := 48;           // Maximum Password length - Default 64
  //    AlphaUpperCase    := pws_yes;      // Should Upper Case characters be Allowed/Required
  //    AlphaLowerCase    := pws_yes;      // Should Lower Case characters be Allowed/Required
  //    Numerals          := pws_yes;      // Should Numeric characters be Allowed/Required
  //    SpecialCharacters := pws_allowed;  // Should Special characters be Allowed/Required
  //    ExcludeSimilar    := pws_yes;      // Should we exclude characters that look very similar
  //    ExcludeAmbiguous  := pws_yes;      // Should we exclude characters know to confuse some apps
  //
  //    if ShowModal = mrOK then begin
  //      QuestionDlg( 'Result',
  //        copy(HashedPassword, 1,48) + #13 +
  //        copy(HashedPassword,49,48) + #13 +
  //        copy(HashedPassword,97,48) + #13 +
  //        BoolToStr(RequirePasswordChange),
  //        mtCustom, [mrOK], ''
  //      );
  //    end;
  //  end;
  //finally
  //  FreeAndNil( PasswordChangeDialog );
  //end;

  //Application.Run;
end.
