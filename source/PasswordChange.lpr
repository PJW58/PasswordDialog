program PasswordChange;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils, Forms, controls, dialogs, PasswordDialog
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TPasswordChangeDialog, PasswordChangeDialog);
  //Application.ProcessMessages;

  PasswordChangeDialog := TPasswordChangeDialog.Create( nil );
  try
    // All of the parameters except for UserId are optional
    // Each has a default, which may or may not suit your needs
    with PasswordChangeDialog do begin
      Caption           := 'Password Reset';
      Iterations        := 429937;       // Number of Iterations for Hash Routine
      Salt              := 'MyUserID';   // Usually the UserID, but you can get creative...
      MinLength         := 1;            // Minimum Password length
      MaxLength         := 48;           // Maximum Password length
      AlphaUpperCase    := pws_yes;      // Should Upper Case characters be Allowed/Required
      AlphaLowerCase    := pws_yes;      // Should Lower Case characters be Allowed/Required
      Numerals          := pws_yes;      // Should Numeric characters be Allowed/Required
      SpecialCharacters := pws_allowed;  // Should Special characters be Allowed/Required
      ExcludeSimilar    := pws_required;
      ExcludeAmbiguous  := pws_required;

      if ShowModal = mrOK then begin
        showmessage(HashedPassword + #13 + BoolToStr(RequirePasswordChange));
      end;
    end;
  finally
    FreeAndNil( PasswordChangeDialog );
  end;


  //Application.Run;
end.

