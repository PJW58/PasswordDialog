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
    // All of the parameters except for Salt are optional
    // Each has a default, which may or may not suit your needs
    with PasswordChangeDialog do begin
      Caption           := 'Password Reset';
      Iterations        := 429937;       // Number of Iterations for Hash Routine
      Salt              := 'MyUserID';   // Usually the UserID, but you can get creative...
      PwdLength         := 12;           // If not specified Default is Minumum Length
      MinLength         := 1;            // Minimum Password length - Default 8
      MaxLength         := 48;           // Maximum Password length - Default 64
      AlphaUpperCase    := pws_required; // Should Upper Case characters be Allowed/Required
      AlphaLowerCase    := pws_yes;      // Should Lower Case characters be Allowed/Required
      Numerals          := pws_yes;      // Should Numeric characters be Allowed/Required
      SpecialCharacters := pws_allowed;  // Should Special characters be Allowed/Required
      ExcludeSimilar    := pws_yes;      // Should we exclude characters that look very similar
      ExcludeAmbiguous  := pws_yes;      // Should we exclude characters know to confuse some apps

      if ShowModal = mrOK then begin
        showmessage(HashedPassword + #13 + BoolToStr(RequirePasswordChange));
      end;
    end;
  finally
    FreeAndNil( PasswordChangeDialog );
  end;


  //Application.Run;
end.

