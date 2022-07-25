# PasswordDialog
 Password Change Dialog with PBKDF2 Hashing
 
 Author:  Paul J West
 License: Modified LGPL with Linking Exception
 
Usage: 
 ```Pascal
   PasswordChangeDialog := TPasswordChangeDialog.Create( nil );
   try
    // All of the parameters except for UserId are optional
    // Each has a default, which may or may not suit your needs
    with PasswordChangeDialog do begin
      Caption           := 'Password Reset';
      Iterations        := 429937;       // Number of Iterations for Hash Routine
      Salt              := 'TestID';     // Usually the UserID, but you can get creative...
      MinLength         := 12;           // Minimum Password length
      MaxLength         := 64;           // Maximum Password length
      AlphaUpperCase    := pws_yes;      // Should Upper Case characters be Allowed/Required
      AlphaLowerCase    := pws_yes;      // Should Lower Case characters be Allowed/Required
      Numerals          := pws_yes;      // Should Numeric characters be Allowed/Required
      SpecialCharacters := pws_allowed;  // Should Special characters be Allowed/Required
      ExcludeSimilar    := pws_no;
      ExcludeAmbiguous  := pws_required;

      if ShowModal = mrOK then begin
        showmessage(Password + #13 + BoolToStr(RequirePasswordChange));
      end;
    end;
  finally
    FreeAndNil( PasswordChangeDialog );
  end;
```
