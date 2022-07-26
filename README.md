# PasswordDialog
 Password Change Dialog with PBKDF2 Hashing
 
 Author:  Paul J West
 
 License: Modified LGPL with Linking Exception
 
 Third Party Libraries:
   Hash Library for Pascal - MIT License
   Some Icons from IconsDB.com - CC0 1.0 Universal Public Domain Dedication.
   
Usage: 
```Pascal
 function ResetPassword(
   const UserID: string;
   out   NewPassword: string;
   out   RequireReset: boolean);
 
 begin
   PasswordChangeDialog := TPasswordChangeDialog.Create( Nil );
   try
    // All of the parameters except for Salt are optional
    // Each has a default, which may or may not suit your needs
    with PasswordChangeDialog do begin
      Caption           := 'Password Reset';
      Iterations        := 429937;       // Number of Iterations for Hash Routine
      Salt              := UserID;       // Usually the UserID, but you can get creative...
      MinLength         := 12;           // Minimum Password length
      MaxLength         := 64;           // Maximum Password length
      AlphaUpperCase    := pws_yes;      // Should Upper Case characters be Allowed/Required
      AlphaLowerCase    := pws_yes;      // Should Lower Case characters be Allowed/Required
      Numerals          := pws_yes;      // Should Numeric characters be Allowed/Required
      SpecialCharacters := pws_allowed;  // Should Special characters be Allowed/Required
      ExcludeSimilar    := pws_no;
      ExcludeAmbiguous  := pws_required;

      if ShowModal = mrOK then begin
        NewPassword  := Password;
		RequireReset := RequirePasswordChange;
      end;
    end;
  finally
    FreeAndNil( PasswordChangeDialog );
  end;
end;
```

Explanation of Parameters

-Caption
   Caption from Lazarus TForm object
   
-Iterations
-Salt
-MinLength
-MaxLength
-AlphaUpperCase
-AlphaLowerCase
-Numerals
-SpecialCharacters
-ExcludeSimilar
-ExcludeAmbiguous