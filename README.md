# PasswordDialog
 Password Change Dialog with PBKDF2 Hashing
 
<img src="https://img.shields.io/badge/Compiler-Free%20Pascal%20%2F%20Lazarus-brightgreen">
 
<img src="https://img.shields.io/badge/License-Modified%20LGPL%20with%20Linking%20exception-brightgreen">

 Author:  Paul J West
 
 Third Party Libraries:
 
 1. Hash Library for Pascal 
    <[wiki](https://wiki.freepascal.org/HashLib4Pascal)>
    <[GitHub](https://github.com/Xor-el/HashLib4Pascal)>
 
 2. <[IconDB](https://IconsDB.com)> CC0 1.0 Universal Public Domain Dedication.
 
<img src="./img/changedialog.png">
<img src="./img/resetdialog.png">
   
Usage:

The easiest way to use these dialogs is to simply call using two Quick Call functions provided.
The defaults will be used for all password difficulty requirements.

```Pascal
OK := PCD_PasswordReset( Parent, UserID, NewPassword, ChangeRequired );
if OK = mrok then ...
```

```Pascal
OK := PCD_PasswordChange( Parent, UserID, NewPassword);
if OK = mrok then ...
```

If you need more control
   
```Pascal
  PasswordChangeDialog := TPasswordChangeDialog.Create( Parent );
  try
    // All of the parameters except for Salt are optional
    // Each has a default, which may or may not suit your needs
    with PasswordChangeDialog do begin
      Caption           := 'Password Reset: ' + UserID;
      Mode              := pcm_Reset;    // pcm_Reset or pcm_Change
      Iterations        := 429937;       // Number of Iterations for Hash Routine
      Salt              := UserID;       // Usually the UserID, but you can get creative...
      PwdLength         := 12;           // If not specified Default is Minimum Length
      MinLength         := 1;            // Minimum Password length - Default 8
      MaxLength         := 48;           // Maximum Password length - Default 64
      AlphaUpperCase    := pws_required; // Should Upper Case characters be Allowed/Required
      AlphaLowerCase    := pws_yes;      // Should Lower Case characters be Allowed/Required
      Numerals          := pws_yes;      // Should Numeric characters be Allowed/Required
      SpecialCharacters := pws_allowed;  // Should Special characters be Allowed/Required
      ExcludeSimilar    := pws_yes;      // Should we exclude characters that look very similar
      ExcludeAmbiguous  := pws_yes;      // Should we exclude characters know to confuse some apps

      Result := ShowModal;
      if Result = mrOK then begin
	  {-- or whatever you need to do here!
        User.Password           := HashedPassword;
        User.PasswordExpired    := RequirePasswordChange;
        User.LastPasswordChange := now();
        User.Save;
      --}
      end;
    end;
  finally
    FreeAndNil( PasswordChangeDialog );
  end;
```
or for a quicky password change

```pascal
PasswordChangeDialog := TPasswordChangeDialog.Create( Parent );
try
  PasswordChangeDialog.Mode := pcm_Change; // pcm_Reset or pcm_Change
  PasswordChangeDialog.Salt := UserID;     // Usually the UserID, but you can get creative...
  if PasswordChangeDialog.ModalResult = mrOK then begin
  {-- or whatever you need to do here!
    User.Password := PasswordChangeDialog.HashedPassword;
    User.LastPasswordChange := now();
    User.Save;
  --}
  end;
finally
  freeandnil(PasswordChangeDialog);
end;
```

Explanation of Parameters

-Caption

   Caption from Lazarus TForm object
   
-Iterations

-Salt

-PwdLength

-MinLength

-MaxLength

-AlphaUpperCase

-AlphaLowerCase

-Numerals

-SpecialCharacters

-ExcludeSimilar

-ExcludeAmbiguous
