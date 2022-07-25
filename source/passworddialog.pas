unit PasswordDialog;

{$mode objfpc}{$H+}

interface

uses
 {============================================================================|
  | Units provided as part of the Compiler Runtime and Component Libraries     |
  |============================================================================}
  SysUtils,               // (RTL) System utilities unit
  Classes,                // (FCL) Classes Unit
  {============================================================================|
  | LCL: Units provided as part of the Lazarus Component Library               |
  |============================================================================}
  Forms,                  // (LCL) Forms Unit
  Buttons,                // (LCL) Button Components
  Clipbrd,                // (LCL) Clipboard Controls
  ComCtrls,               // (LCL) Common Controls
  Controls,               // (LCL) Component Library Controls
  Dialogs,                // (LCL) Standard Dialogs Controls
  EditBtn,                // (LCL) Edit Button Control
  Graphics,               // (LCL) Graphic Controls
  StdCtrls,               // (LCL) Standard Controls
  Spin,                   // (LCL) Spin Control
  {============================================================================|
  | Our Stuff                                                                  |
  |============================================================================}
  HashPassword;           // Password Based Key Derivation Functions

type
  TPasswordRequirement = ( pws_no, pws_yes, pws_required, pws_allowed, pws_notallowed );

{ TPasswordChangeDialog }

type
  TPasswordChangeDialog = class( TForm )
    bbCancel:        TBitBtn;
    bbOK:            TBitBtn;
    btnGenerate:     TButton;
    btnToClip:       TButton;
    cbLowerCase:     TCheckBox;
    cbUpperCase:     TCheckBox;
    cbNumerals:      TCheckBox;
    cbSpecial:       TCheckBox;
    cbSimilar:       TCheckBox;
    cbAmbiguous:     TCheckBox;
    cbRequireChange: TCheckBox;
    ebPassword1:     TEditButton;
    ebPassword2:     TEditButton;
    gbMain:          TGroupBox;
    cbRequirements:  TGroupBox;
    gbButtons:       TGroupBox;
    Label2:          TLabel;
    Label3:          TLabel;
    Label4:          TLabel;
    seLength:        TSpinEdit;
    StatusBar1:      TStatusBar;

    procedure bbOKClick( Sender: TObject );
    procedure bbCancelClick( Sender: TObject );
    procedure btnGenerateClick( Sender: TObject );
    procedure btnToClipClick( Sender: TObject );
    procedure ebPassword1ButtonClick( Sender: TObject );
    procedure ebPassword2ButtonClick( Sender: TObject );
    procedure seLengthChange( Sender: TObject );

    procedure FormCreate( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure RequirementsChange( Sender: TObject );

    function ValidatePassword( Sender: TObject ): boolean;

  private
    FSalt:          string;   // Salt - Set before calling
    FPassword:      string;   // Encoded Password
    FIterations:    integer;
    FMinLength:     integer;
    FMaxLength:     integer;
    FLowerCase:     TPasswordRequirement;
    FUpperCase:     TPasswordRequirement;
    FNumerals:      TPasswordRequirement;
    FSpecial:       TPasswordRequirement;
    FSimilar:       TPasswordRequirement;
    FAmbiguous:     TPasswordRequirement;
    FRequireChange: boolean;

  private
    // Property Write Specifiers
    procedure SetSalt( aValue: string );
    procedure SetMinLength( aValue: integer );
    procedure SetMaxLength( aValue: integer );
    procedure SetLowerCase( aValue: TPasswordRequirement );
    procedure SetUpperCase( aValue: TPasswordRequirement );
    procedure SetNumerals( aValue: TPasswordRequirement );
    procedure SetSpecial( aValue: TPasswordRequirement );
    procedure SetExcludeSimilar( aValue: TPasswordRequirement );
    procedure SetExcludeAmbiguous( aValue: TPasswordRequirement );

  public
    // Write only properties to overide defaults
    property Iterations:        integer write FIterations;
    property Salt:              string  write SetSalt;
    property MinLength:         integer write SetMinLength;
    property MaxLength:         integer write SetMaxLength;
    property AlphaUpperCase:    TPasswordRequirement write SetUpperCase;
    property AlphaLowerCase:    TPasswordRequirement write SetLowerCase;
    property Numerals:          TPasswordRequirement write SetNumerals;
    property SpecialCharacters: TPasswordRequirement write SetSpecial;
    property ExcludeSimilar:    TPasswordRequirement write SetExcludeSimilar;
    property ExcludeAmbiguous:  TPasswordRequirement write SetExcludeAmbiguous;


    // Read only properties to return Results
    property Password:              string  read FPassword;
    property RequirePasswordChange: boolean read FRequireChange;
  end;

var
  PasswordChangeDialog: TPasswordChangeDialog;

implementation

{$R *.lfm}

const
  PUNCTUATION = '!"#$%&''()*+,-./:;<=>?@[\]^_`{|}~';
  UC_ALPHA    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LC_ALPHA    = 'abcdefghijklmnopqrstuvwxyz';
  NUMBERS     = '0123456789';
  SIMILAR     = 'oO0il1';
  AMBIGUOUS   = ' +-~,;:.{}<>[]()/\''`"';

{ TPasswordChangeDialog }

{==============================================================================|
| FormCreate: Initial Form Creation                                            |
|------------------------------------------------------------------------------|
| This is the equivalent to the class constructor in forms.                    |
|==============================================================================}
procedure TPasswordChangeDialog.FormCreate( Sender: TObject );
begin
  Randomize;                      // Seed random number generator

  // Set Defaults - Can be overidden by parent
  FIterations    := 127873;       // Strengh of Hash
  FMinLength     := 8;            // Minimum length
  FMaxLength     := 64;           // Maximum length
  FLowerCase     := pws_yes;      // Require a lower case character
  FUpperCase     := pws_yes;      // Require an upper case character
  FNumerals      := pws_yes;      // Require a number
  FSpecial       := pws_yes;      // Require a Special Character
  FSimilar       := pws_no;       // Do not block similar characters
  FAmbiguous     := pws_required; // Block Ambiguous characters and do not allow change
  FRequireChange := False;
end;

{==============================================================================|
| FormShow:                                                                    |
|------------------------------------------------------------------------------|
| When the form is shown this event is fired - just before the form is visible |
|==============================================================================}
procedure TPasswordChangeDialog.FormShow( Sender: TObject );

  function pws2state( pws: TPasswordRequirement ): TCheckBoxState;
  begin
    case pws of
      pws_no: Result         := cbUnchecked;
      pws_yes: Result        := cbChecked;
      pws_required: Result   := cbChecked;
      pws_allowed: Result    := cbGrayed;
      pws_notallowed: Result := cbUnchecked;
    end;
  end;

begin
  if ( FSalt = '' ) then begin
    QuestionDlg( Caption, 'A Salt is required to generate a password', mtError, [mrCancel], '' );
    FRequireChange := False;
    FPassword      := '';
    ModalResult    := mrCancel;
    Close;
  end;

  bbCancel.Enabled  := True;    // Can always Cancel
  bbOK.Enabled      := False;   // Enabled once passwords are valid
  btnToClip.Enabled := False;   // Enabled once passwords are valid

  ebPassword1.Text      := '';
  ebPassword2.Text      := '';
  ebPassword1.MaxLength := FMaxLength;
  ebPassword2.MaxLength := FMaxLength;

  seLength.MinValue := FMinLength;
  seLength.MaxValue := FMaxLength;
  seLength.Value    := FMinLength;

  cbLowerCase.Enabled := ( FLowerCase <> pws_required ) and ( FLowerCase <> pws_notallowed );
  cbUpperCase.Enabled := ( FUpperCase <> pws_required ) and ( FUpperCase <> pws_notallowed );
  cbNumerals.Enabled  := ( FNumerals <> pws_required ) and ( FNumerals <> pws_notallowed );
  cbSpecial.Enabled   := ( FSpecial <> pws_required ) and ( FSpecial <> pws_notallowed );
  cbSimilar.Enabled   := ( FSimilar <> pws_required ) and ( FSimilar <> pws_notallowed );
  cbAmbiguous.Enabled := ( FAmbiguous <> pws_required ) and ( FAmbiguous <> pws_notallowed );

  cbLowerCase.State := pws2state( FLowerCase );
  cbUpperCase.State := pws2state( FUpperCase );
  cbNumerals.State  := pws2state( FNumerals );
  cbSpecial.State   := pws2state( FSpecial );
  cbSimilar.State   := pws2state( FSimilar );
  cbAmbiguous.State := pws2state( FAmbiguous );

  cbRequireChange.Checked := FRequireChange;
end;

{==============================================================================|
| bbCancelClick:                                                               |
|------------------------------------------------------------------------------|
| Closes the form with an mrCancel result                                      |
|==============================================================================}
procedure TPasswordChangeDialog.bbCancelClick( Sender: TObject );
begin
  FPassword      := '';
  FRequireChange := False;
end;

{==============================================================================|
| bbOKClick:                                                                   |
|------------------------------------------------------------------------------|
| Hash the password, and exit with mrOK result code                            |
| Returns:  New Hashed Password                                                |
|           Flag for calling program to set required password change flag      |
|==============================================================================}
procedure TPasswordChangeDialog.bbOKClick( Sender: TObject );
begin
  FPassword      := GeneratePKDF2( FSalt, ebPassword1.Text, FIterations );
  FRequireChange := cbRequireChange.Checked;
end;

{==============================================================================|
| btnToClipClick:                                                              |
|------------------------------------------------------------------------------|
| Copies the password to the clipboard                                         |
|==============================================================================}
procedure TPasswordChangeDialog.btnToClipClick( Sender: TObject );
begin
  Clipboard.AsText := ebPassword1.Text;
end;

{==============================================================================|
| RequirementsChange: Re-Validate password based on updated settings           |
|------------------------------------------------------------------------------|
| This procedure is called by most of the settings controls.                   |
| Many call it directly as their onchange event                                |
|==============================================================================}
procedure TPasswordChangeDialog.RequirementsChange( Sender: TObject );
begin
  bbOK.Enabled      := ValidatePassword( Sender );
  btnToClip.Enabled := bbOK.Enabled;
end;

{==============================================================================|
| ValidatePassword: Validate password based on settings                        |
|------------------------------------------------------------------------------|
| Verify that the entered passwords match complexity criteria selected         |
|==============================================================================}
function TPasswordChangeDialog.ValidatePassword( Sender: TObject ): boolean;

var
  ok:     boolean;
  aValue: string;

begin
  aValue := ebPassword1.Text;
  ok     := ( ebPassword1.Text = ebPassword2.Text );
  ok     := ok and ( aValue.Length >= seLength.Value );
  ok     := ok and ( aValue.Length <= FMaxLength );

  // if "Required" make it's there!
  if ( cbUpperCase.State = cbChecked ) then
    ok := ok and ( aValue.IndexOfAny( UC_ALPHA ) >= 0 );
  if ( cbLowerCase.State = cbChecked ) then
    ok := ok and ( aValue.IndexOfAny( LC_ALPHA ) >= 0 );
  if ( cbNumerals.State = cbChecked ) then
    ok := ok and ( aValue.IndexOfAny( NUMBERS ) >= 0 );
  if ( cbSpecial.State = cbChecked ) then
    ok := ok and ( aValue.IndexOfAny( PUNCTUATION ) >= 0 );

  // if it's excluded make sure it's NOT there
  if ( cbSimilar.State = cbChecked ) then
    ok := ok and ( aValue.IndexOfAny( SIMILAR ) < 0 );
  if ( cbAmbiguous.State = cbChecked ) then
    ok := ok and ( aValue.IndexOfAny( AMBIGUOUS ) < 0 );

  Result := ok;
end;

{==============================================================================|
| btnGenerateClick: Generate a password based on current settings              |
|==============================================================================}
procedure TPasswordChangeDialog.btnGenerateClick( Sender: TObject );

var
  CH:        char;
  aPassword: string;
  Attempts:  integer;
  ALPHABET:  string;

begin
  ALPHABET := '';
  if cbLowerCase.State <> cbUnChecked then
    ALPHABET := ALPHABET + LC_ALPHA;
  if cbUpperCase.State <> cbUnChecked then
    ALPHABET := ALPHABET + UC_ALPHA;
  if cbNumerals.State <> cbUnChecked then
    ALPHABET := ALPHABET + NUMBERS;
  if cbSpecial.State <> cbUnChecked then
    ALPHABET := ALPHABET + PUNCTUATION;

  if ALPHABET = '' then begin
    QuestionDlg( Caption, 'No character type are allowed', mtWarning, [mrOk], '' );
    exit;
  end;

  Attempts         := 0;
  ebPassword1.Text := '';
  ebPassword1.Update;
  ebPassword2.Text := '';
  ebPassword2.Update;
  repeat
    // if we have not found a valid combination after a large
    // number of attempts the minimum length is to short.
    Inc( Attempts );
    if ( Attempts > 4096 ) then begin
      seLength.Value := seLength.Value + 1;
      seLength.Update;
      Attempts := 0;
    end;

    // Build a password of the specified length
    aPassword := '';
    while aPassword.Length < seLength.Value do begin
      CH := ALPHABET[random( length( ALPHABET ) ) + 1];
      if cbSimilar.Checked and ( Pos( CH, SIMILAR ) > 0 ) then
        Continue;
      if cbAmbiguous.Checked and ( Pos( CH, AMBIGUOUS ) > 0 ) then
        Continue;
      aPassword := aPassword + CH;
    end;

    // Validate it meets all criteria
    ebPassword1.Text := aPassword;
    ebPassword2.Text := aPassword;
    ebPassword1.Update;
  until bbOK.Enabled;

  cbRequireChange.Checked := True;
end;

{==============================================================================|
| ebPassword1ButtonClick: Toggle display mode for First Password               |
|==============================================================================}
procedure TPasswordChangeDialog.ebPassword1ButtonClick( Sender: TObject );
begin
  case ebPassword1.EchoMode of
    emPassword: ebPassword1.EchoMode := emNormal;
    emNormal: ebPassword1.EchoMode   := emNone;
    emNone: ebPassword1.EchoMode     := emPassword;
  end;
end;

{==============================================================================|
| ebPassword2ButtonClick: Toggle display mode for Second Password              |
|==============================================================================}
procedure TPasswordChangeDialog.ebPassword2ButtonClick( Sender: TObject );
begin
  case ebPassword2.EchoMode of
    emPassword: ebPassword2.EchoMode := emNormal;
    emNormal: ebPassword2.EchoMode   := emNone;
    emNone: ebPassword2.EchoMode     := emPassword;
  end;
end;

{==============================================================================|
| seLengthChange: Change of Password Length                                    |
|==============================================================================}
procedure TPasswordChangeDialog.seLengthChange( Sender: TObject );
begin
  ebPassword1.MaxLength := FMaxLength;
  ebPassword2.MaxLength := FMaxLength;
  RequirementsChange( Sender );
end;


{==============================================================================|
| Property Specifiers - Receive parameters from parent                         |
|==============================================================================}

{==============================================================================|
| SetSalt: Update Hash Salt                                                    |
|------------------------------------------------------------------------------|
| By convention the hash salt is the UserID of the user.  This provides a      |
| different salt value fpr each user, rendering rainbow tables innefective     |
|==============================================================================}
procedure TPasswordChangeDialog.SetSalt( aValue: string );
begin
  FSalt := aValue;
end;

{==============================================================================|
| SetMinLength: Update password minimum length from parent                     |
|==============================================================================}
procedure TPasswordChangeDialog.SetMinLength( aValue: integer );
begin
  FMinLength := aValue;
end;

{==============================================================================|
| SetMaxLength: Update password maximum length from parent                     |
|==============================================================================}
procedure TPasswordChangeDialog.SetMaxLength( aValue: integer );
begin
  FMaxLength := aValue;
end;

{==============================================================================|
| SetLowerCase: Update Lower Case setting from parent                          |
|------------------------------------------------------------------------------|
| Valid settings are:                                                          |
|   pws_no         - Characters will not be required, but user can change      |
|   pws_yes        - Characters will be required but user can change           |
|   pws_required   - Characters will be required  - user cannot change         |
|   pws_allowed    - Characters will be allowed but not required               |
|   pws_notallowed - Characters will not be allowed and user cannot change     |
|==============================================================================}
procedure TPasswordChangeDialog.SetLowerCase( aValue: TPasswordRequirement );
begin
  FLowerCase := aValue;
end;

{==============================================================================|
| SetUpperCase: Update Upper Case setting from parent                          |
|------------------------------------------------------------------------------|
| Allowed settings are the same as for lower case                              |
|==============================================================================}
procedure TPasswordChangeDialog.SetUpperCase( aValue: TPasswordRequirement );
begin
  FUpperCase := aValue;
end;

{==============================================================================|
| SetNumerals: Update Numerals setting from parent                             |
|------------------------------------------------------------------------------|
| Allowed settings are the same as for lower case                              |
|==============================================================================}
procedure TPasswordChangeDialog.SetNumerals( aValue: TPasswordRequirement );
begin
  FNumerals := aValue;
end;

{==============================================================================|
| SetSpecial: Update Special Character setting from parent                     |
|------------------------------------------------------------------------------|
| Allowed settings are the same as for lower case                              |
|==============================================================================}
procedure TPasswordChangeDialog.SetSpecial( aValue: TPasswordRequirement );
begin
  FSpecial := aValue;
end;

{==============================================================================|
| SetExcludeSimilar: Update the exclude similar characters setting from parent |
|------------------------------------------------------------------------------|
| Allowed settings are the same as for lower case except that pws_allowed      |
| has no meaning here and should not be used.                                  |
|==============================================================================}
procedure TPasswordChangeDialog.SetExcludeSimilar( aValue: TPasswordRequirement );
begin
  FSimilar := aValue;
end;

{==============================================================================|
| SetExcludeAmbiguous: Update the exclude anbiguous characters setting         |
|------------------------------------------------------------------------------|
| Allowed settings are the same as for lower case except that pws_allowed      |
| has no meaning here and should not be used.                                  |
|==============================================================================}
procedure TPasswordChangeDialog.SetExcludeAmbiguous( aValue: TPasswordRequirement );
begin
  FAmbiguous := aValue;
end;

end.
