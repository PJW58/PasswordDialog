unit PasswordDialog;

{$mode objfpc}{$H+}

interface

uses
{==============================================================================|
| Units provided as part of the Compiler Runtime and Component Libraries       |
|==============================================================================}
  SysUtils,               // (RTL) System utilities unit
  Classes,                // (FCL) Classes Unit
{==============================================================================|
| LCL: Units provided as part of the Lazarus Component Library                 |
|==============================================================================}
  Forms,                  // (LCL) Forms Unit
  Buttons,                // (LCL) Button Components
  Clipbrd,                // (LCL) Clipboard Controls
  ComCtrls,               // (LCL) Common Controls
  Controls,               // (LCL) Component Library Controls
  Dialogs,                // (LCL) Standard Dialogs Controls
  EditBtn,                // (LCL) Edit Button Control
  ExtCtrls,               // (LCL) Extended Controls
  Graphics,               // (LCL) Graphic Controls
  StdCtrls,               // (LCL) Standard Controls
  Spin,                   // (LCL) Spin Control
{==============================================================================|
| Our Stuff                                                                    |
|==============================================================================}
  HashPassword;           // Password Based Key Derivation Functions

{==============================================================================|
| TPasswordChangeMode:                                                         |
|------------------------------------------------------------------------------|
| pcm_Change:  Show only the two password fields and the OK/Cancel Buttons     |
| pcm_Reset:   Show a full password reset dialog with all options              |
|==============================================================================}
type
  TPasswordChangeMode  = ( pcm_Change, pcm_Reset );

{==============================================================================|
| TPasswordRequirement:                                                        |
|------------------------------------------------------------------------------|
| Valid settings are:                                                          |
|   pws_no         - Characters will not be required, but user can change      |
|   pws_yes        - Characters will be required but user can change           |
|   pws_required   - Characters will be required  - user cannot change         |
|   pws_allowed    - Characters will be allowed but not required               |
|   pws_notallowed - Characters will not be allowed and user cannot change     |
|==============================================================================}
type
  TPasswordRequirement = ( pws_no, pws_yes, pws_required, pws_allowed, pws_notallowed );

{ TPasswordChangeDialog }

type
  TPasswordChangeDialog = class( TForm )
    bbCancel:          TBitBtn;
    bbOK:              TBitBtn;
    bbGenerate:        TBitBtn;
    bbToClip:          TBitBtn;
    ckAmbiguous:       TCheckBox;
    ckLowerCase:       TCheckBox;
    ckNumerals:        TCheckBox;
    cbRequireChange:   TCheckBox;
    ckSimilar:         TCheckBox;
    ckSpecial:         TCheckBox;
    ckUpperCase:       TCheckBox;
    ebPassword1:       TEditButton;
    ebPassword2:       TEditButton;
    ImageList1:        TImageList;
    lblPassword1:      TLabel;
    lblPassword2:      TLabel;
    lblLength:         TLabel;
    panelButtons:      TPanel;
    panelMain:         TPanel;
    panelRequirements: TPanel;
    seLength:          TSpinEdit;
    StatusBar1:        TStatusBar;

    procedure bbOKClick             ( Sender: TObject );
    procedure bbCancelClick         ( Sender: TObject );
    procedure bbGenerateClick       ( Sender: TObject );
    procedure bbToClipClick         ( Sender: TObject );
    procedure ebPassword1ButtonClick( Sender: TObject );
    procedure ebPassword2ButtonClick( Sender: TObject );
    procedure seLengthChange        ( Sender: TObject );

    procedure FormCreate            ( Sender: TObject );
    procedure FormShow              ( Sender: TObject );
    procedure FormDblClick          ( Sender: TObject );
    procedure RequirementsChange    ( Sender: TObject );

    function ValidatePassword( Sender: TObject ): boolean;

  private
    FSalt:          string;   // Salt - Set before calling
    FPassword:      string;   // Encoded Password
    FIterations:    integer;  // Number if iterations for Hash
    FPwdLength:     integer;  // Requested Password Length
    FMinLength:     integer;  // Minimum Password Length
    FMaxLength:     integer;  // Maximum Password Length
    FMode:          TPasswordChangeMode;
    FLowerCase:     TPasswordRequirement;
    FUpperCase:     TPasswordRequirement;
    FNumerals:      TPasswordRequirement;
    FSpecial:       TPasswordRequirement;
    FSimilar:       TPasswordRequirement;
    FAmbiguous:     TPasswordRequirement;
    FRequireChange: boolean;

  public
    // Write only properties to overide defaults

    property Salt:       string  write FSalt;
    property Iterations: integer write FIterations;
    property PwdLength:  integer write FPwdLength;
    property MinLength:  integer write FMinLength;
    property MaxLength:  integer write FMaxLength;
    property Mode:               TPasswordChangeMode  write FMode;
    property AlphaUpperCase:     TPasswordRequirement write FUpperCase;
    property AlphaLowerCase:     TPasswordRequirement write FLowerCase;
    property Numerals:           TPasswordRequirement write FNumerals;
    property SpecialCharacters:  TPasswordRequirement write FSpecial;
    property ExcludeSimilar:     TPasswordRequirement write FSimilar;
    property ExcludeAmbiguous:   TPasswordRequirement write FAmbiguous;


    // Read only properties to return Results
    property HashedPassword:        string  read FPassword;
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
  SIMILAR     = 'o0Oil|1';
  AMBIGUOUS   = ' +-~,;:.{}<>[]()/\''`"';

resourcestring
  ERR_SALTREQUIRED = 'A Salt is required to generate a password';
  ERR_NOTYPES      = 'No character types are allowed!';
  ERR_MAXTOSMALL   = 'Maximum password length of %d is too small to accommodate the other requirements';
  ERR_TOMUCHTIME   = 'It''s taking longer than expected to derive a password with these requirements';
  ERR_KEEPTRYING   = 'Do you want me to keep trying?';

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
  FMode          := pcm_Change;   // Default to User Change Dialog
  FIterations    := 127873;       // Strengh of Hash
  FMinLength     := 8;            // Minimum length
  FMaxLength     := 64;           // Maximum length
  FPwdLength     := -1;           // -1 signals use Min Length
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
    QuestionDlg( Caption, ERR_SALTREQUIRED, mtError, [mrCancel], '' );
    FRequireChange := False;
    FPassword      := '';
    ModalResult    := mrCancel;
    Close;
  end;

  bbCancel.Enabled := True;    // Can always Cancel
  bbOK.Enabled     := False;   // Enabled once passwords are valid
  bbToClip.Enabled := False;   // Enabled once passwords are valid

  ebPassword1.Text      := '';
  ebPassword2.Text      := '';
  ebPassword1.MaxLength := FMaxLength;
  ebPassword2.MaxLength := FMaxLength;


  if FMaxLength = FMinLength then begin
    seLength.MinValue := 0;
    seLength.MaxValue := 255;
    seLength.Enabled  := False;
  end else begin
    seLength.MinValue := FMinLength;
    seLength.MaxValue := FMaxLength;
    seLength.Enabled  := True;
  end;

  if FPwdLength < 0 then FPwdLength := FMinLength;
  seLength.Value := FPwdLength;

  ckLowerCase.Enabled := ( FLowerCase <> pws_required ) and ( FLowerCase <> pws_notallowed );
  ckUpperCase.Enabled := ( FUpperCase <> pws_required ) and ( FUpperCase <> pws_notallowed );
  ckNumerals.Enabled  := ( FNumerals  <> pws_required ) and ( FNumerals  <> pws_notallowed );
  ckSpecial.Enabled   := ( FSpecial   <> pws_required ) and ( FSpecial   <> pws_notallowed );
  ckSimilar.Enabled   := ( FSimilar   <> pws_required ) and ( FSimilar   <> pws_notallowed );
  ckAmbiguous.Enabled := ( FAmbiguous <> pws_required ) and ( FAmbiguous <> pws_notallowed );

  if not ckSimilar.Enabled then begin
    ckSimilar.Visible        := False;
    panelRequirements.Height := panelRequirements.Height - ckSimilar.Height;
  end;

  if not ckAmbiguous.Enabled then begin
    ckAmbiguous.Visible      := False;
    panelRequirements.Height := panelRequirements.Height - ckAmbiguous.Height;
  end;

  ckAmbiguous.Visible := ckAmbiguous.Enabled;

  ckLowerCase.State := pws2state( FLowerCase );
  ckUpperCase.State := pws2state( FUpperCase );
  ckNumerals.State  := pws2state( FNumerals );
  ckSpecial.State   := pws2state( FSpecial );
  ckSimilar.State   := pws2state( FSimilar );
  ckAmbiguous.State := pws2state( FAmbiguous );

  cbRequireChange.Checked := FRequireChange;

  case FMode of
    pcm_Reset:
      begin
        panelRequirements.Visible := True;
        PasswordChangeDialog.Height := panelMain.Height + panelRequirements.Height + StatusBar1.Height + 8;
      end;
    pcm_Change:
      begin
        panelRequirements.Visible := False;
        PasswordChangeDialog.Height := panelMain.Height + StatusBar1.Height + bbCancel.Height + 16;;
      end;
  end;

  panelRequirements.Enabled := panelRequirements.Visible;
  panelButtons.Visible      := panelRequirements.Visible;
  panelButtons.Enabled      := panelRequirements.Visible;
  ebPassword1.Width         := PasswordChangeDialog.Width - ( panelMain.Left + 195 );
  ebPassword2.Width         := ebPassword1.Width;
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
procedure TPasswordChangeDialog.bbToClipClick( Sender: TObject );
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
  bbOK.Enabled := ValidatePassword( Sender );
  bbToClip.Enabled := bbOK.Enabled;
  if bbToClip.Enabled
    then bbToClip.ImageIndex := 0
    else bbToClip.ImageIndex := 1;
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
  if ( ckUpperCase.State = cbChecked ) then ok := ok and ( aValue.IndexOfAny( UC_ALPHA    ) >= 0 );
  if ( ckLowerCase.State = cbChecked ) then ok := ok and ( aValue.IndexOfAny( LC_ALPHA    ) >= 0 );
  if ( ckNumerals.State  = cbChecked ) then ok := ok and ( aValue.IndexOfAny( NUMBERS     ) >= 0 );
  if ( ckSpecial.State   = cbChecked ) then ok := ok and ( aValue.IndexOfAny( PUNCTUATION ) >= 0 );

  // if it's excluded make sure it's NOT there
  if ( ckSimilar.State   = cbChecked ) then ok := ok and ( aValue.IndexOfAny( SIMILAR     ) < 0 );
  if ( ckAmbiguous.State = cbChecked ) then ok := ok and ( aValue.IndexOfAny( AMBIGUOUS   ) < 0 );

  Result := ok;
end;

{==============================================================================|
| btnGenerateClick: Generate a password based on current settings              |
|==============================================================================}
procedure TPasswordChangeDialog.bbGenerateClick( Sender: TObject );

var
  CH:        char;
  aPassword: string;
  Attempts:  integer;
  aSize:     integer;
  ALPHABET:  string;

begin
  ALPHABET := '';
  if ckLowerCase.State <> cbUnChecked then ALPHABET := ALPHABET + LC_ALPHA;
  if ckUpperCase.State <> cbUnChecked then ALPHABET := ALPHABET + UC_ALPHA;
  if ckNumerals.State  <> cbUnChecked then ALPHABET := ALPHABET + NUMBERS;
  if ckSpecial.State   <> cbUnChecked then ALPHABET := ALPHABET + PUNCTUATION;

  if ALPHABET = '' then begin
    QuestionDlg( Caption, ERR_NOTYPES, mtWarning, [mrOk], '' );
    exit;
  end;

  aSize := 0;
  if ckLowerCase.State = cbChecked then Inc( aSize );
  if ckUpperCase.State = cbChecked then Inc( aSize );
  if ckNumerals.State  = cbChecked then Inc( aSize );
  if ckSpecial.State   = cbChecked then Inc( aSize );

  if aSize > FMaxLength then begin
    QuestionDlg( Caption, format( ERR_MAXTOSMALL, [FMaxLength] ), mtWarning, [mrCancel], '' );
    ebPassword1.Text := '';
    ebPassword2.Text := '';
    exit;
  end;

  if aSize > seLength.Value then seLength.Value := aSize;

  Attempts := 0;
  ebPassword1.Text := '';
  ebPassword1.Update;
  ebPassword2.Text := '';
  ebPassword2.Update;
  repeat
    // if we have not found a valid combination after a large
    // number of attempts the minimum length is to short.
    // This should never happen because of the checks we do above
    // However, we'll put it here as a failsafe
    Inc( Attempts );
    if ( Attempts > 65535 ) then begin
      Attempts := 0;
      if seLength.Value >= FMaxLength then begin
        if QuestionDlg( Caption, ERR_TOMUCHTIME + #13 + ERR_KEEPTRYING, mtWarning, [mrYes, mrNo], '' ) <> mrYes then begin
          ebPassword1.Text := '';
          ebPassword2.Text := '';
          exit;
        end;
      end else begin
        seLength.Value := seLength.Value + 1;
        seLength.Update;
      end;
    end;

    // Build a password of the specified length
    aPassword := '';
    while aPassword.Length < seLength.Value do begin
      CH := ALPHABET[random( length( ALPHABET ) ) + 1];
      if ckSimilar.Checked and ( Pos( CH, SIMILAR ) > 0 ) then Continue;
      if ckAmbiguous.Checked and ( Pos( CH, AMBIGUOUS ) > 0 ) then Continue;
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
  if ebPassword1.PasswordChar <> #0
    then ebPassword1.PasswordChar := #0
    else ebPassword1.PasswordChar := '#';
end;

{==============================================================================|
| ebPassword2ButtonClick: Toggle display mode for Second Password              |
|==============================================================================}
procedure TPasswordChangeDialog.ebPassword2ButtonClick( Sender: TObject );
begin
  if ebPassword2.PasswordChar <> #0
    then ebPassword2.PasswordChar := #0
    else ebPassword2.PasswordChar := '#';
end;

{==============================================================================|
| FormDblClick: Toggle Between Password Reset and Password Change mode         |
|==============================================================================}
procedure TPasswordChangeDialog.FormDblClick( Sender: TObject );
begin
  if ( FMode = pcm_Reset ) then begin
    panelRequirements.Visible := not panelRequirements.Visible;
    panelRequirements.Enabled := panelRequirements.Visible;
    panelButtons.Visible      := panelRequirements.Visible;
    panelButtons.Enabled      := panelRequirements.Visible;

    if panelRequirements.Visible then begin
      PasswordChangeDialog.Height := panelMain.Height + panelRequirements.Height + StatusBar1.Height + 8;
    end else begin
      PasswordChangeDialog.Height := panelMain.Height + StatusBar1.Height + bbCancel.Height + 16;
    end;

    ebPassword1.Width := PasswordChangeDialog.Width - ( panelMain.Left + 195 );
    ebPassword2.Width := ebPassword1.Width;
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

end.
