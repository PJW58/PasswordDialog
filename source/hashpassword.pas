unit HashPassword;

{$mode objfpc}{$H+}
{==============================================================================|
| PBKDF2 is part of RSA Laboratories Public-Key Cryptography Standards (PKCS)  |
| series,specifically PKCS #5 v2.0, also published as Internet Engineering     |
| Task Force`s RFC-2898. It supersedes PBKDF1, which could only produce        |
| derived keys up to 160 bits long. RFC 8018 (PKCS #5 v2.1), published         |
| in 2017, recommends PBKDF2 for password hashing.                             |
|------------------------------------------------------------------------------|
| This unit is a wrapper around HashLib4Pascal available thru the Lazarus      |
| online package manager.  https://github.com/Xor-el/HashLib4Pascal/           |
|==============================================================================}
interface

uses
  Classes,
  SysUtils,
  HlpIHash,
  HlpHashFactory,
  HlpConverters,
  HlpSHA2_512,
  HlpIHashInfo;

function GenerateSHA512 ( UserID, Password: string ): string;
function GeneratePKDF2  ( UserID, Password: string; Iterations: integer = 65536*8 ): string;

implementation

function GenerateSHA512( UserID, Password: string): string;

var
  Hash: IHash;

begin
  Hash   := TSHA2_512.Create();
  Result := hash.ComputeString(UserID+Password, TEncoding.UTF8).ToString();
end;

function GeneratePKDF2( UserID, Password: string; Iterations: integer = 65536*8 ): string;

var
  BytePassword, ByteSalt: TBytes;
  PBKDF2_HMACInstance: IPBKDF2_HMAC;

begin
  ByteSalt     := TConverters.ConvertStringToBytes( UserID,   TEncoding.UTF8 );
  BytePassword := TConverters.ConvertStringToBytes( Password, TEncoding.UTF8 );
  PBKDF2_HMACInstance := TKDF.TPBKDF2_HMAC.CreatePBKDF2_HMAC(
    THashFactory.TCrypto.CreateSHA2_512(),
    BytePassword,
    ByteSalt,
    Iterations);

  Result := TConverters.ConvertBytesToHexString(PBKDF2_HMACInstance.GetBytes(64), False);
end;

end.
