unit uBinaryHelper;

{$mode Delphi}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uToken, Variants;

type
  TBinaryHelper = record
  public
    class function LogicalOr(const Left, Right: Variant; Op: TKind): Variant; static;
    class function LogicalAnd(const Left, Right: Variant; Op: TKind): Variant; static;
  end;

implementation
class function TBinaryHelper.LogicalOr(const Left, Right: Variant; Op: TKind): Variant;
begin

end;

class function TBinaryHelper.LogicalAnd(const Left, Right: Variant; Op: TKind): Variant;
begin

end;

end.

