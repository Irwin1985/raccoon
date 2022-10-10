unit uError;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Dialogs, uToken;

type

  { TErrorItem }
  TErrorItem = class
    Line, Col: integer;
    Msg: string;
    constructor Create(const ALine, ACol: integer; const AMsg: string);
    constructor Create(AToken: TToken; const AMsg: string);
    function toString: string; override;
  end;

  { TError }
  TErrors = class(specialize TObjectList<TErrorItem>)
    procedure Append(const ALine, ACol: integer; const AMsg: string);
    procedure Append(AToken: TToken; const AMsg: string);
    function isEmpty: boolean;
    procedure Reset;
    function toString: string; override;
  end;

  { EParseError }
  EParseError = class(Exception);

  { ERuntimeError }
  ERuntimeError = class(Exception)
    Token: TToken;
    constructor Create(AToken: TToken; AMessage: string);
  end;

  { EReturnException }
  EReturnException = class(Exception)
    Value: Variant;
    constructor Create(AValue: Variant);
  end;


procedure RuntimeError(E: ERuntimeError);

var
  Errors: TErrors;

implementation
(************************************************************************
* TErrorItem
************************************************************************)
constructor TErrorItem.Create(const ALine, ACol: integer; const AMsg: string);
begin
  Line := ALine;
  Col := ACol;
  Msg := AMsg;
end;

constructor TErrorItem.Create(AToken: TToken; const AMsg: string);
begin
  Line := AToken.Line;
  Col := AToken.Col;
  Msg := AMsg;
end;

function TErrorItem.toString: string;
begin
  Result := Format('@[%d:%d] %s', [Line, Col, Msg]);
end;
(************************************************************************
* TErrors
************************************************************************)
procedure TErrors.Append(const ALine, ACol: integer; const AMsg: string);
begin
  Add(TErrorItem.Create(ALine, ACol, AMsg));
end;

procedure TErrors.Append(AToken: TToken; const AMsg: string);
begin
  Add(TErrorItem.Create(AToken, AMsg));
end;

function TErrors.isEmpty: boolean;
begin
  Result := Count = 0;
end;

procedure TErrors.Reset;
begin
  Clear;
end;

function TErrors.toString: string;
var
  Item: TErrorItem;
begin
  Result := 'Errors:' + LineEnding;
  for Item in Self do
    Result += Item.toString + LineEnding;
end;
(************************************************************************
* Procedure RuntimeError
************************************************************************)
procedure RuntimeError(E: ERuntimeError);
const
  ErrString = '@[%d:%d] Runtime error: %s';
begin
  with E.Token do
    ShowMessage(Format(ErrString, [Line, Col, E.Message]));
  Exit;
end;
(************************************************************************
* ERuntimeError
************************************************************************)
constructor ERuntimeError.Create(AToken: TToken; AMessage: string);
begin
  Token := AToken;
  inherited Create(AMessage);
end;
(************************************************************************
* EReturnException
************************************************************************)
constructor EReturnException.Create(AValue: Variant);
begin
  Value := AValue;
  inherited Create('return');
end;
(************************************************************************
* Initialization
************************************************************************)
initialization
  Errors := TErrors.Create();
(************************************************************************
* Finalization
************************************************************************)
finalization
  Errors.Free;

end.

