unit uEnvironment;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, uToken, uError, uAst, Variants;

type
  TVarMode = (vmVariable, vmConstant);

  TVarTuple = record
    FVarMode: TVarMode;
    FValue: Variant;
  end;

  TRecord = TDictionary<string, TVarTuple>;

  TEnvironment = class;

  // Function Record
  TFunctionPackage = class
  private
    FFuncAST: TFunctionStatement;
    FEnv: TEnvironment;
  public
    constructor Create(AFuncAST: TFunctionStatement; AEnv: TEnvironment);
    destructor Destroy; override;
    property FuncAST: TFunctionStatement read FFuncAST;
    property Env: TEnvironment read FEnv;
  end;


  TEnvironment = class
  private
    FRecord: TRecord;
    FParent: TEnvironment;
    // Function Constant Table
    FuncTable: TObjectList<TFunctionPackage>;
  public
    constructor Create(ARecord: TRecord; AParent: TEnvironment);
    destructor Destroy; override;
    procedure Define(AToken: TToken; AValue: Variant; AMode: TVarMode);
    function Lookup(AToken: TToken): Variant;
    procedure Assign(AToken: TToken; AValue: Variant);
    function Resolve(AToken: TToken; throwError: boolean): TEnvironment;

    function AddFunction(AFuncAST: TFunctionStatement): integer;
    function GetFunction(AToken: TToken): TFunctionPackage;
  end;

implementation
  { TEnvironment }
  constructor TEnvironment.Create(ARecord: TRecord; AParent: TEnvironment);
  begin
    FuncTable := TObjectList<TFunctionPackage>.Create(False);
    if Assigned(ARecord) then
      FRecord := ARecord
    else
      FRecord := TDictionary<string, TVarTuple>.Create();

    if Assigned(AParent) then
      FParent := AParent
    else
      FParent := nil;
  end;

  destructor TEnvironment.Destroy;
  begin
    if Assigned(FRecord) then FRecord.Free;
    if Assigned(FParent) then FParent.Free;
    if Assigned(FuncTable) then FuncTable.Free;
  end;

  procedure TEnvironment.Define(AToken: TToken; AValue: Variant; AMode: TVarMode);
  var
    Tuple: TVarTuple;
  begin
    if FRecord.TryGetValue(LowerCase(AToken.Literal), Tuple) then
      raise ERuntimeError.Create(AToken, 'Variable ' + AToken.Literal + ' already exists.');

    Tuple.FVarMode := AMode;
    Tuple.FValue := AValue;

    FRecord.Add(LowerCase(AToken.Literal), Tuple);
  end;

  function TEnvironment.Lookup(AToken: TToken): Variant;
  var
    Env: TEnvironment;
    Tuple: TVarTuple;
  begin
    Env := Resolve(AToken, True);
    if not Env.FRecord.TryGetValue(LowerCase(AToken.Literal), Tuple) then
      raise ERuntimeError.Create(AToken, 'Variable or Constant not defined: ' + AToken.Literal);

    Result := Tuple.FValue // Unwrap the Value
  end;

  procedure TEnvironment.Assign(AToken: TToken; AValue: Variant);
  var
    Env: TEnvironment;
    Tuple: TVarTuple;
  begin
    Env := Resolve(AToken, False);
    if Env = Nil then
      Define(AToken, AValue, vmVariable) // Define in local environment.
    else
    begin
      // Only variables can be reassigned
      if Env.FRecord.TryGetValue(LowerCase(AToken.Literal), Tuple) then
      begin
        if Tuple.FVarMode = vmVariable then
        begin
          Tuple.FValue := AValue;
          Env.FRecord.AddOrSetValue(LowerCase(AToken.Literal), Tuple)
        end
        else
          raise ERuntimeError.Create(AToken, 'Invalid assignment for constant values.');
      end;
    end;
  end;

  function TEnvironment.Resolve(AToken: TToken; throwError: boolean): TEnvironment;
  var
    Tuple: TVarTuple;
  begin
    if not FRecord.TryGetValue(LowerCase(AToken.Literal), Tuple) then
    begin
      if FParent <> Nil then
        Exit(FParent.Resolve(AToken, throwError))
      else
        if throwError then
          raise ERuntimeError.Create(AToken, 'Variable not defined: ' + AToken.Literal)
        else
          Exit(Nil);
    end;
    Result := Self;
  end;

  function TEnvironment.AddFunction(AFuncAST: TFunctionStatement): integer;
  var
    FuncPackage: TFunctionPackage;
  begin
    FuncPackage := TFunctionPackage.Create(AFuncAST, Self);
    FuncTable.Add(FuncPackage);

    Result := FuncTable.Count-1;
  end;
  (************************************************************************
  * GetFunction
  ************************************************************************)
  function TEnvironment.GetFunction(AToken: TToken): TFunctionPackage;
  var
    Env: TEnvironment;
    FuncID: Variant;
  begin
    Env := Resolve(AToken, True);

    FuncID := Env.Lookup(AToken);

    Result := Env.FuncTable[FuncID];
  end;
  (************************************************************************
  * TFunctionPackage
  ************************************************************************)
  constructor TFunctionPackage.Create(AFuncAST: TFunctionStatement; AEnv: TEnvironment);
  begin
    FFuncAST := AFuncAST;
    FEnv := AEnv;
  end;

  destructor TFunctionPackage.Destroy;
  begin
    if Assigned(FFuncAST) then FFuncAST.Free;
    if Assigned(FEnv) then FEnv.Free;
  end;

end.
