unit uInterpreter;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Dialogs,
  uToken,
  uAst,
  Variants,
  uError,
  uEnvironment;

type
  { BinaryHelper }
  TBinaryFunc = function(Left, Right: Variant): Variant of object;

  { TInterpreter }
  TInterpreter = class(TVisitor)
  private
    FCurrentEnv: TEnvironment;
    FGlobalEnv: TEnvironment;

    FBinaryDisp: TDictionary<extended, TBinaryFunc>;
    function Evaluate(expr: TExpression): Variant;
    function IsTruthy(obj: Variant): boolean;
    procedure ExecuteBlock(stmts: TObjectList<TStatement>; env: TEnvironment);

    { Binary function helpers for Numeric operations }
    function AddNumbers(Left, Right: Variant): Variant;
    function SubNumbers(Left, Right: Variant): Variant;
    function MulNumbers(Left, Right: Variant): Variant;
    function DivNumbers(Left, Right: Variant): Variant;
    function LessNumbers(Left, Right: Variant): Variant;
    function LessEqNumbers(Left, Right: Variant): Variant;
    function GreaterNumbers(Left, Right: Variant): Variant;
    function GreaterEqNumbers(Left, Right: Variant): Variant;
    function EqNumbers(Left, Right: Variant): Variant;
    function NotEqNumbers(Left, Right: Variant): Variant;

    { Binary function helpers for Boolean operations }
    function EqBooleans(Left, Right: Variant): Variant;
    function NotEqBooleans(Left, Right: Variant): Variant;

    { Binary function helpers for String operatins }
    function AddStrings(Left, Right: Variant): Variant;
    function EqStrings(Left, Right: Variant): Variant;
    function NotEqStrings(Left, Right: Variant): Variant;

    procedure RegisterBinaryHelperFunctions;
    procedure CreateGlobalEnvironment;
    procedure VariableDeclaration(AVarStruct: TVariableStruct; Env: TEnvironment);
  public
    constructor Create;
    destructor Destroy; override;

    { Expression Visitor }
    function VisitLiteralExpr(expr: TLiteralExpression): Variant; override;
    function VisitIdentifierExpr(expr: TIdentifier): Variant; override;
    function VisitLogicalExpr(expr: TLogicalExpression): Variant; override;
    function VisitBinaryExpr(expr: TBinaryExpression): Variant; override;
    function VisitAssignmentExpr(expr: TAssignmentExpression): Variant; override;
    function VisitCallExpr(expr: TCallExpression): Variant; override;

    { Statement Visitor }
    procedure VisitExpressionStmt(stmt: TExpressionStatement); override;
    procedure VisitPrintStmt(stmt: TPrintStatement); override;
    procedure VisitVariableStmt(stmt: TVariableStatement); override;
    procedure VisitConstantStmt(stmt: TConstantStatement); override;
    procedure VisitReturnStmt(stmt: TReturnStatement); override;
    procedure VisitBlockStmt(stmt: TBlockStatement); override;
    procedure VisitIfStmt(stmt: TIfStatement); override;
    procedure VisitFunctionStmt(stmt: TFunctionStatement); override;

    procedure Execute(stmt: TStatement);
    procedure Interpret(stmts: TObjectList<TStatement>);
  end;

implementation
  (************************************************************************
  * Create
  ************************************************************************)
  constructor TInterpreter.Create;
  begin
    RegisterBinaryHelperFunctions();
    CreateGlobalEnvironment();
  end;
  (************************************************************************
  * Destroy
  ************************************************************************)
  destructor TInterpreter.Destroy;
  begin
    FBinaryDisp.Free;
    FCurrentEnv.Free;
    inherited Destroy;
  end;
  (************************************************************************
  * RegisterBinaryHelperFunctions
  ************************************************************************)
  procedure TInterpreter.RegisterBinaryHelperFunctions;
  var
    i: extended = 2;
    b: boolean = False;
    s: string = '';
    Key: extended;
  begin
    FBinaryDisp := TDictionary<extended, TBinaryFunc>.Create();

    { Binary function helpers for Numeric operations }
    Key := (Vartype(i)*i)+Ord(TKind.tkPlus);
    FBinaryDisp.Add(Key, AddNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkMinus);
    FBinaryDisp.Add(Key, SubNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkMult);
    FBinaryDisp.Add(Key, MulNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkDiv);
    FBinaryDisp.Add(Key, DivNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkLess);
    FBinaryDisp.Add(Key, LessNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkLessEq);
    FBinaryDisp.Add(Key, LessEqNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkGreater);
    FBinaryDisp.Add(Key, GreaterNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkGreaterEq);
    FBinaryDisp.Add(Key, GreaterEqNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkEqual);
    FBinaryDisp.Add(Key, EqNumbers);

    Key := (Vartype(i)*i)+Ord(TKind.tkNotEqual);
    FBinaryDisp.Add(Key, NotEqNumbers);

    { Binary function helpers for Boolean operations }
    Key := (Vartype(b)*i)+Ord(TKind.tkEqual);
    FBinaryDisp.Add(Key, EqBooleans);

    Key := (Vartype(b)*i)+Ord(TKind.tkNotEqual);
    FBinaryDisp.Add(Key, NotEqBooleans);

    { Binary function helpers for String operatins }
    Key := (Vartype(s)*i)+Ord(TKind.tkPlus);
    FBinaryDisp.Add(Key, AddStrings);

    Key := (Vartype(s)*i)+Ord(TKind.tkEqual);
    FBinaryDisp.Add(Key, EqStrings);

    Key := (Vartype(s)*i)+Ord(TKind.tkNotEqual);
    FBinaryDisp.Add(Key, NotEqStrings);

  end;
  (************************************************************************
  * CreateGlobalEnvironment
  ************************************************************************)
  procedure TInterpreter.CreateGlobalEnvironment;
  var
    GlobalRecord: TRecord;
    Tuple: TVarTuple;
  begin
    GlobalRecord := TRecord.Create();
    Tuple.FVarMode := vmConstant;
    Tuple.FValue := '1.0';

    GlobalRecord.Add('version', Tuple);
    Tuple.FVarMode := vmConstant;
    Tuple.FValue := 'Irwin Rodriguez <rodriguez.irwin@gmail.com>';

    GlobalRecord.Add('author', Tuple);

    FGlobalEnv := TEnvironment.Create(GlobalRecord, Nil);

    FCurrentEnv := FGlobalEnv; // Comenzando con el environment Global.
  end;
  (************************************************************************
  * IsTruthy
  ************************************************************************)
  function TInterpreter.IsTruthy(obj: Variant): boolean;
  begin
    Result := VarIsBool(obj) and (obj = True);
  end;
  (************************************************************************
  * AddNumbers
  ************************************************************************)
  function TInterpreter.AddNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left + Right;
  end;
  (************************************************************************
  * SubNumbers
  ************************************************************************)
  function TInterpreter.SubNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left - Right;
  end;
  (************************************************************************
  * MulNumbers
  ************************************************************************)
  function TInterpreter.MulNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left * Right;
  end;
  (************************************************************************
  * DivNumbers
  ************************************************************************)
  function TInterpreter.DivNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left / Right;
  end;
  (************************************************************************
  * LessNumbers
  ************************************************************************)
  function TInterpreter.LessNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left < Right;
  end;
  (************************************************************************
  * LessEqNumbers
  ************************************************************************)
  function TInterpreter.LessEqNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left <= Right;
  end;
  (************************************************************************
  * GreaterNumbers
  ************************************************************************)
  function TInterpreter.GreaterNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left > Right;
  end;
  (************************************************************************
  * GreaterEqNumbers
  ************************************************************************)
  function TInterpreter.GreaterEqNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left >= Right;
  end;
  (************************************************************************
  * EqNumbers
  ************************************************************************)
  function TInterpreter.EqNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left = Right;
  end;
  (************************************************************************
  * NotEqNumbers
  ************************************************************************)
  function TInterpreter.NotEqNumbers(Left, Right: Variant): Variant;
  begin
    Result := Left <> Right;
  end;
  (************************************************************************
  * EqBooleans
  ************************************************************************)
  function TInterpreter.EqBooleans(Left, Right: Variant): Variant;
  begin
    Result := Left = Right;
  end;
  (************************************************************************
  * NotEqBooleans
  ************************************************************************)
  function TInterpreter.NotEqBooleans(Left, Right: Variant): Variant;
  begin
    Result := Left <> Right;
  end;
  (************************************************************************
  * AddStrings
  ************************************************************************)
  function TInterpreter.AddStrings(Left, Right: Variant): Variant;
  begin
    Result := Left + Right;
  end;
  (************************************************************************
  * EqStrings
  ************************************************************************)
  function TInterpreter.EqStrings(Left, Right: Variant): Variant;
  begin
    Result := Left = Right;
  end;
  (************************************************************************
  * NotEqStrings
  ************************************************************************)
  function TInterpreter.NotEqStrings(Left, Right: Variant): Variant;
  begin
    Result := Left <> Right;
  end;
  (************************************************************************
  * VisitLiteralExpr
  ************************************************************************)
  function TInterpreter.VisitLiteralExpr(expr: TLiteralExpression): Variant;
  begin
    Result := expr.value;
  end;
  (************************************************************************
  * VisitidentifierExpr
  ************************************************************************)
  function TInterpreter.VisitIdentifierExpr(expr: TIdentifier): Variant;
  begin
    Result := FCurrentEnv.Lookup(expr.Token);
  end;
  (************************************************************************
  * VisitLogicalExpr
  ************************************************************************)
  function TInterpreter.VisitLogicalExpr(expr: TLogicalExpression): Variant;
  var
    Left: Variant;
  begin
    Left := Evaluate(expr.Left);
    case expr.Ope of
    tkOr:
      if isTruthy(Left) then
        Exit(True);
    tkAnd:
      if not isTruthy(Left) then
        Exit(False);
    end;

    Exit(Evaluate(expr.Right));
  end;
  (************************************************************************
  * VisitBinaryExpr
  ************************************************************************)
  function TInterpreter.VisitBinaryExpr(expr: TBinaryExpression): Variant;
  var
    Left, Right: Variant;
    BinaryFunc: TBinaryFunc;
    Key: integer;
  begin
    Left := Evaluate(expr.Left);
    Right := Evaluate(expr.Right);
    Key := VarType(Left)+VarType(Right)+Ord(expr.Ope);
    if not FBinaryDisp.TryGetValue(Key, BinaryFunc) then
      raise ERuntimeError.Create(expr.Token, 'Incompatible data types');

    Result := BinaryFunc(Left, Right);

  end;
  (************************************************************************
  * VisitAssignmentExpr
  ************************************************************************)
  function TInterpreter.VisitAssignmentExpr(expr: TAssignmentExpression): Variant;
  begin
    Result := Evaluate(expr.Value);
    FCurrentEnv.Assign(expr.Left.Token, Result);
  end;
  (************************************************************************
  * VisitCallExpr
  ************************************************************************)
  function TInterpreter.VisitCallExpr(expr: TCallExpression): Variant;
  var
    FuncPackage: TFunctionPackage = Nil;
    Args: array of Variant;
    i: integer = 0;
    arg: TExpression;
    NewEnv: TEnvironment;
  begin
    FuncPackage := FCurrentEnv.GetFunction(expr.Callee.Token);

    // Evaluate Arguments
    SetLength(Args, expr.Arguments.Count-1);
    for arg in expr.Arguments do
    begin
      Args[i] := Evaluate(arg);
      Inc(i);
    end;

    // Zip arguments with function parameters
    NewEnv := TEnvironment.Create(Nil, FuncPackage.Env);

    for i := 0 to Length(Args) do
    begin
      NewEnv.Define(FuncPackage.FuncAST.Parameters[i].Token, Args[i], vmVariable);
    end;
    // Execute the function
    try
      ExecuteBlock(FuncPackage.FuncAST.Body.Stmts, NewEnv);
    except
      on E: EReturnException do
        Result := E.Value;
    end;
  end;
  (************************************************************************
  * VisitReturnStmt
  ************************************************************************)
  procedure TInterpreter.VisitReturnStmt(stmt: TReturnStatement);
  var
    ReturnedValue: Variant;
  begin
    ReturnedValue := null;
    if stmt.Value <> nil then
      ReturnedValue := Evaluate(stmt.Value);

    raise EReturnException.Create(ReturnedValue);
  end;

  (************************************************************************
  * VisitReturnStmt
  ************************************************************************)
  procedure TInterpreter.VisitBlockStmt(stmt: TBlockStatement);
  begin
    ExecuteBlock(stmt.Stmts, TEnvironment.Create(nil, FCurrentEnv));
  end;

  (************************************************************************
  * ExecuteBlock
  ************************************************************************)
  procedure TInterpreter.ExecuteBlock(stmts: TObjectList<TStatement>; env: TEnvironment);
  var
    PreviousEnv: TEnvironment;
    stmt: TStatement;
  begin
    PreviousEnv := FCurrentEnv;
    try
      FCurrentEnv := env; // switch curent environment into block environment.
      for stmt in stmts do
        Execute(stmt);
    finally
      FCurrentEnv := PreviousEnv;
    end;
  end;
  (************************************************************************
  * VisitIfStmt
  ************************************************************************)
  procedure TInterpreter.VisitIfStmt(stmt: TIfStatement);
  var
    Condition: Variant;
  begin
    Condition := Evaluate(stmt.Condition);

    if not VarIsBool(Condition) then
      raise ERuntimeError.Create(stmt.Token, 'If condition must be a boolean expression');

    if Condition then
      Execute(stmt.Consequence)
    else
      if Assigned(stmt.Alternative) then
        Execute(stmt.Alternative);

  end;

  (************************************************************************
  * VisitExpressionStmt
  ************************************************************************)
  procedure TInterpreter.VisitExpressionStmt(stmt: TExpressionStatement);
  begin
    Evaluate(stmt.expr);
  end;
  (************************************************************************
  * VisitPrintStmt
  ************************************************************************)
  procedure TInterpreter.VisitPrintStmt(stmt: TPrintStatement);
  var
    exp: TExpression;
    res: Variant;
  begin
    for exp in stmt.Values do
    begin
      res := Evaluate(exp);
      if VarIsNull(res) then
        ShowMessage('Null')
      else
        ShowMessage(VarToStr(res));
    end;
  end;
  (************************************************************************
  * VisitVariableStmt
  ************************************************************************)
  procedure TInterpreter.VisitVariableStmt(stmt: TVariableStatement);
  var
    CurEnv: TEnvironment;
    VarStruct: TVariableStruct;
  begin
    CurEnv := FCurrentEnv;
    if stmt.Scope = vsPublic then
      CurEnv := FGlobalEnv;

    for VarStruct in stmt.VarList do
      VariableDeclaration(VarStruct, CurEnv);

  end;
  (************************************************************************
  * VariableDeclaration
  ************************************************************************)
  procedure TInterpreter.VariableDeclaration(AVarStruct: TVariableStruct; Env: TEnvironment);
  var
    Value: Variant;
    HasInitilizer: Boolean = False;
  begin
    Value := null;
    if AVarStruct.Value <> Nil then
    begin
      HasInitilizer := True;
      Value := Evaluate(AVarStruct.Value)
    end;

    if AVarStruct.VarType <> vtOther then
    begin
      case AVarStruct.VarType of
      vtString:
        begin
          if HasInitilizer and (not VarIsStr(Value)) then
            raise ERuntimeError.Create(AVarStruct.Name.Token, 'Invalid value for string type')
          else if not HasInitilizer then
            Value := '';
        end;
      vtNumber:
        begin
          if HasInitilizer and (not VarIsFloat(Value)) then
            raise ERuntimeError.Create(AVarStruct.Name.Token, 'Invalid value for number type')
          else if not HasInitilizer then
            Value := 0;
        end;
      vtBoolean:
        begin
          if HasInitilizer and (not VarIsBool(Value)) then
            raise ERuntimeError.Create(AVarStruct.Name.Token, 'Invalid value for boolean type')
          else if not HasInitilizer then
            Value := False;
        end;
      end;
    end;

    // Register the variable (case insensitive)
    Env.Define(AVarStruct.Name.Token, Value, vmVariable);
  end;
  (************************************************************************
  * VisitConstantStmt
  ************************************************************************)
  procedure TInterpreter.VisitConstantStmt(stmt: TConstantStatement);
  var
    Assignment: TAssignmentExpression;
    Value: Variant;
  begin
    for Assignment in stmt.ConstantList do
    begin
      Value := Evaluate(Assignment.Value);
      FCurrentEnv.Define(Assignment.Left.Token, Value, vmConstant);
    end;
  end;
  (************************************************************************
  * VisitFunctionStmt
  ************************************************************************)
  procedure TInterpreter.VisitFunctionStmt(stmt: TFunctionStatement);
  var
    functionId: integer;
  begin
    functionId := FCurrentEnv.AddFunction(stmt);
    FCurrentEnv.Define(stmt.Name.Token, functionId, vmConstant);
  end;
  (************************************************************************
  * Execute
  ************************************************************************)
  procedure TInterpreter.Execute(stmt: TStatement);
  begin
    try
       stmt.Accept(Self);
    except
      on E: ERuntimeError do
        RuntimeError(E);
    end;
  end;
  (************************************************************************
  * Evaluate
  ************************************************************************)
  function TInterpreter.Evaluate(expr: TExpression): Variant;
  begin
    Result := expr.Accept(Self);
  end;
  (************************************************************************
  * Interpret
  ************************************************************************)
  procedure TInterpreter.Interpret(stmts: TObjectList<TStatement>);
  var
    stmt: TStatement;
  begin
    try
      for stmt in stmts do
      begin
        Execute(stmt);
      end;
    except
      on E: EReturnException do
        E.Value := null; // Value is discarded.
    end;
  end;

initialization

finalization

end.
