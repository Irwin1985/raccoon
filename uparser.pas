unit uParser;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Variants,
  uToken,
  uAst,
  uError;

type
  { TParser }
  TParser = class(TObject)
  private
    FTokens: TObjectList<TToken>;
    FCurrent: integer;
    function Advance: TToken;
    function Peek: TToken;
    function Previous: TToken;
    function Check(tk: TKind): boolean;
    function Consume(tk: TKind; msg: string): TToken;
    function Match(atk: array of Tkind): boolean;
    function IsAtEnd: boolean;
    procedure Error(Token: TToken; Msg: string);
    procedure Synchronize;
    function ParseExpressionList: TExpressionList;
    function IsLiteral: boolean;
    function IsAssignmentOperator: boolean;
    function CheckValidAssignmentTarget(ALeft: TExpression): TExpression;
    function VariableDeclarationList: TObjectList<TVariableStruct>;
    function VariableDeclaration: TVariableStruct;
    function FunctionParameters: TObjectList<TIdentifier>;

    function ParseProgram: TObjectList<TStatement>;
    function StatementList(stopLookAhead: TKind): TObjectList<TStatement>;
    function Statement: TStatement;
    function PrintStatement: TStatement;
    function AskStatement: TStatement;
    function VariableStatement: TStatement;
    function ConstantStatement: TStatement;
    function ReturnStatement: TStatement;
    function BlockStatement(stopLookAhead: array of Tkind; skipNewLine: boolean): TBlockStatement;
    function IfStatement: TStatement;
    function FunctionStatement: TStatement;
    function ExpressionStatement: TStatement;

    function Expression: TExpression;
    function Assignment: TExpression;
    function LogicalOr: TExpression;
    function LogicalAnd: TExpression;
    function Equality: TExpression;
    function Comparison: TExpression;
    function Term: TExpression;
    function Factor: TExpression;
    function Call: TExpression;
    function Primary: TExpression;
    function Literal: TExpression;
    function Identifier: TExpression;
    function GroupedExpression: TExpression;

  public
    constructor Create(tokens: TObjectList<TToken>);
    destructor Destroy; override;
    function Parse: TObjectList<TStatement>;
  end;

implementation
  (************************************************************************
  * Advance
  ************************************************************************)
  function TParser.Advance: TToken;
  begin
    Result := FTokens[FCurrent];
    if not IsAtEnd() then
       Inc(FCurrent);
  end;
  (************************************************************************
  * Peek
  ************************************************************************)
  function TParser.Peek: TToken;
  begin
    Result := FTokens[FCurrent];
  end;
  (************************************************************************
  * Previous
  ************************************************************************)
  function TParser.Previous: TToken;
  begin
    Result := FTokens[FCurrent-1];
  end;
  (************************************************************************
  * Check
  ************************************************************************)
  function TParser.Check(tk: TKind): boolean;
  begin
    if not IsAtEnd() then
       Exit(Peek().Kind = tk)
    else
      Exit(False);
  end;
  (************************************************************************
  * Consume
  ************************************************************************)
  function TParser.Consume(tk: TKind; msg: string): TToken;
  begin
    if Check(tk) then
       Exit(Advance());

    Error(Peek(), msg);
  end;
  (************************************************************************
  * Match
  ************************************************************************)
  function TParser.Match(atk: array of Tkind): boolean;
  var
    tk: TKind;
  begin
    for tk in atk do
    begin
      if Check(tk) then
      begin
        Advance();
        Exit(True);
      end;
    end;
    Exit(False);
  end;
  (************************************************************************
  * IsAtEnd
  ************************************************************************)
  function TParser.IsAtEnd: boolean;
  begin
    Result := Peek().Kind = TKind.tkEof;
  end;
  (************************************************************************
  * Error
  ************************************************************************)
  procedure TParser.Error(Token: TToken; Msg: string);
  begin
    Errors.Append(Token, Msg);
    raise EParseError.Create(Msg);
  end;
  (************************************************************************
  * Synchronize
  ************************************************************************)
  procedure TParser.Synchronize;
  begin
    Advance();

    while not IsAtEnd() do
    begin
      if Previous().Kind = TKind.tkNewLine then
        Exit();
      case Peek().Kind of
        TKind.tkClass, TKind.tkFunction, TKind.tkLocal,
        TKind.tkPublic, TKind.tkFor, TKind.tkIf, TKind.tkWhile,
        TKind.tkReturn:
          Exit();
      end;
      Advance();
    end;
  end;
  (************************************************************************
  * IsLiteral
  ************************************************************************)
  function TParser.IsLiteral: boolean;
  var
    tok: TToken;
  begin
    tok := Peek();
    Result := tok.Kind in [tkNumber, tkString, tkTrue, tkFalse, tkNull];
  end;
  (************************************************************************
  * IsAssignmentOperator
  ************************************************************************)
  function TParser.IsAssignmentOperator: boolean;
  begin
    Result := Match([
              tkAssign,
              tkPlusAssign,
              tkMinusAssign,
              tkMultAssign,
              tkDivAssign
              ]);
  end;
  (************************************************************************
  * CheckValidAssignment
  ************************************************************************)
  function TParser.CheckValidAssignmentTarget(ALeft: TExpression): TExpression;
  begin
    if ALeft.ClassNameIs('TIdentifier') then
      Exit(Aleft);
    raise ERunTimeError.Create(ALeft.Token, 'Invalid left-hand side in assignment expression.');
  end;
  (************************************************************************
  * Identifier
  ************************************************************************)
  function TParser.Identifier: TExpression;
  var
    tok: TToken;
  begin
    tok := Previous();
    Result := TIdentifier.Create(tok, VarToStr(tok.Literal));
  end;
  (************************************************************************
  * GroupedExpression
  ************************************************************************)
  function TParser.GroupedExpression: TExpression;
  begin
    Result := Expression();
    Consume(tkRightParen, 'Missing parenthesis ")" after expression.')
  end;
  (************************************************************************
  * ExpressionList
  ************************************************************************)
  function TParser.ParseExpressionList: TExpressionList;
  begin
    Result := TExpressionList.Create();
    repeat
      Result.Add(Expression());
    until not Match(tkComma);
  end;
  (************************************************************************
  * Program
  ************************************************************************)
  function TParser.ParseProgram: TObjectList<TStatement>;
  begin
    Result := StatementList(TKind.tkEof);
  end;
  (************************************************************************
  * StatementList
  ************************************************************************)
  function TParser.StatementList(stopLookAhead: TKind): TObjectList<TStatement>;
  begin
    Result := TObjectList<TStatement>.Create(True); // owns objects
    while Peek().Kind <> stopLookAhead do
    begin
      Result.Add(Statement());
    end;
  end;
  (************************************************************************
  * Statement
  ************************************************************************)
  function TParser.Statement: TStatement;
  begin
    if Match(tkPrint) then
      Exit(PrintStatement());

    if Match(tkAsk) then
      Exit(AskStatement());

    if Match([tkLocal, tkPublic]) then
      Exit(VariableStatement());

    if Match(tkConst) then
      Exit(ConstantStatement());

    if Match(tkReturn) then
      Exit(ReturnStatement());

    if Match(tkIf) then
      Exit(IfStatement());

    if Match(tkFunction) then
      Exit(FunctionStatement());

    Result := ExpressionStatement();
  end;
  (************************************************************************
  * ExpressionStatement
  ************************************************************************)
  function TParser.ExpressionStatement: TStatement;
  var
    expr: TExpression = Nil;
    tok: TToken;
  begin
    tok := Peek();
    expr := Expression();
    Consume(TKind.tkNewLine, 'Expected newline after expression');
    Result := TExpressionStatement.Create(tok, expr);
  end;
  (************************************************************************
  * PrintStatement
  ************************************************************************)
  function TParser.PrintStatement: TStatement;
  var
    ExpressionList: TExpressionList = Nil;
    tok: TToken;
  begin
    tok := Previous();
    Consume(TKind.tkLeftParen, 'Expected ''('' after print statement');
    ExpressionList := TExpressionList.Create(False);
    if not Check(TKind.tkRightParen) then
    begin
      repeat
        ExpressionList.Add(Expression());
      until not Match(tkComma);
    end;
    Consume(TKind.tkRightParen, 'Expected '')'' after expression');
    Consume(TKind.tkNewLine, 'Expected newline at the end of PrintStatement');
    Result := TPrintStatement.Create(tok, ExpressionList);
  end;

  (************************************************************************
  * AskStatement
  ************************************************************************)
  function TParser.AskStatement: TStatement;
  var
    ExpressionList: TExpressionList = Nil;
    tok: TToken;
  begin
    tok := Previous();
    ExpressionList := TExpressionList.Create(False);
    repeat
      ExpressionList.Add(Expression());
    until not Match(tkComma);
    Consume(TKind.tkNewLine, 'Expected newline at the end of PrintStatement');
    Result := TPrintStatement.Create(tok, ExpressionList);
  end;
  (************************************************************************
  * VariableStatement
  ************************************************************************)
  function TParser.VariableStatement: TStatement;
  var
    Token: TToken;
    VariableScope: TVariableScope;
    VarList: TObjectList<TVariableStruct> = Nil;
  begin
    Token := Previous();

    VariableScope := vsLocal;
    if Token.Kind = tkPublic then
      VariableScope := vsPublic;

    VarList := VariableDeclarationList();

    Consume(TKind.tkNewLine, 'Expected newline');

    Result := TVariableStatement.Create(Token, VariableScope, VarList);
  end;
  (************************************************************************
  * VariableDeclarationList
  ************************************************************************)
  function TParser.VariableDeclarationList: TObjectList<TVariableStruct>;
  begin
    Result := TObjectList<TVariableStruct>.Create(False);
    repeat
      Result.Add(VariableDeclaration());
    until not Match(tkComma);
  end;
  (************************************************************************
  * VariableDeclaration
  ************************************************************************)
  function TParser.VariableDeclaration: TVariableStruct;
  var
    TokIdent: TToken;
    Ident: TIdentifier;
    StrType: string;
    VariableType: TVariableType;
    Value: TExpression = Nil;
  begin

    // Variable Name
    TokIdent := Consume(tkIdentifier, 'Expected variable name.');
    Ident := TIdentifier.Create(TokIdent, VarToStr(TokIdent.Literal));

    // Variable type
    VariableType := vtOther; // Default type is vtOther => Null
    if Match(tkAs) then
    begin
      StrType := LowerCase(Consume(tkIdentifier, 'Variable type expected.').Literal);
      if StrType = 'string' then
        VariableType := vtString
      else if StrType = 'number' then
        VariableType := vtNumber
      else if StrType = 'boolean' then
        VariableType := vtBoolean;
    end;
    // Check for Declaration + Assignment
    if Match(tkAssign) then
      Value := Expression();

    Result := TVariableStruct.Create(Ident, VariableType, Value);
  end;

  (************************************************************************
  * ConstantStatement
  ************************************************************************)
  function TParser.ConstantStatement: TStatement;
  var
    ConstantList: TObjectList<TAssignmentExpression> = Nil;
  begin
    ConstantList := TObjectList<TAssignmentExpression>.Create(False);

    repeat
      ConstantList.Add(Assignment() as TAssignmentExpression);
    until not Match(tkComma);

    Consume(TKind.tkNewLine, 'Expected newline');

    Result := TConstantStatement.Create(ConstantList);
  end;
  (************************************************************************
  * ReturnStatement
  ************************************************************************)
  function TParser.ReturnStatement: TStatement;
  begin
    Result := TReturnStatement.Create(Previous(), Expression());
    Consume(TKind.tkNewLine, 'Expected newline');
  end;
  (************************************************************************
  * BlockStatement
  ************************************************************************)
  function TParser.BlockStatement(stopLookAhead: array of Tkind; skipNewLine: boolean): TBlockStatement;
  var
    Tok: TToken;
    Stmts: TObjectList<TStatement> = Nil;
  begin
    Tok := Consume(TKind.tkNewLine, 'Expected newline');
    Stmts := TObjectList<TStatement>.Create(False);

    while (not IsAtEnd()) and (not Match(stopLookAhead)) do
      Stmts.Add(Statement());

    if skipNewLine then
       Consume(TKind.tkNewLine, 'Expected newline');

    Result := TBlockStatement.Create(Tok, Stmts);
  end;
  (************************************************************************
  * IfStatement
  ************************************************************************)
  function TParser.IfStatement: TStatement;
  var
    Tok: TToken;
    Condition: TExpression = Nil;
    Consequence: TBlockStatement = Nil;
    Alternative: TBlockStatement = Nil;
  begin
    Tok := Previous();

    if Match(tkLeftParen) then
    begin
      Condition := Expression();
      Consume(tkRightParen, 'Missing ")" after condition.');
    end
    else
      Condition := Expression();

    Match(tkThen); // Optionally check for THEN keyword

    Consequence := BlockStatement([tkElse, tkEndif], False);

    if Previous().Kind = tkElse then
      Alternative := BlockStatement(tkEndif, True)
    else
      Consume(TKind.tkNewLine, 'Expected newline');

    Result := TIfStatement.Create(Tok, Condition, Consequence, Alternative);
  end;
  (************************************************************************
  * FunctionParameters
  ************************************************************************)
  function TParser.FunctionParameters: TObjectList<TIdentifier>;
  begin
    Result := TObjectList<TIdentifier>.Create(False);
    if not Check(tkRightParen) then
      repeat
        Result.Add(TIdentifier.Create(Consume(tkIdentifier, 'Expected indentifier.'), VarToStr(Previous().Literal)));
      until not Match(tkComma);
    Consume(tkRightParen, 'Missing parenthesis ")" in function declaration.');
  end;
  (************************************************************************
  * FunctionStatement
  ************************************************************************)
  function TParser.FunctionStatement: TStatement;
  var
    Tok: TToken;
    Name: TIdentifier = Nil;
    Parameters: TObjectList<TIdentifier> = Nil;
    Body: TBlockStatement = Nil;
  begin
    Tok := Previous();
    Name := TIdentifier.Create(Consume(tkIdentifier, 'Expected function name.'), VarToStr(Previous().Literal));

    if Match(tkLeftParen) then
      Parameters := FunctionParameters();

    Body := BlockStatement(tkEndFunc, True);

    // Env: will be assigned in the interpreter.
    Result := TFunctionStatement.Create(Tok, Name, Parameters, Body);
  end;
  (************************************************************************
  * Expression
  ************************************************************************)
  function TParser.Expression: TExpression;
  begin
    Result := Assignment();
  end;
  (************************************************************************
  * Assignment
  ************************************************************************)
  function TParser.Assignment: TExpression;
  var
    Tok: TToken;
    Ope: TKind;
    Left: TExpression = Nil;
    Value: TExpression = Nil;
  begin
    Left := LogicalOr();
    if not IsAssignmentOperator() then
      Exit(Left);
    // It's an assignment
    Tok := Left.Token;
    Ope := Previous().Kind;
    Value := Assignment();
    Left := CheckValidAssignmentTarget(Left);
    Result := TAssignmentExpression.Create(Tok, Ope, Left, Value);
  end;
  (************************************************************************
  * LogicalOr
  ************************************************************************)
  function TParser.LogicalOr: TExpression;
  var
    Left: TExpression = Nil;
    Right: TExpression = Nil;
    Tok: TToken;
  begin
    Left := LogicalAnd();
    while Match(tkOr) do
    begin
      Tok := Previous();
      Right := LogicalAnd();
      Left := TLogicalExpression.Create(Tok, Left, Tok.Kind, Right);
    end;

    Result := Left;
  end;
  (************************************************************************
  * LogicalAnd
  ************************************************************************)
  function TParser.LogicalAnd: TExpression;
  var
    Left: TExpression = Nil;
    Right: TExpression = Nil;
    Tok: TToken;
  begin
    Left := Equality();

    while Match(tkAnd) do
    begin
      Tok := Previous();
      Right := Equality();
      Left := TLogicalExpression.Create(Tok, Left, Tok.Kind, Right);
    end;

    Result := Left;
  end;
  (************************************************************************
  * Equality
  ************************************************************************)
  function TParser.Equality: TExpression;
  var
    Left: TExpression = Nil;
    Right: TExpression = Nil;
    Tok: TToken;
  begin
    Left := Comparison();
    while Match([tkEqual, tkNotEqual]) do
    begin
      Tok := Previous();
      Right := Comparison();
      Left := TBinaryExpression.Create(Tok, Left, Tok.Kind, Right);
    end;

    Result := Left;
  end;
  (************************************************************************
  * Comparison
  ************************************************************************)
  function TParser.Comparison: TExpression;
  var
    Left: TExpression = Nil;
    Right: TExpression = Nil;
    Tok: TToken;
  begin
    Left := Term();

    while Match([tkLess, tkLessEq, tkGreater, tkGreaterEq]) do
    begin
      Tok := Previous();
      Right := Term();
      Left := TBinaryExpression.Create(Tok, Left, Tok.Kind, Right);
    end;

    Result := Left;
  end;
  (************************************************************************
  * Term
  ************************************************************************)
  function TParser.Term: TExpression;
  var
    Left: TExpression = Nil;
    Right: TExpression = Nil;
    Tok: TToken;
  begin
    Left := Factor();
    while Match([tkPlus, tkMinus]) do
    begin
      Tok := Previous();
      Right := Factor();
      Left := TBinaryExpression.Create(Tok, Left, Tok.Kind, Right);
    end;

    Result := Left;
  end;
  (************************************************************************
  * Factor
  ************************************************************************)
  function TParser.Factor: TExpression;
  var
    Left: TExpression = Nil;
    Right: TExpression = Nil;
    Tok: TToken;
  begin
    Left := Call();
    while Match([tkMult, tkDiv]) do
    begin
      Tok := Previous();
      Right := Call();
      Left := TBinaryExpression.Create(Tok, Left, Tok.Kind, Right);
    end;

    Result := Left;
  end;
  (************************************************************************
  * Call
  ************************************************************************)
  function TParser.Call: TExpression;
  var
    Left: TExpression = Nil;
    Args: TObjectList<TExpression> = Nil;
  begin
    Left := Primary();
    if not Match(tkLeftParen) then
      Exit(Left);

    // It's a function call
    if not Match(tkRightParen) then
       Args := ParseExpressionList();
    Consume(tkRightParen, 'Missing ")" after function call');

    Result := TCallExpression.Create(Left.Token, Left, Args);
  end;
  (************************************************************************
  * Primary
  ************************************************************************)
  function TParser.Primary: TExpression;
  begin
    if IsLiteral() then
      Exit(Literal());
    if Match(tkIdentifier) then
      Exit(Identifier());
    if Match(tkLeftParen) then
      Exit(GroupedExpression());

    Error(Peek(), 'Unexpected primary expression.');

  end;
  (************************************************************************
  * Literal
  ************************************************************************)
  function TParser.Literal: TExpression;
  var
    tok: TToken;
  begin
    tok := Advance();
    Result := TLiteralExpression.Create(tok, tok.Literal);
  end;
  {----------------------------PUBLIC METHODS----------------------------------}
  (************************************************************************
  * Create
  ************************************************************************)
  constructor TParser.Create(tokens: TObjectList<TToken>);
  begin
    FTokens := tokens;
    FCurrent := 0;
  end;
  (************************************************************************
  * Destroy
  ************************************************************************)
  destructor TParser.Destroy;
  begin
    inherited;
  end;
  (************************************************************************
  * Parse
  ************************************************************************)
  function TParser.Parse: TObjectList<TStatement>;
  begin
    Result := ParseProgram();
  end;

end.

