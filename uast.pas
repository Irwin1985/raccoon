unit uAst;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Generics.Collections, Dialogs, uToken;

type

  { Visitor }
  TVisitor = class;

  { All nodes base class }
  TNode = class(TObject)
  private
    FToken: TToken;
  public
    constructor Create(AToken: TToken);
    property Token: TToken read FToken;
  end;

  { All expressions based nodes }
  TExpression = class(TNode)
  public
    // Base node for all expressions
    function Accept(v: TVisitor): Variant; virtual; abstract;
  end;

  { TExpressionList }
  TExpressionList = TObjectList<TExpression>;

  { TLiteralExpression }
  TLiteralExpression = class(TExpression)
  private
    FValue: Variant;
  public
    constructor Create(AToken: TToken; AValue: Variant);
    function Accept(v: TVisitor): Variant; override;
    property Value: Variant read FValue;
  end;

  { TLogicalExpression }
  TLogicalExpression = class(TExpression)
  private
    FLeft: TExpression;
    FOpe: TKind;
    FRight: TExpression;
  public
    constructor Create(AToken: TToken; ALeft: TExpression; AOpe: TKind; ARight: TExpression);
    destructor Destroy; override;
    function Accept(v: TVisitor): Variant; override;
    property Left: TExpression read FLeft;
    property Ope: TKind read FOpe;
    property Right: TExpression read FRight;
  end;

  { TBinaryExpression }
  TBinaryExpression = class(TExpression)
  private
    FLeft, FRight: TExpression;
    FOpe: TKind;
  public
    constructor Create(AToken: TToken; ALeft: TExpression; AOpe: TKind; ARight: TExpression);
    destructor Destroy; override;
    function Accept(v: TVisitor): Variant; override;
    property Left: TExpression read FLeft;
    property Right: TExpression read FRight;
    property Ope: TKind read FOpe;
  end;

  { TIdentifier }
  TIdentifier = class(TExpression)
  private
    FName: string;
  public
    constructor Create(AToken: TToken; AName: string);
    destructor Destroy; override;
    function Accept(v: TVisitor): Variant; override;
    property Name: string read FName;
  end;

  { TAssignmentExpression }
  TAssignmentExpression = class(TExpression)
  private
    FOpe: TKind;
    FLeft: TExpression;
    FValue: TExpression;
  public
    constructor Create(AToken: TToken; AOpe: TKind; ALeft, AValue: TExpression);
    destructor Destroy; override;
    function Accept(v: TVisitor): Variant; override;
    property Ope: TKind read FOpe;
    property Left: TExpression read FLeft;
    property Value: TExpression read FValue;
  end;

  { TCallExpression }
  TCallExpression = class(TExpression)
  private
    FCallee: TExpression;
    FArguments: TObjectList<TExpression>;
  public
    constructor Create(AToken: TToken; ACallee: TExpression; AArguments: TObjectList<TExpression>);
    destructor Destroy; override;
    function Accept(v: TVisitor): Variant; override;
    property Callee: TExpression read FCallee;
    property Arguments: TObjectList<TExpression> read FArguments;
  end;

  { All statements based nodes }
  TStatement = class(TNode)
  public
    // Base node for all statements
    procedure Accept(v: TVisitor); virtual; abstract;
  end;

  { TExpressionStatement }
  TExpressionStatement = class(TStatement)
  private
    FExpr: TExpression;
  public
    constructor Create(AToken: TToken; AExpr: TExpression);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property Expr: TExpression read FExpr;
  end;

  { TPrintStatement }
  TPrintStatement = class(TStatement)
  private
    FValues: TExpressionList;
  public
    constructor Create(AToken: TToken; AValues: TExpressionList);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property Values: TExpressionList read FValues;
  end;

  { TVariableType }
  TVariableType = (vtString, vtNumber, vtBoolean, vtOther);
  { TVariableScope }
  TVariableScope = (vsLocal, vsPublic);

  { TVariableStruct }
  TVariableStruct = class
  private
    FName: TIdentifier;
    FType: TVariableType;
    FValue: TExpression;
  public
    constructor Create(AName: TIdentifier; AType: TVariableType; AValue: TExpression);
    destructor Destroy; override;
    property Name: TIdentifier read FName;
    property VarType: TVariableType read FType;
    property Value: TExpression read FValue;
  end;

  { TVariableStatement }
  TVariableStatement = class(TStatement)
  private
    FScope: TVariableScope;
    FVarList: TObjectList<TVariableStruct>;
  public
    constructor Create(AToken: TToken; AScope: TVariableScope; AVarList: TObjectList<TVariableStruct>);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property Scope: TVariableScope read FScope;
    property VarList: TObjectList<TVariableStruct> read FVarList;
  end;

  { TConstantStatement }
  TConstantStatement = class(TStatement)
  private
    FConstantList: TObjectList<TAssignmentExpression>;
  public
    constructor Create(AConstantList: TObjectList<TAssignmentExpression>);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property ConstantList: TObjectList<TAssignmentExpression> read FConstantList;
  end;

  { TReturnStatement }
  TReturnStatement = class(TStatement)
  private
    FValue: TExpression;
  public
    constructor Create(AToken: TToken; AValue: TExpression);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property Value: TExpression read FValue;
  end;

  { TBlockStatement }
  TBlockStatement = class(TStatement)
  private
    FStmts: TObjectList<TStatement>;
  public
    constructor Create(AToken: TToken; AStmts: TObjectList<TStatement>);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property Stmts: TObjectList<TStatement> read FStmts;
  end;

  { TIfStatement }
  TIfStatement = class(TStatement)
  private
    FCondition: TExpression;
    FConsequence: TBlockStatement;
    FAlternative: TBlockStatement;
  public
    constructor Create(AToken: TToken; ACondition: TExpression; AConsequence: TBlockStatement; AAlternative: TBlockStatement);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property Condition: TExpression read FCondition;
    property Consequence: TBlockStatement read FConsequence;
    property Alternative: TBlockStatement read FAlternative;
  end;

  { TFunctionStatement }
  TFunctionStatement = class(TStatement)
  private
    FName: TIdentifier;
    FParameters: TObjectList<TIdentifier>;
    FBody: TBlockStatement;
  public
    constructor Create(AToken: TToken; AName: TIdentifier; AParameters: TObjectList<TIdentifier>; ABody: TBlockStatement);
    destructor Destroy; override;
    procedure Accept(v: TVisitor); override;
    property Name: TIdentifier read FName;
    property Parameters: TObjectList<TIdentifier> read FParameters;
    property Body: TBlockStatement read FBody;
  end;

  { TVisitor }
  TVisitor = class(TObject)
  public
    function VisitLiteralExpr(expr: TLiteralExpression): Variant; virtual; abstract;
    function VisitIdentifierExpr(expr: TIdentifier): Variant; virtual; abstract;
    function VisitLogicalExpr(expr: TLogicalExpression): Variant; virtual; abstract;
    function VisitBinaryExpr(expr: TBinaryExpression): Variant; virtual; abstract;
    function VisitAssignmentExpr(expr: TAssignmentExpression): Variant; virtual; abstract;
    function VisitCallExpr(expr: TCallExpression): Variant; virtual; abstract;

    { STATEMENTS }
    procedure VisitExpressionStmt(stmt: TExpressionStatement); virtual; abstract;
    procedure VisitPrintStmt(stmt: TPrintStatement); virtual; abstract;
    procedure VisitVariableStmt(stmt: TVariableStatement); virtual; abstract;
    procedure VisitConstantStmt(stmt: TConstantStatement); virtual; abstract;
    procedure VisitReturnStmt(stmt: TReturnStatement); virtual; abstract;
    procedure VisitBlockStmt(stmt: TBlockStatement); virtual; abstract;
    procedure VisitIfStmt(stmt: TIfStatement); virtual; abstract;
    procedure VisitFunctionStmt(stmt: TFunctionStatement); virtual; abstract;
  end;

implementation
  (************************************************************************
  * TNode
  ************************************************************************)
  constructor TNode.Create(AToken: TToken);
  begin
    FToken := AToken;
  end;
  (************************************************************************
  * TLiteralExpression
  ************************************************************************)
  constructor TLiteralExpression.Create(AToken: TToken; AValue: Variant);
  begin
    inherited Create(AToken);
    FValue := AValue;
  end;

  function TLiteralExpression.Accept(v: TVisitor): Variant;
  begin
    Result := v.VisitLiteralExpr(Self);
  end;
  (************************************************************************
  * TLogicalExpression
  ************************************************************************)
  constructor TLogicalExpression.Create(AToken: TToken; ALeft: TExpression; AOpe: TKind; ARight: TExpression);
  begin
    inherited Create(AToken);
    FLeft := ALeft;
    FOpe := AOpe;
    FRight := ARight;
  end;

  destructor TLogicalExpression.Destroy;
  begin
    if Assigned(FLeft) then FLeft.Free;
    if Assigned(FRight) then FRight.Free;
    inherited Destroy;
  end;

  function TLogicalExpression.Accept(v: TVisitor): Variant;
  begin
    Result := v.VisitLogicalExpr(Self);
  end;
  (************************************************************************
  * TBinaryExpression
  ************************************************************************)
  constructor TBinaryExpression.Create(AToken: TToken; ALeft: TExpression; AOpe: TKind; ARight: TExpression);
  begin
    inherited Create(AToken);
    FLeft := ALeft;
    FOpe := AOpe;
    FRight := ARight;
  end;

  destructor TBinaryExpression.Destroy;
  begin
    if Assigned(FLeft) then FLeft.Free;
    if Assigned(FRight) then FRight.Free;
    inherited Destroy;
  end;

  function TBinaryExpression.Accept(v: TVisitor): Variant;
  begin
    Result := v.VisitBinaryExpr(Self);
  end;
  (************************************************************************
  * TIdentifier
  ************************************************************************)
  constructor TIdentifier.Create(AToken: TToken; AName: string);
  begin
    inherited Create(AToken);
    FName := AName;
  end;

  destructor TIdentifier.Destroy;
  begin
    inherited Destroy;
  end;

  function TIdentifier.Accept(v: TVisitor): Variant;
  begin
    Result := v.VisitIdentifierExpr(Self);
  end;

  (************************************************************************
  * TAssignmentExpression
  ************************************************************************)
  constructor TAssignmentExpression.Create(AToken: TToken; AOpe: TKind; ALeft, AValue: TExpression);
  begin
    inherited Create(AToken);
    FOpe := AOpe;
    FLeft := ALeft;
    FValue := AValue;
  end;

  destructor TAssignmentExpression.Destroy;
  begin
    if Assigned(FLeft) then FLeft.Free;
    if Assigned(FValue) then FValue.Free;
  end;

  function TAssignmentExpression.Accept(v: TVisitor): Variant;
  begin
    Result := v.VisitAssignmentExpr(Self);
  end;
  (************************************************************************
  * TCallExpression
  ************************************************************************)
  constructor TCallExpression.Create(AToken: TToken; ACallee: TExpression; AArguments: TObjectList<TExpression>);
  begin
    inherited Create(AToken);
    FCallee := ACallee;
    FArguments := AArguments;
  end;

  destructor TCallExpression.Destroy;
  begin
    if Assigned(FCallee) then FCallee.Free;
    if Assigned(FArguments) then FArguments.Free;
  end;

  function TCallExpression.Accept(v: TVisitor): Variant;
  begin
    Result := v.VisitCallExpr(Self);
  end;

  (************************************************************************
  * TExpressoinStatement
  ************************************************************************)
  constructor TExpressionStatement.Create(AToken: TToken; AExpr: TExpression);
  begin
    inherited Create(AToken);
    FExpr := AExpr;
  end;

  destructor TExpressionStatement.Destroy;
  begin
    FreeAndNil(FExpr);
    inherited Destroy;
  end;

  procedure TExpressionStatement.Accept(v: TVisitor);
  begin
    v.VisitExpressionStmt(self);
  end;
  (************************************************************************
  * TPrintStatement
  ************************************************************************)
  constructor TPrintStatement.Create(AToken: TToken; AValues: TExpressionList);
  begin
    inherited Create(AToken);
    FValues := AValues;
  end;

  destructor TPrintStatement.Destroy;
  begin
    if Assigned(FValues) then
    begin
      FValues.Free;
    end;
  end;

  procedure TPrintStatement.Accept(v: TVisitor);
  begin
    v.VisitPrintStmt(Self);
  end;
  (************************************************************************
  * TVariableStruct
  ************************************************************************)
  constructor TVariableStruct.Create(AName: TIdentifier; AType: TVariableType; AValue: TExpression);
  begin
    FName := AName;
    FType := AType;
    FValue := AValue;
  end;

  destructor TVariableStruct.Destroy;
  begin
    if Assigned(FName) then FName.Free;
    if Assigned(FValue) then FValue.Free;
  end;
  (************************************************************************
  * TVariableStatement
  ************************************************************************)
  constructor TVariableStatement.Create(AToken: TToken; AScope: TVariableScope; AVarList: TObjectList<TVariableStruct>);
  begin
    inherited Create(AToken);
    FScope := AScope;
    FVarList := AVarList;
  end;

  destructor TVariableStatement.Destroy;
  begin
    if Assigned(FVarList) then FVarList.Free;
  end;

  procedure TVariableStatement.Accept(v: TVisitor);
  begin
    v.VisitVariableStmt(Self);
  end;

  (************************************************************************
  * TConstantStatement
  ************************************************************************)
  constructor TConstantStatement.Create(AConstantList: TObjectList<TAssignmentExpression>);
  begin
    FConstantList := AConstantList;
  end;

  destructor TConstantStatement.Destroy;
  begin
    if Assigned(FConstantList) then FConstantList.Free;
    inherited Destroy;
  end;

  procedure TConstantStatement.Accept(v: TVisitor);
  begin
    v.VisitConstantStmt(Self);
  end;
  (************************************************************************
  * TReturnStatement
  ************************************************************************)
  constructor TReturnStatement.Create(AToken: TToken; AValue: TExpression);
  begin
    inherited Create(AToken);
    FValue := AValue;
  end;

  destructor TReturnStatement.Destroy;
  begin
    if Assigned(FValue) then FValue.Free;
  end;

  procedure TReturnStatement.Accept(v: TVisitor);
  begin
    v.VisitReturnStmt(Self);
  end;
  (************************************************************************
  * TBlockStatement
  ************************************************************************)
  constructor TBlockStatement.Create(AToken: TToken; AStmts: TObjectList<TStatement>);
  begin
    inherited Create(AToken);
    FStmts := AStmts;
  end;

  destructor TBlockStatement.Destroy;
  begin
    if Assigned(FStmts) then FStmts.Free;
    inherited Destroy;
  end;

  procedure TBlockStatement.Accept(v: TVisitor);
  begin
    v.VisitBlockStmt(Self);
  end;
  (************************************************************************
  * TIfStatement
  ************************************************************************)
  constructor TIfStatement.Create(AToken: TToken; ACondition: TExpression; AConsequence: TBlockStatement; AAlternative: TBlockStatement);
  begin
    inherited Create(AToken);
    FCondition := ACondition;
    FConsequence := AConsequence;
    FAlternative := AAlternative;
  end;

  destructor TIfStatement.Destroy;
  begin
    if Assigned(FCondition) then FreeAndNil(FCondition);
    if Assigned(FConsequence) then FreeAndNil(FConsequence);
    if Assigned(FAlternative) then FreeAndNil(FAlternative);
    inherited Destroy;
  end;

  procedure TIfStatement.Accept(v: TVisitor);
  begin
    v.VisitIfStmt(Self);
  end;
  (************************************************************************
  * TFunctionStatement
  ************************************************************************)
  constructor TFunctionStatement.Create(AToken: TToken; AName: TIdentifier; AParameters: TObjectList<TIdentifier>; ABody: TBlockStatement);
  begin
    inherited Create(AToken);
    FName := AName;
    FParameters := AParameters;
    FBody := ABody;
  end;

  destructor TFunctionStatement.Destroy;
  begin
    if Assigned(FName) then FName.Free;
    if Assigned(FParameters) then FParameters.Free;
    if Assigned(FBody) then FBody.Free;
    inherited Destroy;
  end;

  procedure TFunctionStatement.Accept(v: TVisitor);
  begin
    v.VisitFunctionStmt(Self);
  end;

end.

