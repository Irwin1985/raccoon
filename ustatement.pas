unit uStatement;

{$mode delphi}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, Generics.Collections, uExpression, uToken;

type
  IStatementVisitor = interface;

  TStatement = class
    abstract
  public
    procedure Accept(ev: IStatementVisitor); virtual; abstract;
  end;

  { TBlockStatement }
  TBlockStatement = class(TStatement)
  private
    FBlock: TObjectList<TStatement>;
    // FEnvironment: TEnvironment;
  public
    constructor Create(block: TObjectList<TStatement>);
    destructor Destroy; override;
    procedure Accept(ev: IStatementVisitor); override;
    property block: TObjectList<TStatement> read FBlock;
    // property environment: TEnvironment read FEnvironment;
  end;

  { TExpressionStatement }
  TExpressionStatement = class(TStatement)
  private
    FExpr: TExpression;
  public
    constructor Create(expr: TExpression);
    destructor Destroy; override;
    procedure Accept(ev: IStatementVisitor); override;
    property expr: TExpression read FExpr;
  end;

  { VariableStatement }
  TVariableStatement = class(TStatement)
  private
    FToken: TToken;
    FExpr: TExpression;
  public
    constructor Create(token: TToken; expr: TExpression);
    destructor Destroy; override;
    procedure Accept(ev: IStatementVisitor); override;
    property token: TToken read FToken;
    property expr: TExpression read FExpr;
  end;

  { IStatementVisitor }
  IStatementVisitor = interface
    ['{AA8A441E-392A-49E0-9017-835824323483}']
    procedure VisitBlockStmt(stmt: TBlockStatement);
    procedure VisitExpressionStmt(stmt: TExpressionStatement);
    procedure VisitVariableStmt(stmt: TVariableStatement);
  end;

implementation
  { BlockStatement }
  constructor TBlockStatement.Create(block: TObjectList<TStatement>);
  begin
    FBlock := block;
    // FEnvironment := environment;
  end;

  destructor TBlockStatement.Destroy;
  begin
    FreeAndNil(FBlock);
    // FreeAndNil(FEnvironment);
    inherited Destroy;
  end;

  procedure TBlockStatement.Accept(ev: IStatementVisitor);
  begin
    ev.VisitBlockStmt(self);
  end;

  { TExpressionStatement }
  constructor TExpressionStatement.Create(expr: TExpression);
  begin
    FExpr := expr;
  end;

  destructor TExpressionStatement.Destroy;
  begin
    FreeAndNil(FExpr);
    inherited Destroy;
  end;

  procedure TExpressionStatement.Accept(ev: IStatementVisitor);
  begin
    ev.VisitExpressionStmt(self);
  end;

  { TVariableStatement }
  constructor TVariableStatement.Create(token: TToken; expr: TExpression);
  begin
    FToken := token;
    FExpr := expr;
  end;

  destructor TVariableStatement.Destroy;
  begin
    FreeAndNil(FExpr);
    inherited Destroy;
  end;

  procedure TVariableStatement.Accept(ev: IStatementVisitor);
  begin
    ev.VisitVariableStmt(Self);
  end;

end.

