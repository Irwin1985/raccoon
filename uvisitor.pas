unit uVisitor;

{$mode Delphi}

interface

uses
  Classes, SysUtils, uAst;

type
  TVisitor = class(TObject)
    published
      function VisitLiteralExpr(expr: TLiteralExpression): TObject; virtual;
      procedure VisitBlockStmt(stmt: TBlockStatement); virtual;
      procedure VisitExpressionStmt(stmt: TExpressionStatement); virtual;
      procedure VisitVariableStmt(stmt: TVariableStatement); virtual;
  end;

implementation

end.

