unit uToken;

{$mode delphi}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Variants, Generics.Collections;

type
  TKind = (
    tkEof,

    // Symbols
    tkLeftParen,       { '(' }
    tkRightParen,      { ')' }
    tkLeftBrace,       (* '{' *)
    tkRightBrace,      (* '}' *)
    tkLeftBracket,     { '[' }
    tkRightBracket,    { ']' }
    tkDot,             { '.' }
    tkComma,           { ',' }
    tkSemicolon,       { ';' }
    tkColon,           { ':' }
    tkAsk,             { '?' }

    // Literals
    tkNumber,
    tkString,
    tkIdentifier,

    // Arithmetic Operators
    tkPlus,            { '+' }
    tkMinus,           { '-' }
    tkMult,            { '*' }
    tkDiv,             { '/' }

    // Relational Operators
    tkLess,            { '<' }
    tkLessEq,          { '<=' }
    tkGreater,         { '>' }
    tkGreaterEq,       { '>=' }
    tkAssign,          { '=' }
    tkPlusAssign,      { '+=' }
    tkMinusAssign,     { '-=' }
    tkMultAssign,      { '*=' }
    tkDivAssign,       { '/=' }
    tkEqual,           { '==' }
    tkNotEqual,        { '!=' }

    // Keywords
    tkAnd,
    tkAppend,
    tkAs,
    tkBottom,
    tkBrowse,
    tkCase,
    tkCentury,
    tkClass,
    tkConnection,
    tkCount,
    tkConst,
    tkCreate,
    tkDatabase,
    tkDo,
    tkEach,
    tkElif,
    tkElse,
    tkEndcase,
    tkEnddo,
    tkEndfor,
    tkEndfunc,
    tkEndif,
    tkEndproc,
    tkEndscan,
    tkExit,
    tkFalse,
    tkFor,
    tkFunction,
    tkGather,
    tkIf,
    tkJoin,
    tkLocal,
    tkLocate,
    tkMkdir,
    tkNull,
    tkNot,
    tkOf,
    tkOr,
    tkProcedure,
    tkPublic,
    tkPrint,
    tkReadonly,
    tkRelease,
    tkRepeat,
    tkReplace,
    tkReturn,
    tkSafety,
    tkScan,
    tkSet,
    tkSkip,
    tkStep,
    tkStore,
    tkThen,
    tkThis,
    tkTo,
    tkTrue,
    tkUntil,
    tkUse,
    tkWait,
    tkWhen,
    tkWhile,
    tkWindow,
    tkWith,
    tkNewLine
  );
  // Helper para convertir de TTokenKind a String
  TTokenKindHelper = type helper for TKind
    function toString: string;
  end;

  { TToken }
  TToken = class
    private
      FKind: TKind;
      FLiteral: Variant;
      FLine, FCol: Integer;
    public
      property Kind: TKind read FKind;
      property Literal: Variant read FLiteral;
      property Line: Integer read FLine;
      property Col: Integer read FCol;
      constructor Create(AKind: TKind; ALiteral: Variant; ALine, ACol: Integer);
      function toString: String; override;
  end;
var
  keywords: TDictionary<string, TKind>;

implementation

  { TTooken }
  constructor TToken.Create(AKind: TKind; ALiteral: Variant; ALine, ACol: Integer);
  begin
    inherited Create;
    FKind := AKind;
    FLiteral := ALiteral;
    FLine := ALine;
    FCol := ACol;
  end;

  function TToken.toString: string;
  begin
    Result := Format('Token[%d:%d](%s, ''%s'')', [FCol, FLine, FKind.toString, VarToStr(FLiteral)]);
  end;

  { TTokenKindHelper }
  function TTokenKindHelper.toString: string;
  begin
    case Self of
    tkEof: Result := 'EOF';

    // Symbols
    tkLeftParen:      Result := 'LPAREN';
    tkRightParen:     Result := 'RPAREN';
    tkLeftBrace:      Result := 'LBRACE';
    tkRightBrace:     Result := 'RBRACE';
    tkLeftBracket:    Result := 'LBRACKET';
    tkRightBracket:   Result := 'RBRACKET';
    tkDot:            Result := 'DOT';
    tkComma:          Result := 'COMMA';
    tkSemicolon:      Result := 'SEMICOLON';
    tkColon:          Result := 'COLON';
    tkAsk:            Result := '?';

    // Literals
    tkNumber:         Result := 'NUMBER';
    tkString:         Result := 'STRING';
    tkIdentifier:     Result := 'IDENT';

    // Arithmetic Operators
    tkPlus:           Result := 'PLUS';
    tkMinus:          Result := 'MINUS';
    tkMult:           Result := 'ASTERISK';
    tkDiv:            Result := 'SLASH';

    // Relational Operators
    tkLess:           Result := 'LESS';
    tkLessEq:         Result := 'LESS_EQ';
    tkGreater:        Result := 'GREATER';
    tkGreaterEq:      Result := 'GREATER_EQ';
    tkAssign:         Result := 'EQUAL';
    tkPlusAssign:     Result := 'PLUS_EQ';
    tkMinusAssign:    Result := 'MINUS_EQ';
    tkMultAssign:     Result := 'MULT_EQ';
    tkDivAssign:      Result := 'SLASH_EQ';
    tkEqual:          Result := '==';
    tkNotEqual:       Result := '!=';

    // Keywords
    tkAnd:            Result := 'and';
    tkAppend:         Result := 'append';
    tkAs:             Result := 'as';
    tkBottom:         Result := 'bottom';
    tkBrowse:         Result := 'browse';
    tkCase:           Result := 'case';
    tkCentury:        Result := 'century';
    tkClass:          Result := 'class';
    tkConnection:     Result := 'connection';
    tkCount:          Result := 'count';
    tkConst:          Result := 'const';
    tkCreate:         Result := 'create';
    tkDatabase:       Result := 'database';
    tkDo:             Result := 'do';
    tkEach:           Result := 'each';
    tkElif:           Result := 'elif';
    tkElse:           Result := 'else';
    tkEndcase:        Result := 'endcase';
    tkEnddo:          Result := 'enddo';
    tkEndfor:         Result := 'endfor';
    tkEndfunc:        Result := 'endfunc';
    tkEndif:          Result := 'endif';
    tkEndproc:        Result := 'endproc';
    tkEndscan:        Result := 'endscan';
    tkExit:           Result := 'exit';
    tkFalse:          Result := 'false';
    tkFor:            Result := 'for';
    tkFunction:       Result := 'function';
    tkGather:         Result := 'gather';
    tkIf:             Result := 'if';
    tkJoin:           Result := 'join';
    tkLocal:          Result := 'local';
    tkLocate:         Result := 'locate';
    tkMkdir:          Result := 'mkdir';
    tkNull:           Result := 'null';
    tkNot:            Result := 'not';
    tkOf:             Result := 'of';
    tkOr:             Result := 'or';
    tkProcedure:      Result := 'procedure';
    tkPublic:         Result := 'public';
    tkPrint:          Result := 'print';
    tkReadonly:       Result := 'readonly';
    tkRelease:        Result := 'release';
    tkRepeat:         Result := 'repeat';
    tkReplace:        Result := 'replace';
    tkReturn:         Result := 'return';
    tkSafety:         Result := 'safety';
    tkScan:           Result := 'scan';
    tkSet:            Result := 'set';
    tkSkip:           Result := 'skip';
    tkStep:           Result := 'step';
    tkStore:          Result := 'store';
    tkThen:           Result := 'then';
    tkThis:           Result := 'this';
    tkTo:             Result := 'to';
    tkTrue:           Result := 'true';
    tkUntil:          Result := 'until';
    tkUse:            Result := 'use';
    tkWait:           Result := 'wait';
    tkWhen:           Result := 'when';
    tkWhile:          Result := 'while';
    tkWindow:         Result := 'window';
    tkWith:           Result := 'with';
    tkNewLine:        Result := 'newline';
    else
      result := 'ILLEGAL';
    end;
  end;

  initialization
    keywords := TDictionary<string, TKind>.Create;
    keywords.Add('and', tkAnd);
    keywords.Add('append', tkAppend);
    keywords.Add('as', tkAs);
    keywords.Add('bottom', tkBottom);
    keywords.Add('browse', tkBrowse);
    keywords.Add('case', tkCase);
    keywords.Add('century', tkCentury);
    keywords.Add('class', tkClass);
    keywords.Add('connection', tkConnection);
    keywords.Add('count', tkCount);
    keywords.Add('const', tkConst);
    keywords.Add('create', tkCreate);
    keywords.Add('database', tkDatabase);
    keywords.Add('do', tkDo);
    keywords.Add('each', tkEach);
    keywords.Add('elif', tkElif);
    keywords.Add('else', tkElse);
    keywords.Add('endcase', tkEndcase);
    keywords.Add('enddo', tkEnddo);
    keywords.Add('endfor', tkEndfor);
    keywords.Add('endfunc', tkEndfunc);
    keywords.Add('endif', tkEndif);
    keywords.Add('endproc', tkEndproc);
    keywords.Add('endscan', tkEndscan);
    keywords.Add('exit', tkExit);
    keywords.Add('false', tkFalse);
    keywords.Add('for', tkFor);
    keywords.Add('function', tkFunction);
    keywords.Add('gather', tkGather);
    keywords.Add('if', tkIf);
    keywords.Add('join', tkJoin);
    keywords.Add('local', tkLocal);
    keywords.Add('locate', tkLocate);
    keywords.Add('mkdir', tkMkdir);
    keywords.Add('null', tkNull);
    keywords.Add('of', tkOf);
    keywords.Add('or', tkOr);
    keywords.Add('procedure', tkProcedure);
    keywords.Add('public', tkPublic);
    keywords.Add('print', tkPrint);
    keywords.Add('readonly', tkReadonly);
    keywords.Add('release', tkRelease);
    keywords.Add('repeat', tkRepeat);
    keywords.Add('replace', tkReplace);
    keywords.Add('return', tkReturn);
    keywords.Add('safety', tkSafety);
    keywords.Add('scan', tkScan);
    keywords.Add('set', tkSet);
    keywords.Add('skip', tkSkip);
    keywords.Add('step', tkStep);
    keywords.Add('store', tkStore);
    keywords.Add('then', tkThen);
    keywords.Add('this', tkThis);
    keywords.Add('to', tkTo);
    keywords.Add('true', tkTrue);
    keywords.Add('until', tkUntil);
    keywords.Add('use', tkUse);
    keywords.Add('wait', tkWait);
    keywords.Add('when', tkWhen);
    keywords.Add('while', tkWhile);
    keywords.Add('window', tkWindow);
    keywords.Add('with', tkWith);
  finalization
    FreeAndNil(keywords);
end.

