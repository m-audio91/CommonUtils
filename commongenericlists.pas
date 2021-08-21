unit CommonGenericLists;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uMinimalList;

type     
  TWordList = specialize TMinimalList<Word>;
  TCardinalList = specialize TMinimalList<Cardinal>;
  TIntegerList = specialize TMinimalList<Integer>;
  TDoubleList = specialize TMinimalList<Double>;

implementation

end.

