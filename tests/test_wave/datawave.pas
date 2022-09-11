unit datawave;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, ddgfx;

type
  { TDataWave }

  TDataWave = class
  public
    data : array of double;

    shp  : TShape;

    constructor Create(aparent : TDrawGroup);
    destructor Destroy; override;

    procedure GenerateRandomData(acount : integer);

    procedure UpdateVertexData;
  end;


implementation

{ TDataWave }

constructor TDataWave.Create(aparent : TDrawGroup);
begin
  SetLength(data, 0);
  shp := TShape.Create(aparent);
end;

destructor TDataWave.Destroy;
begin
  //shp.Free;
  SetLength(data, 0);

  inherited Destroy;
end;

procedure TDataWave.GenerateRandomData(acount : integer);
var
  i : integer;
begin
  SetLength(data, acount);
  for i := 0 to acount - 1 do
  begin
    data[i] := Random(1000000) / 1000000;
  end;

  UpdateVertexData;
end;

procedure TDataWave.UpdateVertexData;
var
  i : integer;
  varr : array of TVertex;
  v : TVertex;
  vcnt : integer;
begin
  vcnt := length(data);
  varr := nil;
  SetLength(varr, vcnt);
  for i := 0 to vcnt - 1 do
  begin
    v[0] := i;
    v[1] := 50 * data[i];
    varr[i] := v;
  end;

  shp.Clear; // removes all primitives
  shp.AddPrimitive(GL_LINE_STRIP, vcnt, @varr[0]);

  SetLength(varr, 0);
end;

initialization
begin
  Randomize;
end;

end.

