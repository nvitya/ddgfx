unit ddgfx_font;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, {$IFDEF DYNAMIC}freetypehdyn{$ELSE}freetypeh{$ENDIF};

type

  { TFontFace }

  TFontFace = class
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TFaceObjectList = specialize TFPGObjectList<TFontFace>;

  { TFontManager }

  TFontManager = class
  private
    FTLib : PFT_Library;
    FFaceList : TFaceObjectList;

  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  fontmanager : TFontManager;

procedure InitFontManager;
procedure DoneFontManager;

implementation

procedure InitFontManager;
begin
  if fontmanager = nil then
  begin
    fontmanager := TFontManager.Create;
  end;
end;

procedure DoneFontManager;
begin
  if fontmanager <> nil then FreeAndNil(fontmanager);
end;

{ TFontFace }

constructor TFontFace.Create;
begin

end;

destructor TFontFace.Destroy;
begin
  inherited Destroy;
end;

{ TFontManager }

constructor TFontManager.Create;
var
  r : integer;
begin
  FFaceList := TFaceObjectList.Create(True);
  {$IFDEF DYNAMIC}
  if Pointer(FT_Init_FreeType) = nil then InitializeFreetype();
  {$ENDIF}
  r := FT_Init_FreeType(FTLib);

end;

destructor TFontManager.Destroy;
begin
  FFaceList.Free;
  inherited Destroy;
end;

initialization
begin
  fontmanager := nil;
end;

end.

