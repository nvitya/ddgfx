unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ddgfx, freetypeh;


type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnExit : TButton;
    oglpanel : TPanel;
    pnl : TPanel;
    drawtimer : TTimer;
    edX : TEdit;
    edY : TEdit;
    btnSet : TButton;
    procedure btnExitClick(Sender : TObject);
    procedure drawtimerTimer(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure btnSetClick(Sender : TObject);
  private

  public
    scene : TddScene;

    txt : TTextBox;

    t : single; // time

    origw, origh : integer;

    //-----------------------------

    fontface : PFT_Face;
    ft_library_var : PFT_Library;
    glyphcode : integer;
    glyphslot : PFT_GlyphSlot;
    glyph : PFT_Glyph;
    bmglyph : PFT_BitmapGlyph;

    procedure RenderChar;


    procedure OglboxResize(Sender: TObject);

  end;

var
  frmMain : TfrmMain;

implementation

uses dglOpenGL, freetypex, ddgfx_font;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender : TObject);
begin

  origw := oglpanel.Width;
  origh := oglpanel.Height;

  scene := TddScene.Create(self, oglpanel);

  scene.bgcolor.r := 0.4;
  scene.bgcolor.g := 0.4;
  scene.bgcolor.b := 0.2;

  txt := TTextBox.Create(scene.root, 'Hello World! Agy');
  txt.x := 10;
  txt.y := 10;
  txt.scalex := 1; //8 * 2;
  txt.scaley := 1; //8 * 2;

  //RenderChar;

  scene.OnResize := @OglboxResize;

  //drawtimer.Enabled := true;
  t := 0;
end;

procedure TfrmMain.btnExitClick(Sender : TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.drawtimerTimer(Sender : TObject);
begin

  // modify the shapes
  t += 1;

  txt.x := 10 + 2 * sin(t / 50);

{
  txt.alpha := 1 + 0.9 * sin(t / 50);
  txt.scalex := 1 + 0.5 * sin(t / 10);
  txt.scaley := txt.scalex;
  txt.rotation := 20 + 40 * sin(t / 70);

  // for proper rescaling effect the x,y should remain zero
  scene.root.x := 100 + 50*sin(t / 20);
  scene.root.y :=  50 + 50*cos(t / 20);

  scene.root.scalex := oglpanel.Width / origw;
  scene.root.scaley := oglpanel.Height / origh;
}

  scene.Repaint;
end;

procedure TfrmMain.FormDestroy(Sender : TObject);
begin
end;

procedure TfrmMain.btnSetClick(Sender : TObject);
begin
  txt.x := StrToFloatDef(edX.Text, 10);
  txt.y := StrToFloatDef(edY.Text, 10);

  scene.Repaint;
end;

{$if 0}

procedure TfrmMain.RenderChar;
var
  w, h : integer;
  pd : pbyte;
  pt : pbyte;
begin
  FT_Init_FreeType(ft_library_var);

  if FT_New_Face(ft_library_var, './liberationserif.ttf', 0, fontface) <> 0 then
  begin
    raise Exception.Create('Error opening TTF font.');
  end;

  if FT_Set_Char_Size(fontface, 0, 9 shl 6, 96, 96) <> 0 then
    raise Exception.Create('FT_Set_Char_Size failed');

  glyphcode := ord('g');

  if FT_Load_Glyph(fontface, FT_Get_Char_Index(fontface, glyphcode ), FT_LOAD_DEFAULT) <> 0 then
    raise Exception.Create('Load glyph failed!');

  if FT_Get_Glyph(fontface^.glyph, glyph) <> 0 then raise Exception.Create('get glyph failed.');

  if FT_Glyph_To_Bitmap(glyph, FT_RENDER_MODE_NORMAL, nil, true) <> 0 then raise Exception.Create('error rendering bitmap.');

  bmglyph := PFT_BitmapGlyph(glyph);

{
  writeln('Bitmap glyph data:');
  writeln('  top: ',bmglyph.top);
  writeln('  left: ',bmglyph.left);
  writeln('  width: ',bmglyph.bitmap.width);
  writeln('  rows: ',bmglyph.bitmap.rows);
  writeln('  pitch: ',bmglyph.bitmap.pitch);
  writeln('  num_grays: ',bmglyph.bitmap.num_grays);
  writeln('  pixel_mode: ',ord(bmglyph.bitmap.pixel_mode));
}

  pd := bmglyph^.bitmap.buffer;
  for h := 1 to bmglyph^.bitmap.rows do
  begin
    pt := txt.data;
    Inc(pt, txt.width * (h + 10));
    Inc(pt, 10);
    for w := 1 to bmglyph^.bitmap.width do
    begin
      //write(IntToHex(pd^, 2));
      pt^ := pd^;
      Inc(pt);

      inc(pd);
    end;
  end;

  FT_Done_Glyph(glyph);
end;

{$elseif 0}

var
  ftmgr : freetypex.TFontManager;

procedure TfrmMain.RenderChar;
var
  s : string;
  fid : integer;
  sbm : TStringBitmaps;
  pfb : PFontBitmap;

  pt : pbyte;
  tx, ty : integer;

  w, h : integer;
  pd : pbyte;

  i : integer;
begin
  ftmgr := freetypex.TFontManager.Create;
{
  writeln('searching font...');
  s := ftmgr.SearchFont('liberationserif', false);
  writeln(' = ', s);
}
  //fid := ftmgr.RequestFont('liberationserif');
  fid := ftmgr.RequestFont('LiberationSans-Regular.ttf');

  s := txt.Text;

  sbm := ftmgr.GetStringGray(fid, s, 12);

  tx := 10;
  ty := 20;

  for i := 0 to sbm.Count-1 do
  begin
    pfb := sbm.Bitmaps[i];

    pd := @pfb^.data[0];
    for h := 1 to pfb^.height do
    begin
      pt := txt.data;
      Inc(pt, txt.width * (h + ty + pfb^.y));
      Inc(pt, tx + pfb^.x);
      for w := 1 to pfb^.width do
      begin
        //write(IntToHex(pd^, 2));
        if pd^ <> 0 then pt^ := pd^;
        Inc(pt);

        inc(pd);
      end;
    end;
  end;

end;

{$else}

procedure TfrmMain.RenderChar;
var
  pt : pbyte;
  pd : pbyte;
  tx, ty : integer;
  w, h : integer;

  face : TFontFace;
  sface : TSizedFont;

  gbmp : TGlyphBitmap;

  s : string;
  i : integer;

begin

  exit;

  InitFontManager;

  //face := fontmanager.GetFont('liberationserif.ttf');
  face := fontmanager.GetFont('LiberationSans-Regular.ttf');

  writeln('font found.');
  sface := face.GetSizedFont(16);

  s := '1234567890';
  //s := txt.ftext;

  tx := 0;

  ty := 15;

  for i := 1 to length(s) do
  begin
    gbmp := sface.GetGlyphBmp(s[i]);

    pd := gbmp.data;
    for h := 1 to gbmp.height do
    begin
      pt := txt.data;
      Inc(pt, txt.width * (h + ty - gbmp.y));
      Inc(pt, 5 + tx + gbmp.x);
      for w := 1 to gbmp.width do
      begin
        pt^ := pd^;
        Inc(pt);
        inc(pd);
      end;
    end;

    tx += gbmp.advanceX;

  end;

end;

{$endif}

procedure TfrmMain.OglboxResize(Sender : TObject);
begin

end;


end.

