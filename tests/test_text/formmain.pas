// coding: UTF-8

unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ddgfx, ddgfx_font;


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

    txt, txt2 : TTextBox;
    am  : TAlphaMap;

    t : single; // time

    font1 : TFontFace;
    font2 : TFontFace;

    origw, origh : integer;

    //-----------------------------

    procedure MakeAlphaMap;

    procedure OglboxResize(Sender: TObject);

  end;

var
  frmMain : TfrmMain;

implementation

uses dglOpenGL, freetypex;

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

  InitFontManager;
  font1 := fontmanager.GetFont('LiberationSans-Regular.ttf');
  font2 := fontmanager.GetFont('liberationserif.ttf');

  txt := TTextBox.Create(scene.root, font1.GetSizedFont(9), 'Hello World! éáőúöüóÉÁŐÚŰÖÜÓ Agy');
  txt.x := 10;
  txt.y := 10;
  txt.scalex := 1; //8 * 2;
  txt.scaley := 1; //8 * 2;

  writeln('txt width: ', txt.Width);

  txt2 := TTextBox.Create(scene.root, font2.GetSizedFont(20), 'Text 2');
  txt2.x := 100;
  txt2.y := 100;

  am := TAlphaMap.Create(scene.root, 403, 303);
  am.x := 32;
  am.y := 32;
  MakeAlphaMap;
  am.visible := False;


  scene.OnResize := @OglboxResize;

  t := 0;
  drawtimer.Enabled := true;
end;

procedure TfrmMain.btnExitClick(Sender : TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.drawtimerTimer(Sender : TObject);
begin

  // modify the shapes
  t += 1;

  //txt.x := 10 + 2 * sin(t / 50);

  txt2.Text := 'txt2 value = '+FloatToStr(t);
  txt.Text := 'Hello World! éáőúöüóÉÁŐÚŰÖÜÓ Agy' + IntToStr(trunc(t) mod 20);

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

procedure TfrmMain.MakeAlphaMap;
var
  i, maxi : integer;
  bp : PByte;
begin
  i := 0;
  am.Clear(0);
  maxi := am.BmpWidth * am.BmpHeight;
  while i < maxi do
  begin
    bp := am.data + i;
    bp^ := 255;

    inc(i, 77);
  end;
end;

procedure TfrmMain.OglboxResize(Sender : TObject);
begin

end;


end.

